{-|
Module      : CSH.Eval.DB.Schema
Description : Schema definition.
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.DB.Schema defines and documents the database schema.
-}

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}

module CSH.Eval.DB.Schema (
     SchemaInit

    -- * Types
   , committee_t
   , status_t
   , member_t
   , dues_t
   , event_t
   , project_t
   , eval_t

     -- * Tables
     -- ** Objects
     -- $objects
   , member
   , event
   , project
   , evaluation
   , conditional
   , freshman_project
   , packet
   , queue
   , application
   , metric
   , review
   , interview
   , question
   , term

     -- ** Contexts
     -- $contexts
     -- *** Span Contexts
     -- $span_contexts
   , eboard
   , room
   , membership

     -- *** Participation Contexts
     -- $participation_contexts
   , event_attendee
   , project_participant
   , freshman_project_participant

     -- *** Other Contexts
   , signature
   , review_metric
   , interview_metric
   , answer
   , dues

     -- ** Logging
   , statement
   , statement_exec

     -- * Foreign Keys
   , enableForeignKeys

     -- * Indices
   , enableIndices
) where

import Control.Monad

import qualified Data.Text as T

import Data.List(zipWith4)

import qualified Hasql as H

import qualified Hasql.Postgres as HP

type SchemaInit = forall s. H.Tx HP.Postgres s ()

-- # Types

-- | This represents the different committees. Committees are mutually
--   exclusive. The possible committees represented are:
--
--   * @evals@     - Evaluations
--   * @rnd@       - Research and Development
--   * @social@    - Social
--   * @history@   - History
--   * @opcomm@    - Operational Communications
--   * @imps@      - House Improvements
--   * @financial@ - Financial
--   * @chariman@  - Chairman
committee_t :: SchemaInit
committee_t = mapM_ H.unitEx
   [ [H.stmt|drop type if exists "committee_t" cascade|]
   , [H.stmt| create type "committee_t" as enum
       ( 'evals'
       , 'rnd'
       , 'social'
       , 'history'
       , 'opcomm'
       , 'imps'
       , 'financial'
       , 'chairman'
       )
     |]
   ]

-- |  The status of votes and evaluations. All votes and evaluations must have a
--    status, and all statuses are mutually exclusive. Possible statuses ar:
--
--    * @pending@ - The vote or evaluation has not yet happened, for instance
--                  a submitted major project that has not yet come up, or a
--                  freshman who has not gone through their 10 week evaluation
--                  yet.
--    * @passed@  - The vote or evaluation has passed.
--    * @failed@  - The vote or evaluation has failed.
status_t :: SchemaInit
status_t = mapM_ H.unitEx
   [ [H.stmt|drop type if exists "status_t" cascade|]
   , [H.stmt|create type "status_t" as enum 
       ( 'pending'
       , 'passed'
       , 'failed'
       )
     |]
   ]

-- | Possible membership states a member can have. All members must have a
--   membership status, and all membership statuses are mutually exclusive.
--   Membership statuses defined here have a one-to-one coorespondence with the
--   membership statuses in the constitution. The possible membership statuses
--   are:
--
--   * @active@       - from Articles Section 3.B Active Membership
--   * @alumni_good@  - from Articles Section 3.C.2 Alumni Membership selection:
--                      "Active members who depart house (i.e. resign) after
--                      passing the current operating session’s Membership
--                      Evaluations are considered to be Alumni in good
--                      standing."
--   * @alumni_bad@   - from Articles Section 3.C.2 Alumni Membership selection:
--                      "Active members who depart house without passing the
--                      current operating session’s Membership Evaluations are
--                      considered to be Alumni in bad standing."
--   * @honorary@     - from Articles Section 3.D Honorary Membership.
--   * @advisory@     - from Articles Section 3.E Advisory Membership. This
--                      should not be used to represent a reslife advisor
--   * @introductory@ - from Articles Section 3.A Introductory Membership
--   * @non@          - Represents a non-member who has information in the
--                      evaluations database. This membership status would be
--                      given to people who go through the evaluations process,
--                      but end up failing an evaluation before they have passed
--                      their first membership evaluation.
member_t :: SchemaInit
member_t = mapM_ H.unitEx
   [ [H.stmt|drop type if exists "member_t" cascade|]
   , [H.stmt|create type "member_t" as enum
       ( 'active'
       , 'alumni_good'
       , 'alumni_bad'
       , 'honorary'
       , 'advisory'
       , 'introductory'
       , 'non'
       )
     |]
   ]

-- | Represents the status of a member's owed dues. The possible values are:
--
--   * @paid@   - The member has paid dues.
--   * @exempt@ - The member is exempted from paying dues.
dues_t :: SchemaInit
dues_t = mapM_ H.unitEx
   [ [H.stmt|drop type if exists "dues_t" cascade|]
   , [H.stmt|create type "dues_t" as enum 
       ( 'paid'
       , 'exempt'
       )
     |]
   ]

-- | Represents the type of an event. The possible values are:
--
--   * @house@       - A House Meeting.
--   * @social@      - A social event.
--   * @committee@   - A committee meeting.
--   * @seminar@     - A technical seminar.
--   * @orientation@ - A CSH Orientation event. This includes events like the
--                     House Systems Seminar, which isn't a technical seminar,
--                     but has attendance we need to keep for clerical reasons.
event_t :: SchemaInit
event_t = mapM_ H.unitEx
   [ [H.stmt|drop type if exists "event_t" cascade|]
   , [H.stmt|create type "event_t" as enum
       ( 'house'
       , 'social'
       , 'committee'
       , 'seminar'
       , 'orientation'
       )
     |]
   ]

-- | Represents the type of a project. Currently, there is only one type of
--   project. This is built in to the schema in preperation for the points
--   system of evaluation, where there is the possibility of different types of
--   projects, such as a minor project or social project. Those types of
--   projects would be added to this enumeration.
--
--   * @major@ - A major technical project.
project_t :: SchemaInit
project_t = mapM_ H.unitEx
   [ [H.stmt|drop type if exists "project_t" cascade|]
   , [H.stmt|create type "project_t" as enum (
        'major'
    )|]
   ]

-- | Represents the type of an evaluation. This is enumerated to allow for
--   changes to the constitution modifying the current evaluations process, or
--   to allow for a future change in scope of the evaluations database.
--
--   * @introductory@ - An introductory evaluation (also refered to as a 10-week
--                      evaluation, or freshman evals).
--   * @membership@   - A membership evaluation.
eval_t :: SchemaInit
eval_t = mapM_ H.unitEx
   [ [H.stmt|drop type if exists "eval_t" cascade|]
   , [H.stmt|create type "eval_t" as enum
       ( 'introductory'
       , 'membership'
       )
     |]
   ]

-- | Each entry in the member table represents a person with information stored
--   in the evaluations database. All people represented in the member table are
--   aloud to log into the evaluations database, although the information they
--   can access is restricted based on various attributes of the member, such
--   as membership status and eboard position. Attributes associated with a
--   member are:
--
--   * @id@             - Unique identifier for the member in the evaluation
--                        database. This is the primary key for entries in the
--                        table.
--   * @uuid@           - LDAP Universially Unique Identifier. Only members with
--                        CSH LDAP entries (e.g. accounts) will have uuids. All
--                        uuids are required to be unique.
--   * @username@       - Username used to log into the evaluations database.
--                        This will be either the CSH LDAP username or a
--                        username chosen when the introductory account is
--                        created to exclusively log into the evaluations
--                        database. All usernames are required to be unique.
--   * @commonname@     - Common Name for the user (e.g. "Stephen Demos").
--                        Corresponds to a cn in LDAP
--   * @password_hash@  - A hash of a password. This is used for members who
--                        have an introductory account exclusively for logging
--                        into the evaluations database. Users with LDAP entries
--                        will log in through webauth will not use this field.
--   * @password_salt@  - Salt for the password. See @password_hash@.
--   * @housing_points@ - The number of Housing Points accumulated by the
--                        member.
--   * @onfloor_status@ - True if the member has onfloor status as described in
--                        the constitution, False otherwise.
member :: SchemaInit
member = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "member" cascade|]
   , [H.stmt|create table "member"
       ( "id"              bigserial  primary key
       , "uuid"            uuid       default null  constraint "unique_member_uuid" unique
       , "username"        varchar    not null  constraint "unique_member_username" unique
       , "commonname"      varchar    not null
       , "password_hash"   bytea      default null
       , "password_salt"   bytea      default null
       , "housing_points"  integer    not null default 0
       , "onfloor_status"  boolean    not null default false
    )|]
   ]

-- | Stores information on the Executive Board positions of every member at any
--   given time. The absence of an entry for a member on a particular date
--   implies they did not hold an Executive Board position at on that date. A
--   member may not hold two Executive Board positions simultaneously. The
--   absence of an @end_date@ attribute implies the member currently holds the
--   specified Executive Board position.
--
--   * @member_id@  - The member that held the Executive Board position for this
--                    span.
--   * @committee@  - The committee that the Executive Board member was running
--                    for this span.
--   * @start_date@ - The date that this Executive Board span started.
--   * @end_date@   - The date that this Executive Board span ended. If this
--                    attribute is null, the member currently holds this
--                    Executive Board position.
eboard :: SchemaInit
eboard = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "eboard" cascade|]
   , [H.stmt|create table "eboard"
       ( "member_id"   bigint       not null
       , "committee"   committee_t  not null
       , "start_date"  date         not null
       , "end_date"    date         default null
       , constraint "no_simultaneous_eboard_positions" unique ("member_id", "start_date")
    )|]
   ]

-- | Tracks the occupancy periods of every room on floor.
--   Rooms occupied in the future represent scheduled room changes. (e.g. when
--   the housing board is completed
--
--   * @member_id@   - The member occupying the room during the given period
--   * @room_number@ - The occupied room (fully qualified (NRH3111 for example)
--   * @start_date@  - The beginning of the occupancy period (date).
--   * @end_date@    - The end of the occupancy period (date).
room :: SchemaInit
room = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "room" cascade|]
   , [H.stmt|create table "room"
       ( "member_id"    bigint   not null
       , "room_number"  varchar  not null
       , "start_date"   date     not null
       , "end_date"     date     not null
       , constraint "no_simultaneous_member_occupation" unique ("member_id", "start_date")
       , constraint "no_simultaneous_room_occupation" unique ("room_number", "start_date")
    )|]
   ]

-- | Tracks membership status over time.
--
--   * @member_id@  - The member status
--   * @status@     - status type
--   * @start_date@ - Date status began
--   * @end_date@   - Date status ended (default is null)
membership :: SchemaInit
membership = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "membership" cascade|]
   , [H.stmt|create table "membership"
       ( "member_id"   bigint    not null
       , "status"      member_t  not null
       , "start_date"  date      not null
       , "end_date"    date      default null
       , constraint "no_simultaneous_membership_status" unique ("member_id", "start_date")
    )|]
   ]

-- | Logs an event
--
--   * @title@       - varchar; The name of the event.
--   * @held@        - timestamp; The time the event began.
--   * @category@    - @event_t@; The kind of event. See @event_t@ for an
--                     enumeration of possible kinds.
--   * @committee@   - @committee_t@; The committee affiliated with the event.
--   * @description@ - varchar; A description of the event. This may be used to
--                     generate a webnews post with the notes
event :: SchemaInit
event = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "event" cascade|]
   , [H.stmt|create table "event"
       ( "id"           bigserial    primary key
       , "title"        varchar      not null
       , "held"         timestamp    not null
       , "category"     event_t      not null
       , "committee"    committee_t  not null
       , "description"  varchar      not null
       , constraint "unique_event_title_held" unique ("title", "held")
    )|]
   ]

-- | Records attendance of members at events
--
--   * @member_id@ - Member in attendance
--   * @event_id@  - Event being attended
--   * @host@ - This attendee also hosted the event
event_attendee :: SchemaInit
event_attendee = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "event_attendee" cascade|]
   , [H.stmt|create table "event_attendee"
       ( "member_id"  bigint   not null
       , "event_id"   bigint   not null
       , "host"       boolean  not null default false
       , constraint "unique_event_attendee" unique ("member_id", "event_id")
    )|]
   ]

-- | Project record.
--
--   * @id@           - Unique id of the project
--   * @title@        - Title of the project
--   * @description@  - Description of the project. Should be blog post in
--                      length
--   * @submitted@    - Date submitted
--   * @passed@       - Date passed (if null, project has not been evaluated)
--   * @committee@    - Committee afiliated with the project
--   * @project_type@ - Kind of project (e.g. "Major")
--   * @comments@     - Eboard comments on the project
--   * @status@       - current status of the project, for possible values, see
--                      @status_t@
project :: SchemaInit
project = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "project" cascade|]
   , [H.stmt|create table "project"
       ( "id"            bigserial    primary key
       , "title"         varchar      not null
       , "description"   varchar      not null
       , "submitted"     timestamp    not null
       , "passed"        timestamp    default null
       , "committee"     committee_t  not null
       , "project_type"  project_t    not null
       , "comments"      varchar      default null
       , "status"        status_t     not null default 'pending'
    )|]
   ]

-- | Records a member's participation in a project. This table exists for
--   group major project support.
--
--   * @member_id@   - The id of the member who participated in the project
--   * @project_id@  - The id of the project participated in
--   * @description@ - The description of the work contribution.
--                     In single owner projects this field will be null.
--                     Otherwise, it will read something like:
--                     "I'd say the work distribution was about 60/40"
project_participant :: SchemaInit
project_participant = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "project_participant" cascade|]
   , [H.stmt|create table "project_participant"
       ( "member_id"    bigint   not null
       , "project_id"   bigint   not null
       , "description"  varchar  default null
       , constraint "one_member_per_participant" unique ("member_id", "project_id")
    )|]
   ]

-- | A record of an evaluation. These should be created at the beginning of a
--   term as an active member. Effectively scheduling an evaluation.
--
--   * @id@        - Unique id of the evaluation
--   * @member_id@ - id of member being evaluated
--   * @comments@  - any comments the member may have on their pending evaluation
--   * @deadline@  - the date the evaluation occurs
--   * @available@ - the availability of the results
--   * @status@    - status of the evaluation
--   * @eval_type@ - see @eval_t@ for details.
evaluation :: SchemaInit
evaluation = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "evaluation" cascade|]
   , [H.stmt|create table "evaluation"
       ( "id"         bigserial  primary key
       , "member_id"  bigint     not null
       , "comments"   varchar    default null
       , "deadline"   timestamp  not null
       , "available"  boolean    default false
       , "status"     status_t   not null default 'pending'
       , "eval_type"  eval_t     not null
    )|]
   ]

-- | Represents a conditional and its stipulations. No result is recorded for
--   the conditional as that is tied to the evaluation record
--
--   * @id@            - unique id of the conditional
--   * @evaluation_id@ - id of the evaluation associated with this 
--   * @deadline@      - date the conditional is due
--   * @description@   - explanation of the terms of the conditional
--   * @comments@      - summary of the evaluation
conditional :: SchemaInit
conditional = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "conditional" cascade|]
   , [H.stmt|create table "conditional"
       ( "id"             bigserial  primary key
       , "evaluation_id"  bigint     not null  constraint "one_conditional_per_eval" unique
       , "deadline"       timestamp  not null
       , "description"    varchar    not null
       , "comments"       varchar    default null
    )|]
   ]

-- | Represents a freshman project
--
-- * @id@          - Unique id
-- * @description@ - Writeup of the actual project.
-- * @term_id@     - Term the project was held
-- * @event_id@    - The event associated with the project
freshman_project :: SchemaInit
freshman_project = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "freshman_project" cascade|]
   , [H.stmt|create table "freshman_project"
       ( "id"            bigserial  primary key
       , "description"   varchar    not null
       , "term_id"       bigint     not null
       , "event_id"      bigint     default null
    )|]
   ]

-- | A participant in a freshman project
--
-- * @freshman_project_id@ - freshman project participated in
-- * @evaluation_id@       - evaluation associated with the participation
-- * @eboard@              - whether or not this participant was on freshman
--                           eboard, the most prestigious of positions
-- * @status@              - result of the project. Determined by freshman eboard
-- * @comments@            - comments on the participation from the freshman eboard
freshman_project_participant :: SchemaInit
freshman_project_participant = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "freshman_project_participant" cascade|]
   , [H.stmt|create table "freshman_project_participant"
       ( "freshman_project_id"  bigint    not null
       , "evaluation_id"        bigint    not null
       , "eboard"               boolean   not null default false
       , "status"               status_t  not null default 'pending'
       , "comments"             varchar   default null
       , constraint "one_freshman_project_per_eval" unique ("freshman_project_id", "evaluation_id")
    )|]
   ]

-- | Central record for a packet.
--
-- * @id@          - unique packet identifier. A member may have more than one
--                   packet, so they must be distinguishable
-- * @member_id@   - id of the member who owns the packet
-- * @due_date@    - the due date of the packet
-- * @percent_req@ - the percentage of the packet that must be completed to pass
--                   this is kept as a per packet parameter because the number
--                   has been debated several times. If the percentage is
--                   changed sometime in the future old packets should still
--                   keep their original requiremnts.
packet :: SchemaInit
packet = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "packet" cascade|]
   , [H.stmt|create table "packet"
       ( "id"           bigserial  primary key
       , "member_id"    bigint     not null
       , "due_date"     date       not null
       , "percent_req"  integer    not null
       , constraint "no_simultaneous_packets" unique ("member_id", "due_date")
    )|]
   ]

-- | A packet signature.
--   At the time of packet creation, a number of rows are entered into this
--   table to represent the signatures needed by a freshman. Specifically,
--   one row is entered for each member living on floor, and each off floor
--   eboard member. Alumni and off floor signatures are entered on an ad-hoc
--   basis.
--
-- * @member_id@ - the member signing (or expected to sign) the packet
-- * @packet_id@ - the packet associated with the signature
-- * @required@  - Whether or not the 
-- * @signed@    - The date the packet was signed. This can be null for
--                 signatures that were required but never aquired
signature :: SchemaInit
signature = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "signature" cascade|]
   , [H.stmt|create table "signature"
       ( "member_id"  bigint     not null
       , "packet_id"  bigint     not null
       , "required"   boolean    not null
       , "signed"     timestamp  default null
       , constraint "one_signature_per_packet_per_member" unique ("member_id", "packet_id")
    )|]
   ]


-- | The Housing Queue.
--   This represents the queue waiting to move on floor.
--
-- * @member_id@ - the id of the member waiting in the queue
-- * @entered@   - the date the member entered the queue
-- * @exited@    - the date the member left the queue (due to entering a room
--                 or otherwise
queue :: SchemaInit
queue = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "queue" cascade|]
   , [H.stmt|create table "queue"
       ( "member_id"  bigint     not null
       , "entered"    timestamp  not null
       , "exited"     timestamp  default null
       , constraint "no_simultaneous_queue_positions" unique ("member_id", "entered")
       )
     |]
   ]

-- | An application
-- This represents the record of an application for Introductory Membership
--
-- @id@        - the unique id of the application
-- @member_id@ - the member associated with the application (a member entry is
--               created for each applicant, but an applicant could apply many
--               times)
-- @created@   - the date the application was created
-- @status@    - the status of the application
application :: SchemaInit
application = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "application" cascade|]
   , [H.stmt|create table "application"
       ( "id"         bigserial  primary key
       , "member_id"  bigint     not null
       , "created"    timestamp  not null
       , "status"     status_t   not null default 'pending'
       , constraint "no_simultaneous_applications" unique ("member_id", "created")
    )|]
   ]

-- | A metric used on applications. For example: "social"
--
-- * @id@     - unique id of a of the metric
-- * @name@   - the name of the metric
-- * @active@ - whether or not the metric currently appears on applications
metric :: SchemaInit
metric = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "metric" cascade|]
   , [H.stmt|create table "metric"
       ( "id"      bigserial  primary key
       , "name"    varchar    not null  constraint "unique_metric_name" unique
       , "active"  boolean    default true
    )|]
   ]


-- | The metric score given by a reviewer to an application
--
-- * @metric_id@   - the category being scored
-- * @review_id@ - the person currently reviewing applications
-- * @score@       - the score given to the application
review_metric :: SchemaInit
review_metric = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "review_metric" cascade|]
   , [H.stmt|create table "review_metric"
       ( "metric_id"    bigint   not null
       , "review_id"  bigint   not null
       , "score"        integer  not null
       , constraint "one_score_per_review_per_metric" unique ("metric_id", "review_id")
    )|]
   ]

-- | The metric score given from an interview
--
-- * @metric_id@   - the category being scored
-- * @interview_id@ - the person currently reviewing applications
-- * @score@       - the score given to the application
interview_metric :: SchemaInit
interview_metric = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "interview_metric" cascade|]
   , [H.stmt|create table "interview_metric"
       ( "metric_id"       bigint   not null
       , "interview_id"  bigint   not null
       , "score"           integer  not null
       , constraint "one_score_per_interview_per_metric" unique ("metric_id", "interview_id")
    )|]
   ]

-- | A member's review of an application
--
-- * @id@             - unique identifier for the review
-- * @member_id@      - the member reviewing
-- * @application_id@ - the application under review
-- * @review_start@   - the start time of the member's review
-- * @review_end@     - the end time of the member's review
review :: SchemaInit
review = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "review" cascade|]
   , [H.stmt|create table "review"
       ( "id"              bigserial  primary key
       , "member_id"       bigint     not null
       , "application_id"  bigint     not null
       , "review_start"    timestamp  not null
       , "review_submit"   timestamp  not null
       , constraint "one_review_per_member_per_application" unique ("member_id", "application_id")
    )|]
   ]

-- | A member's interview with an applicant
--
-- * @id@             - unique identifier for the interview review
-- * @member_id@      - the member interviewing the applicant
-- * @application_id@ - the application of the interviewee
-- * @interview_date@ - the date of the interview
interview :: SchemaInit
interview = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "interview" cascade|]
   , [H.stmt|create table "interview"
       ( "id"              bigserial  primary key
       , "member_id"       bigint     not null
       , "application_id"  bigint     not null
       , "interview_date"  timestamp  not null
       , constraint "one_interview_per_member_per_application" unique ("member_id", "application_id")
    )|]
   ]

-- | A question on our application
--
-- * @id@     - unique id of the question
-- * @active@ - whether or not it appears on current applications
-- * @query@  - the text of the question
question :: SchemaInit
question = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "question" cascade|]
   , [H.stmt|create table "question"
       ( "id"      bigserial  primary key
       , "active"  boolean    default true
       , "query"   varchar    not null
    )|]
   ]

-- | An answer to a question on an application
--
-- * @application_id@ - the id of the application associated with the answer
-- * @question_id@    - the id of the question being answered
-- * @response@       - the text of the applicant's response to the question
answer :: SchemaInit
answer = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "answer" cascade|]
   , [H.stmt|create table "answer"
       ( "application_id"  bigint   not null
       , "question_id"     bigint   not null
       , "response"        varchar  not null
       , constraint "one_response_per_application_per_question" unique ("application_id", "question_id")
    )|]
   ]

-- | A period of time RIT is open (either a quarter or a semester)
-- 
-- * @id@         - the term id. For example, Fall quarter 2010 would have the
--                  id 20101 and the Fall semester 2014 would have the id 2141
-- * @start_date@ - the start date of the term
-- * @end_date@   - the end date of the term
term :: SchemaInit
term = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "term" cascade|]
   , [H.stmt|create table "term"
       ( "id"          bigint  primary key
       , "start_date"  date    not null  constraint "no_simultaneous_terms" unique
       , "end_date"    date    default null
    )|]
   ]

-- | Whether or not a member has paid dues for a term. This is tracked by the
--   Eval database because it has impacts on who is a voting member.
--
-- * @term_id@   - The term the dues were owed
-- * @member_id@ - The id of the member owing the dues
-- * @status@    - The status of the dues (either paid or exempt). Rationale is
--                 there will not be an entry otherwise
dues :: SchemaInit
dues = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "dues" cascade|]
   , [H.stmt|create table "dues"
       ( "term_id"    bigint  not null
       , "member_id"  bigint  not null
       , "status"     dues_t  not null
       , constraint "one_dues_status_per_term" unique ("term_id", "member_id")
    )|]
   ]

statement :: SchemaInit
statement = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "statement" cascade|]
   , [H.stmt|create table "statement"
       ( "id"            bigserial  primary key
       , "sg_record"     varchar    not null  constraint "unique_statement_group_record" unique
       , "side_effects"  boolean    not null
    )|]
   ]

statement_exec :: SchemaInit
statement_exec = mapM_ H.unitEx
   [ [H.stmt|drop table if exists "statement_exec" cascade|]
   , [H.stmt|create table "statement_exec"
       ( "statement_id"  bigint     not null
       , "member_id"     bigint     not null
       , "timestamp"     timestamp  not null
    )|]
   ]

enableForeignKeys :: SchemaInit
enableForeignKeys = mapM_ H.unitEx
   [ [H.stmt|alter table "eboard" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "room" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "membership" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "event_attendee" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "event_attendee" add foreign key ("event_id") references "event" ("id")|]
   , [H.stmt|alter table "project_participant" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "project_participant" add foreign key ("project_id") references "project" ("id")|]
   , [H.stmt|alter table "evaluation" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "conditional" add foreign key ("evaluation_id") references "evaluation" ("id")|]
   , [H.stmt|alter table "freshman_project" add foreign key ("term_id") references "term" ("id")|]
   , [H.stmt|alter table "freshman_project" add foreign key ("event_id") references "event" ("id")|]
   , [H.stmt|alter table "freshman_project_participant" add foreign key ("freshman_project_id") references "freshman_project" ("id")|]
   , [H.stmt|alter table "freshman_project_participant" add foreign key ("evaluation_id") references "evaluation" ("id")|]
   , [H.stmt|alter table "packet" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "signature" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "signature" add foreign key ("packet_id") references "packet" ("id")|]
   , [H.stmt|alter table "queue" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "application" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "review_metric" add foreign key ("metric_id") references "metric" ("id")|]
   , [H.stmt|alter table "review_metric" add foreign key ("review_id") references "review" ("id")|]
   , [H.stmt|alter table "interview_metric" add foreign key ("metric_id") references "metric" ("id")|]
   , [H.stmt|alter table "interview_metric" add foreign key ("interview_id") references "interview" ("id")|]
   , [H.stmt|alter table "review" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "review" add foreign key ("application_id") references "application" ("id")|]
   , [H.stmt|alter table "interview" add foreign key ("member_id") references "member" ("id")|]
   , [H.stmt|alter table "interview" add foreign key ("application_id") references "application" ("id")|]
   , [H.stmt|alter table "answer" add foreign key ("application_id") references "application" ("id")|]
   , [H.stmt|alter table "answer" add foreign key ("question_id") references "question" ("id")|]
   , [H.stmt|alter table "dues" add foreign key ("term_id") references "term" ("id")|]
   , [H.stmt|alter table "statement_exec" add foreign key ("statement_id") references "statement" ("id")|]
   , [H.stmt|alter table "statement_exec" add foreign key ("member_id") references "member" ("id")|]
   ]

enableIndices :: SchemaInit
enableIndices = mapM_ H.unitEx
   [ [H.stmt|create index "member_id_index" on "member" ("id")|]
   , [H.stmt|create index "member_uuid_index" on"member" ("uuid")|]
   , [H.stmt|create index "member_username_index" on "member" ("username")|]
   , [H.stmt|create index "member_commonname_index" on "member" ("commonname")|]
   , [H.stmt|create index "member_onfloor_status_index" on "member" ("onfloor_status")|]
   , [H.stmt|create index "eboard_member_id_index" on "eboard" ("member_id")|]
   , [H.stmt|create index "room_member_id_index" on "room" ("member_id")|]
   , [H.stmt|create index "room_room_number_index" on "room" ("room_number")|]
   , [H.stmt|create index "membership_member_id_index" on "membership" ("member_id")|]
   , [H.stmt|create index "membership_status_index" on "membership" ("status")|]
   , [H.stmt|create index "event_id_index" on "event" ("id")|]
   , [H.stmt|create index "event_title_index" on "event" ("title")|]
   , [H.stmt|create index "event_category_index" on "event" ("category")|]
   , [H.stmt|create index "event_attendee_member_id_index" on "event_attendee" ("member_id")|]
   , [H.stmt|create index "event_attendee_event_id_index" on "event_attendee" ("event_id")|]
   , [H.stmt|create index "project_id_index" on "project" ("id")|]
   , [H.stmt|create index "project_title_index" on "project" ("title")|]
   , [H.stmt|create index "project_status_index" on "project" ("status")|]
   , [H.stmt|create index "project_participant_member_id_index" on "project_participant" ("member_id")|]
   , [H.stmt|create index "project_participant_project_id_index" on "project_participant" ("project_id")|]
   , [H.stmt|create index "evaluation_id_index" on "evaluation" ("id")|]
   , [H.stmt|create index "evaluation_member_id_index" on "evaluation" ("member_id")|]
   , [H.stmt|create index "evaluation_eval_type_index" on "evaluation" ("eval_type")|]
   , [H.stmt|create index "evaluation_available_index" on "evaluation" ("available")|]
   , [H.stmt|create index "conditional_id_index" on "conditional" ("id")|]
   , [H.stmt|create index "conditional_evaluation_id" on "conditional" ("evaluation_id")|]
   , [H.stmt|create index "freshman_project_id_index" on "freshman_project" ("id")|]
   , [H.stmt|create index "freshman_project_participant_id_index" on "freshman_project_participant" ("freshman_project_id")|]
   , [H.stmt|create index "freshman_project_participant_evaluation_id_index" on "freshman_project_participant" ("evaluation_id")|]
   , [H.stmt|create index "packet_id_index" on "packet" ("id")|]
   , [H.stmt|create index "packet_member_id_index" on "packet" ("member_id")|]
   , [H.stmt|create index "signature_member_id_index" on "signature" ("member_id")|]
   , [H.stmt|create index "signature_packet_id_index" on "signature" ("packet_id")|]
   , [H.stmt|create index "queue_id_index" on "queue" ("id")|]
   , [H.stmt|create index "queue_member_id_index" on "queue" ("member_id")|]
   , [H.stmt|create index "application_id_index" on "application" ("id")|]
   , [H.stmt|create index "application_member_id_index" on "application" ("member_id")|]
   , [H.stmt|create index "metric_id_index" on "metric" ("id")|]
   , [H.stmt|create index "review_metric_metric_id_index" on "review_metric" ("metric_id")|]
   , [H.stmt|create index "review_metric_review_id_index" on "review_metric" ("review_id")|]
   , [H.stmt|create index "interview_metric_metric_id_index" on "interview_metric" ("metric_id")|]
   , [H.stmt|create index "interview_metric_interview_id_index" on "interview_metric" ("interview_id")|]
   , [H.stmt|create index "review_id_index" on "review" ("id")|]
   , [H.stmt|create index "review_member_id_index" on "review" ("member_id")|]
   , [H.stmt|create index "review_application_id_index" on "review" ("application_id")|]
   , [H.stmt|create index "interview_id_index" on "interview" ("id")|]
   , [H.stmt|create index "interview_member_id_index" on "interview" ("member_id")|]
   , [H.stmt|create index "interview_application_id_index" on "interview" ("application_id")|]
   , [H.stmt|create index "question_id_index" on "question" ("id")|]
   , [H.stmt|create index "answer_application_id_index" on "answer" ("application_id")|]
   , [H.stmt|create index "answer_question_id_index" on "answer" ("question_id")|]
   , [H.stmt|create index "term_id_index" on "term" ("id")|]
   , [H.stmt|create index "dues_term_id_index" on "dues" ("term_id")|]
   , [H.stmt|create index "dues_member_id_index" on "dues" ("member_id")|]
   , [H.stmt|create index "statement_id_index" on "statement" ("id")|]
   , [H.stmt|create index "statement_sg_record_index" on "statement" ("sg_record")|]
   , [H.stmt|create index "statement_exec_statement_id_index" on "statement_exec" ("statement_id")|]
   , [H.stmt|create index "statement_exec_member_id_index" on "statement_exec" ("member_id")|]
   ]

-- $objects
-- All objects in the database have a unique identifier of SQL type @bigserial@.
-- This is an auto-incrementing field that can be used to unambiguously refer to
-- a specific object in the database. These can be considered the "discrete
-- units" that the evaluations database stores.

-- $contexts
-- A context is a table that does not have a unique identifier attribute. These
-- tables describe some aspect of the objects that they are referring to, or
-- give that object context.

-- $span_contexts
-- These tables describe spans of time that a member had a particular context.
-- For example, a member can be in a room for a span of time, described as a
-- start date and an end date. If the end date for the span is null, the context
-- currently applies to the member. A member may not have two spans for one
-- context simultaneously.

-- $participation_contexts
-- These tables describe participation. The participation can be for an event, a
-- project, or freshman project.
