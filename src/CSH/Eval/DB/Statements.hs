{-|
Module      : CSH.Eval.DB.Statements
Description : Prepared statement definitions and associated data structures.
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.DB.Statements contains the definition of all SQL statements.
-}

{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module CSH.Eval.DB.Statements where

import Data.Word

import Data.UUID

import Data.Time.Clock

import qualified Data.ByteString as B

import qualified Data.Text as T

import qualified Hasql as H

import qualified Hasql.Postgres as HP

-- * Statement SQL Definitions

-- ** Object Lookup

-- The following prepared statements look up specific objects in the model,
-- typically via a unique identifier or sufficient combination thereof.

-- *** Member

-- | Fetch a member by ID.
getMemberIDP :: Word64 -- ^ Member ID
             -> H.Stmt HP.Postgres
getMemberIDP = [H.stmt|select * from member where id = ?|]

-- | Fetch a member by UUID.
getMemberUUIDP :: UUID -- ^ Member UUID
               -> H.Stmt HP.Postgres
getMemberUUIDP = [H.stmt|select * from "member" where "uuid" = ?|]

-- | Fetch a member by username.
getMemberUsernameP :: T.Text -- ^ Username
                   -> H.Stmt HP.Postgres
getMemberUsernameP = [H.stmt|select * from "member" where "username" = ?|]

-- | Fetch the member(s) with the given commonname.
getMembersCommonnameP :: T.Text -- ^ Common name
                      -> H.Stmt HP.Postgres
getMembersCommonnameP = [H.stmt|select * from "member" where "commonname" = ?|]

-- | Fetch all members with the given on floor status.
getMembersOnfloorP :: Bool -- ^ On floor status
                   -> H.Stmt HP.Postgres
getMembersOnfloorP = [H.stmt|select * from "members" where "onfloor_status" = ?|]

-- *** Event

-- | Fetch an event by ID.
getEventIDP :: Word64 -- ^ Event ID
            -> H.Stmt HP.Postgres
getEventIDP = [H.stmt|select * from "event" where "id" = ?|]

-- | Fetch all event(s) with the given title.
getEventsTitleP :: T.Text -- ^ Event title
                -> H.Stmt HP.Postgres
getEventsTitleP = [H.stmt|select * from "event" where "title" = ?|]

-- *** Project

-- | Fetch a project by ID.
getProjectIDP :: Word64 -- ^ Project ID
              -> H.Stmt HP.Postgres
getProjectIDP = [H.stmt|select * from "project" where "id" = ?|]

-- | Fetch all project(s) with a given title.
getProjectsTitleP :: T.Text -- ^ Project title
                  -> H.Stmt HP.Postgres
getProjectsTitleP = [H.stmt|select * from "project" where "title" = ?|]

-- | Fetch all project(s) with a given status.
getProjectsStatusP :: T.Text -- ^ Project status
                   -> H.Stmt HP.Postgres
getProjectsStatusP = [H.stmt|select * from "project" where "status" = ?|]

-- *** Evaluation

-- | Fetch an evaluation by ID.
getEvaluationIDP :: Word64 -- ^ Evaluation ID
                 -> H.Stmt HP.Postgres
getEvaluationIDP = [H.stmt|select * from "evaluation" where "id" = ?|]

-- | Fetch all evaluation(s) associated with a specific member.
getEvaluationsMemberIDP :: Word64 -- ^ ID of member subject to evaluation
                        -> H.Stmt HP.Postgres
getEvaluationsMemberIDP = [H.stmt|select *
                                  from "evaluation"
                                  where "member_id" = ?|]

-- | Fetch all evaluation(s) with a given status.
getEvaluationsStatusP :: T.Text -- ^ Evaluation status
                      -> H.Stmt HP.Postgres
getEvaluationsStatusP = [H.stmt|select *
                                from "evaluation"
                                where "eval_type" = ?|]

-- *** Conditional

-- | Fetch a conditional by ID.
getConditionalIDP :: Word64 -- ^ Conditional ID
                  -> H.Stmt HP.Postgres
getConditionalIDP = [H.stmt|select * from "conditional" where "id" = ?|]

-- | Fetch all conditional(s) associated with a specific evaluation.
getConditionalEvaluationIDP :: Word64 -- ^ Evaluation ID
                            -> H.Stmt HP.Postgres
getConditionalEvaluationIDP = [H.stmt|select *
                                      from "conditional"
                                      where "evaluation_id" = ?|]

-- *** Freshman Project

-- | Fetch a freshman project by ID.
getFreshmanProjectIDP :: Word64 -- ^ Freshman Project ID
                      -> H.Stmt HP.Postgres
getFreshmanProjectIDP = [H.stmt|select * from "freshman_project" where "id" = ?|]

-- *** Packet

-- | Fetch a packet by ID.
getPacketIDP :: Word64 -- ^ Packet ID
             -> H.Stmt HP.Postgres
getPacketIDP = [H.stmt|select * from "packet" where "id" = ?|]

-- | Fetch all packet(s) associated with a specific member.
getPacketsMemberIDP :: Word64 -- ^ Member ID
                    -> H.Stmt HP.Postgres
getPacketsMemberIDP = [H.stmt|select * from "packet" where "member_id" = ?|]

-- *** Queue

-- | Fetch a queue entry by ID.
getQueueIDP :: Word64 -- ^ Queue ID
            -> H.Stmt HP.Postgres
getQueueIDP = [H.stmt|select * "queue" where "id" = ?|]

-- | Fetch all queue entries associated with a specific member.
getQueuesMemberIDP :: Word64 -- ^ Member ID
                   -> H.Stmt HP.Postgres
getQueuesMemberIDP = [H.stmt|select * from "queueu" where "member_id" = ?|]

-- *** Application

-- | Fetch an application by ID.
getApplicationIDP :: Word64 -- ^ Application ID
                  -> H.Stmt HP.Postgres
getApplicationIDP = [H.stmt|select * from "application" where "id" = ?|]

-- | Fetch all application(s) associated with a specific member.
getApplicationsMemberIDP :: Word64 -- ^ Member ID
                         -> H.Stmt HP.Postgres
getApplicationsMemberIDP = [H.stmt|select * from "application" where "member_id" = ?|]

-- | Fetch all application(s) with a given status.
getApplicationsStatusP :: T.Text -- ^ Application status
                       -> H.Stmt HP.Postgres
getApplicationsStatusP = [H.stmt|select * from "application" where "status" = ?|]

-- | Fetch all applications associated with a specific member and of a given
--   status.
getApplicationsMemberIDStatusP :: Word64 -- ^ Member ID
                               -> T.Text -- ^ Application status
                               -> H.Stmt HP.Postgres
getApplicationsMemberIDStatusP = [H.stmt|select * from "application" where "member_id" = ? and "status" = ?|]

-- *** Metric

-- | Fetch a metric by ID.
getMetricIDP :: Word64 -- ^ Metric ID
             -> H.Stmt HP.Postgres
getMetricIDP = [H.stmt|select * from "metric" where "id" = ?|]

-- | Fetch all metric(s) with the given status.
getMetricsActiveP :: Bool -- ^ Metric activity
                  -> H.Stmt HP.Postgres
getMetricsActiveP = [H.stmt|select * from "metric" where "active" = ?|]

-- *** Review

-- | Fetch a reviewer by ID.
getReviewIDP :: Word64 -- ^ Review ID
               -> H.Stmt HP.Postgres
getReviewIDP = [H.stmt|select * from "review" where "id" = ?|]

-- | Fetch all reviewer(s) associated with a specific member.
getReviewsMemberIDP :: Word64 -- ^ Member ID
                    -> H.Stmt HP.Postgres
getReviewsMemberIDP = [H.stmt|select * from "review" where "member_id" = ?|]

-- | Fetch all reviewer(s) associated with a specific application.
getReviewsApplicationIDP :: Word64 -- ^ Application ID
                         -> H.Stmt HP.Postgres
getReviewsApplicationIDP = [H.stmt|select * from "review" where "application_id" = ?|]

-- | Fetch the reviewer associated with a specific member and a specific
--   application.
getReviewMemberIDApplicationIDP :: Word64 -- ^ Member ID
                                -> Word64 -- ^ Application ID
                                -> H.Stmt HP.Postgres
getReviewMemberIDApplicationIDP = [H.stmt|select * from "review" where "member_id" = ? and "application_id" = ?|]

-- *** Interview

-- | Fetch an interviewer by ID.
getInterviewIDP :: Word64 -- ^ Interview ID
                -> H.Stmt HP.Postgres
getInterviewIDP = [H.stmt|select * from "interview" where "id" = ?|]

-- | Fetch all interviewer(s) associated with a specific member.
getInterviewsMemberIDP :: Word64 -- ^ Member ID
                         -> H.Stmt HP.Postgres
getInterviewsMemberIDP = [H.stmt|select * from "interview" where "member_id" = ?|]

-- | Fetch all interviewer(s) associated with a specific application.
getInterviewsApplicationIDP :: Word64 -- ^ Application ID
                              -> H.Stmt HP.Postgres
getInterviewsApplicationIDP = [H.stmt|select * from "interview" where "application_id" = ?|]

-- | Fetch the interviewer associated with a specific member and a specific
--   application.
getInterviewMemberIDApplicationIDP :: Word64 -- ^ Member ID
                                     -> Word64 -- ^ Application ID
                                     -> H.Stmt HP.Postgres
getInterviewMemberIDApplicationIDP = [H.stmt|select * from "interview" where "member_id" = ? and "application_id" = ?|]

-- *** Question

-- | Fetch a question by ID.
getQuestionIDP :: Word64 -- ^ Question ID
               -> H.Stmt HP.Postgres
getQuestionIDP = [H.stmt|select * from "question" where "id" = ?|]

-- | Fetch all question(s) with a given active status.
getQuestionsActiveP :: Bool -- ^ Question activity
                    -> H.Stmt HP.Postgres
getQuestionsActiveP = [H.stmt|select * from "question" where "active" = ?|]

-- *** Term

-- | Fetch a term by ID.
getTermIDP :: Word64 -- ^ Term ID
           -> H.Stmt HP.Postgres
getTermIDP = [H.stmt|select * from "term" where "id" = ?|]

-- *** Statement

-- | Fetch a statement by ID.
getStatementIDP :: Word64 -- ^ Statemend ID
                -> H.Stmt HP.Postgres
getStatementIDP = [H.stmt|select * from "statement" where "id" = ?|]

-- | Fetch a statement by 'StatementGroup' record name (in this file).
getStatementSgRecordP :: T.Text -- ^ Statement Name
                      -> H.Stmt HP.Postgres
getStatementSgRecordP = [H.stmt|select * from "statement" where "sg_record" = ?|]

-- | Fetch all statements with the given side effects flag.
getStatementsSideEffectsP :: Bool -- ^ Statement side-effects flag
                          -> H.Stmt HP.Postgres
getStatementsSideEffectsP = [H.stmt|select * from "statement" where "side_effects" = ?|]

-- ** Context Lookup

-- The following prepared statements look up specific contextss in the model,
-- typically via a unique object identifier or sufficient combination thereof.

-- *** Eboard

-- | Fetch all Eboard records associated with a specific member.
getEboardsMemberIDP :: Word64 -- ^ Member ID
                    -> H.Stmt HP.Postgres
getEboardsMemberIDP = [H.stmt|select * from "eboard" where "member_id" = ?|]

-- | Fetch all Eboard records for the currently serving Eboard.
getCurrentEboardsP :: H.Stmt HP.Postgres
getCurrentEboardsP = [H.stmt|select * from "eboard" where "end_date" is null|]

-- *** Room

-- | Fetch all room(s) associated with a specific member.
getRoomsMemberIDP :: Word64 -- ^ Member ID
                  -> H.Stmt HP.Postgres
getRoomsMemberIDP = [H.stmt|select * from "room" where "member_id" = ?|]

-- | Fetch all room records with the given room number.
getRoomsRoomNumberP :: T.Text -- ^ Room Number
                    -> H.Stmt HP.Postgres
getRoomsRoomNumberP = [H.stmt|select * from "room" where "room_number" = ?|]

-- | Fetch all room records representing present occupancy.
getCurrentRoomsP :: H.Stmt HP.Postgres
getCurrentRoomsP = [H.stmt|select * from "room" where "end_date" is null|]

-- *** Membership

-- | Fetch all membership records associated with a specific record.
getMembershipsMemberIDP :: Word64 -- ^ Member ID
                        -> H.Stmt HP.Postgres
getMembershipsMemberIDP = [H.stmt|select * from "membership" where "member_id" = ?|]

-- | Fetch all currently enforced membership statuses.
getCurrentMembershipsP :: H.Stmt HP.Postgres
getCurrentMembershipsP = [H.stmt|select * from "membership" where "end_date" is null|]

-- *** Event Attendee

-- | Fetch all event attendee records associated with a specific event.
getEventAttendeesEventIDP :: Word64 -- ^ Member ID
                          -> H.Stmt HP.Postgres
getEventAttendeesEventIDP = [H.stmt|select * from "event_attendee" where "member_id" = ?|]

-- | Fetch all event attendee records associated with a specific member.
getEventAttendeesMemberIDP :: Word64 -- ^ Event ID
                          -> H.Stmt HP.Postgres
getEventAttendeesMemberIDP = [H.stmt|select * from "event_attendee" where "event_id" = ?|]

-- *** Project Participant

-- | Fetch all project participant records associated with a specific member.
getProjectParticipantsMemberIDP :: Word64 -- ^ Member ID
                                -> H.Stmt HP.Postgres
getProjectParticipantsMemberIDP = [H.stmt|select * from "project_participant" where "member_id" = ?|]

-- | Fetch all project participant records associated with a specific project.
getProjectParticipantsProjectIDP :: Word64 -- ^ Project ID
                                 -> H.Stmt HP.Postgres
getProjectParticipantsProjectIDP = [H.stmt|select * from "project_participant" where "project_id" = ?|]

-- *** Freshman Project Participant

-- | Fetch all freshman project participant records associated with a specific
--   freshman project.
getFreshmanProjectParticipantsFreshmanProjectIDP :: Word64 -- ^ Freshman Project ID
                                                 -> H.Stmt HP.Postgres
getFreshmanProjectParticipantsFreshmanProjectIDP = [H.stmt|select * from "freshman_project_participant" where "freshman_project_id" = ?|]

-- | Fetch all freshman project participant records associated with a specific
--   evaluation.
getFreshmanProjectParticipantsEvaluationIDP :: Word64 -- ^ Evaluation ID
                                            -> H.Stmt HP.Postgres
getFreshmanProjectParticipantsEvaluationIDP = [H.stmt|select * from "freshman_project_participant" where "evaluation_id" = ?|]

-- | Fetch all freshman project participant records associated with the Eboard
--   of a specific freshman project.
getFreshmanProjectParticipantsEboardP :: Word64 -- ^ Freshman Project ID
                                      -> H.Stmt HP.Postgres
getFreshmanProjectParticipantsEboardP = [H.stmt|select * from "freshman_project_participant" where "freshman_project_id" = ? and "eboard" = 'true'|]

-- *** Signature

-- | Fetch all signatures associated with a specific member.
getSignaturesMemberIDP :: Word64 -- ^ Member ID
                       -> H.Stmt HP.Postgres
getSignaturesMemberIDP = [H.stmt|select * from "signatures" where "member_id" = ?|]

-- | Fetch all signatures associated with a specific packet.
getSignaturesPacketIDP :: Word64 -- ^ Packet ID
                       -> H.Stmt HP.Postgres
getSignaturesPacketIDP = [H.stmt|select * from "signatures" where "packet_id" = ?|]

-- | Fetch all required signatures associated with a specific packet.
getSignaturesRequiredP :: Word64 -- ^ Packet ID
                       -> H.Stmt HP.Postgres
getSignaturesRequiredP = [H.stmt|select * from "signatures" where "packet_id" = ? and "required" = 'true'|]

-- | Fetch all acquired signatures associated with a specific packet.
getSignaturesSignedP :: Word64 -- ^ Packet ID
                     -> H.Stmt HP.Postgres
getSignaturesSignedP = [H.stmt|select * from "signatures" where "packet_id" = ? and "signed" is not null|]

-- *** Review Metric

-- | Fetch all reviewer metric scores associated with a specific metric.
getReviewMetricsMetricIDP :: Word64 -- ^ Metric ID
                          -> H.Stmt HP.Postgres
getReviewMetricsMetricIDP = [H.stmt|select * from "review_metric" where "metric_id" = ?|]

-- | Fetch all reviewer metric scores associated with a specific reviewer.
getReviewMetricsReviewIDP :: Word64 -- ^ Review ID
                          -> H.Stmt HP.Postgres
getReviewMetricsReviewIDP = [H.stmt|select * from "review_metric" where "reviewer_id" = ?|]

-- *** Interview Metric

-- | Fetch all interviewer metric scores associated with a specific metric.
getInterviewMetricsMetricIDP :: Word64 -- ^ Metric ID
                             -> H.Stmt HP.Postgres
getInterviewMetricsMetricIDP = [H.stmt|select * from "interview_metric" where "metric_id" = ?|]

-- | Fetch all interviewer metric scores associated with a specific interviewer.
getInterviewMetricsInterviewIDP :: Word64 -- ^ Interview ID
                                -> H.Stmt HP.Postgres
getInterviewMetricsInterviewIDP = [H.stmt|select * from "interview_metric" where "interview_id" = ?|]

-- *** Answer

-- | Fetch all application answers associated with a specific application.
getAnswersApplicationIDP :: Word64 -- ^ Application ID
                         -> H.Stmt HP.Postgres
getAnswersApplicationIDP = [H.stmt|select * from "answers" where "application_id" = ?|]

-- | Fetch all application answers associated with a specific question.
getAnswersQuestionIDP :: Word64 -- ^ Question ID
                      -> H.Stmt HP.Postgres
getAnswersQuestionIDP = [H.stmt|select * from "answers" where "question_id" = ?|]

-- *** Dues

-- | Fetch all dues records associated with a specific term.
getDuesTermIDP :: Word64 -- ^ Term ID
               -> H.Stmt HP.Postgres
getDuesTermIDP = [H.stmt|select * from "dues" where "term_id" = ?|]

-- | Fetch all dues records associated with a specific member.
getDuesMemberIDP :: Word64 -- ^ Member ID
                 -> H.Stmt HP.Postgres
getDuesMemberIDP = [H.stmt|select * from "dues" where "member_id" = ?|]

-- *** Statement Exec

-- | Fetch all statement executions associated  with a specific statement.
getStatementExecStatementIDP :: Word64 -- ^ Statement ID
                             -> H.Stmt HP.Postgres
getStatementExecStatementIDP = [H.stmt|select * from "statement_exec" where "statement_id" = ?|]

-- | Fetch all statement executions associated  with a specific member.
getStatementExecMemberIDP :: Word64 -- ^ Member ID
                          -> H.Stmt HP.Postgres
getStatementExecMemberIDP = [H.stmt|select * from "statement_exec" where "member_id" = ?|]

-- ** Object Initializers

-- The following prepared statements instantiate specific objects in the model.
-- Depending on the object, various specialized instantiators may be provided
-- to handle common cases.

-- *** Member

-- | Instantiate an introductory member. The key differences between this
--   statement and 'mkExtantMemberP' are the addition of password handling
--   information and the lack of housing points or on floor status information.
mkIntroMemberP :: UUID         -- ^ Member UUID
               -> T.Text       -- ^ Username
               -> T.Text       -- ^ Common name
               -> B.ByteString -- ^ Password hash
               -> B.ByteString -- ^ Password salt
               -> H.Stmt HP.Postgres
mkIntroMemberP = [H.stmt|insert into "member" ("uuid", "username", "commonname", "password_hash", "password_salt") values (?, ?, ?, ?, ?) returning "id"|]

-- | Instantiate a member who already exists, i.e. has associated records in a
--   previous evaluations records system.
mkExtantMemberP :: UUID   -- ^ Member UUID
                -> T.Text -- ^ Username
                -> T.Text -- ^ Common name
                -> Int    -- ^ Housing points
                -> Bool   -- ^ On floor status
                -> H.Stmt HP.Postgres
mkExtantMemberP = [H.stmt|insert into "member" ("uuid", "username", "commonname", "housing_points", "onfloor_status") values (?, ?, ?, ?, ?) returning "id"|]

-- *** Event

-- | Instantiate an event.
mkEventP :: T.Text  -- ^ Title
         -> UTCTime -- ^ Held time
         -> T.Text  -- ^ Event category
         -> T.Text  -- ^ Event committee
         -> T.Text  -- ^ Description
         -> H.Stmt HP.Postgres
mkEventP = [H.stmt|insert into "event" ("title", "held", "category", "committee", "description") values (?, ?, ?, ?, ?) returning "id"|]

-- *** Project

-- | Instantiate a project.
mkProjectP :: T.Text  -- ^ Title
           -> T.Text  -- ^ Description
           -> UTCTime -- ^ Submission time
           -> T.Text  -- ^ Project committee
           -> T.Text  -- ^ Project type
           -> H.Stmt HP.Postgres
mkProjectP = [H.stmt|insert into "project" ("title", "description", "submitted", "committee", "project_type") values (?, ?, ?, ?, ?) returning "id"|]

-- *** Evaluation

-- | Instantiate an evaluation.
mkEvaluationP :: Word64  -- ^ Member ID
              -> UTCTime -- ^ Evaluation time
              -> T.Text  -- ^ Evaluation type
              -> H.Stmt HP.Postgres
mkEvaluationP = [H.stmt|insert into "evaluation" ("member_id", "deadline", "eval_type") values (?, ?, ?) returning "id"|]

-- *** Conditional

-- | Instantiate a conditional.
mkConditionalP :: Word64  -- ^ Evaluation ID
               -> UTCTime -- ^ Due date
               -> T.Text  -- ^ Description
               -> H.Stmt HP.Postgres
mkConditionalP = [H.stmt|insert into "conditional" ("evaluation_id", "deadline", "description") values (?, ?, ?) returning "id"|]

-- *** Freshman Project

-- | Instantiate a freshman project.
mkFreshmanProjectP :: T.Text  -- ^ Description
                   -> UTCTime -- ^ Project time
                   -> H.Stmt HP.Postgres
mkFreshmanProjectP = [H.stmt|insert into "freshman_project" ("description", "project_date") values (?, ?) returning "id"|]

-- *** Packet

-- | Instantiate a packet.
mkPacketP :: Word64  -- ^ Member ID
          -> UTCTime -- ^ Due date
          -> Int     -- ^ Percent required
          -> H.Stmt HP.Postgres
mkPacketP = [H.stmt|insert into "packet" ("member_id", "due_date", "percent_req") values (?, ?, ?) returning "id"|]

-- *** Application

-- | Instantiate an application.
mkApplicationP :: Word64  -- ^ Member ID
               -> UTCTime -- ^ Creation date
               -> H.Stmt HP.Postgres
mkApplicationP = [H.stmt|insert into "application" ("member_id", "created") values (?, ?) returning "id"|]

-- *** Metric

-- | Instantiate a metric.
mkMetricP :: T.Text -- ^ Name
          -> H.Stmt HP.Postgres
mkMetricP = [H.stmt|insert into "metric" ("name") values (?) returning "id"|]

-- *** Review

-- | Instantiate a reviewer.
mkReviewP :: Word64  -- ^ Member ID
            -> Word64  -- ^ Application ID
            -> UTCTime -- ^ Start time
            -> UTCTime -- ^ Submission time
            -> H.Stmt HP.Postgres
mkReviewP = [H.stmt|inset into "reviewer" ("member_id", "application_id", "review_start", "review_submit") values (?, ?, ?, ?) returning "id"|]

-- *** Interview

-- | Instantiate an interviewer.
mkInterviewP :: Word64  -- ^ Member ID
             -> Word64  -- ^ Application ID
             -> UTCTime -- ^ Interview time
             -> H.Stmt HP.Postgres
mkInterviewP = [H.stmt|insert into "interviewer" ("member_id", "application_id", "interview_date") values (?, ?, ?) returning "id"|]

-- *** Question

-- | Instantiate a question.
mkQuestionP :: T.Text -- ^ Query
            -> H.Stmt HP.Postgres
mkQuestionP = [H.stmt|insert into "question" ("query") values (?) returning "id"|]

-- *** Term

-- | Instantiate a term.
mkTermP :: UTCTime -- ^ Start date
        -> H.Stmt HP.Postgres
mkTermP = [H.stmt|insert into "term" ("start_date") values (?) returning "id"|]

-- *** Statement

-- | Instantiate a statement.
mkStatementP :: T.Text -- ^ Statement name
             -> Bool   -- ^ Side effects flag
             -> H.Stmt HP.Postgres
mkStatementP = [H.stmt|insert into "statement" ("sg_record", "side_effects") values (?, ?) returning "id"|]

-- ** Context Initializers

-- The following prepared statements instantiate specific contexts in the model.
-- Depending on the context, various specialized instantiators may be provided
-- to handle common cases. Unfortunately, Haskell's type system is powerless to
-- ensure the correctness of these queries, so the expected HDBC type(s) and
-- semantics of the positional parameters are provided.

-- *** Eboard

-- | Grant an Eboard context to a given member.
grEboardP :: Word64  -- ^ Member ID
          -> T.Text  -- ^ Committee
          -> UTCTime -- ^ Service start date
          -> H.Stmt HP.Postgres
grEboardP = [H.stmt|insert into "eboard" ("member_id", "committee", "start_date") values (?, ?, ?)|]

-- ***Room

-- | Grant a room occupancy context to a given member.
grRoomP :: Word64  -- ^ Member ID
        -> T.Text  -- ^ Room number
        -> UTCTime -- ^ Residence start date
        -> H.Stmt HP.Postgres
grRoomP = [H.stmt|insert into "room" ("member_id", "room_number", "start_date") values (?, ?, ?)|]

-- *** Queue

-- | Instantiate a queue entry.
grQueueP :: Word64  -- ^ Member ID
         -> UTCTime -- ^ Entrance time
         -> H.Stmt HP.Postgres
grQueueP = [H.stmt|insert into "queue" ("member_id", "entered") values (?, ?)|]

-- *** Membership

-- | Grant a membership context to a given member.
grMembershipP :: Word64  -- ^ Member ID
              -> T.Text  -- ^ Membership status
              -> UTCTime -- ^ Initiation time
              -> H.Stmt HP.Postgres
grMembershipP = [H.stmt|insert into "membership" ("member_id", "status", "start_date") values (?, ?, ?)|]

-- *** Event Attendee

-- | Grant an event attendee context to the given member for the given event.
grEventAttendeeP :: Word64 -- ^ Member ID
                 -> Word64 -- ^ Event ID
                 -> H.Stmt HP.Postgres
grEventAttendeeP = [H.stmt|insert into "event_attendee" ("member_id", "event_id") values (?, ?)|]

-- *** Project Participant

-- | Grant a project participant context to a given member for a given project.
grProjectParticipantP :: Word64 -- ^ Member ID
                      -> Word64 -- ^ Project ID
                      -> T.Text -- ^ Description
                      -> H.Stmt HP.Postgres
grProjectParticipantP = [H.stmt|insert into "project_participant" ("member_id", "project_id", "description") values (?, ?, ?|]

-- *** Freshman Project Participant

-- | Grant a freshman project participant context to the given evaluation
--   instance for the given freshman project.
grFreshmanProjectParticipantP :: Word64 -- ^ Freshman Project ID
                              -> Word64 -- ^ Evaluation ID
                              -> H.Stmt HP.Postgres
grFreshmanProjectParticipantP = [H.stmt|insert into "freshman_project_participant" ("freshman_project_id", "evaluation_id") values (?, ?)|]

-- *** Signature

-- | Grant a signature context to the given packet for the given member,
--   including whether or not the signature was required.
grSignatureP :: Word64 -- ^ Member ID
             -> Word64 -- ^ Packet ID
             -> Bool   -- ^ Required flag
             -> H.Stmt HP.Postgres
grSignatureP = [H.stmt|insert into "signature" ("member_id", "packet_id", "required") values (?, ?, ?)|]

-- *** Review Metric

-- | Grant a reviewer metric context to the given metric for the given reviewer,
--   including the score.
grReviewMetricP :: Word64 -- ^ Metric ID
                -> Word64 -- ^ Review ID
                -> Int    -- ^ Score
                -> H.Stmt HP.Postgres
grReviewMetricP = [H.stmt|insert into "review_metric" ("metric_id", "reviewer_id", "score") values (?, ?, ?)|]

-- *** Interview Metric

-- | Grant an interviewer metric context to the given metric for the given
--   interviewer, including the score.
grInterviewMetricP :: Word64 -- ^ Metric ID
                   -> Word64 -- ^ Interview ID
                   -> Int    -- ^ Score
                   -> H.Stmt HP.Postgres
grInterviewMetricP = [H.stmt|insert into "interviewer_metric" ("metric_id", "interviewer_id", "score") values (?, ?, ?)|]

-- *** Answer

-- | Grant an answer context to the given question for the given application,
--   including the response.
grAnswerP :: Word64 -- ^ Application ID
          -> Word64 -- ^ Question ID
          -> T.Text -- ^ Response
          -> H.Stmt HP.Postgres
grAnswerP = [H.stmt|insert into "answer" ("application_id", "question_id", "response") values (?, ?, ?)|]

-- *** Dues

-- | Grant a dues context to the given member for the given term, including the
--   status.
grDuesP :: Word64 -- ^ Term ID
        -> Word64 -- ^ Member ID
        -> T.Text -- ^ Dues status
        -> H.Stmt HP.Postgres
grDuesP = [H.stmt|insert into "dues" ("term_id", "member_id", "status") values (?, ?, ?)|]

-- *** Statement Exec

-- | Grant a statement execution context to the given statement for the given
--   member, including the time stamp.
grStatementExecP :: Word64  -- ^ Statement ID
                 -> Word64  -- ^ Member ID
                 -> UTCTime -- ^ Execution timestamp
                 -> H.Stmt HP.Postgres
grStatementExecP = [H.stmt|insert into "statement_exec" ("statement_id", "member_id", "timestamp") values (?, ?, ?)|]

-- ** Object Mutators

-- The following prepared statements mutate specific objects in the model.
-- Depending on the object, various specialized mutators may be provided to
-- handle common cases.

-- *** Member

-- | Update a given member's common name.
upMemberCommonNameP :: T.Text -- ^ Common name
                    -> Word64 -- ^ Member ID
                    -> H.Stmt HP.Postgres
upMemberCommonNameP = [H.stmt|update "member" set "commonname" = ? where "id" = ?|]

-- | Update a given member's password hash and salt.
upMemberPasswordP :: B.ByteString -- ^ Password hash
                  -> B.ByteString -- ^ Password salt
                  -> Word64       -- ^ Member ID
                  -> H.Stmt HP.Postgres
upMemberPasswordP = [H.stmt|update "member" set "password_hash" = ?, "password_salt" = ? where "id" = ?|]

-- | Update a given member's housing points.
upMemberHousingPointsP :: Int    -- ^ Housing points
                       -> Word64 -- ^ Member ID
                       -> H.Stmt HP.Postgres
upMemberHousingPointsP = [H.stmt|update "member" set "housing_points" = ? where "id" = ?|]

-- | Update a given member's on floor status.
upMemberOnfloorStatusP :: Bool   -- ^ On floor status
                       -> Word64 -- ^ Member ID
                       -> H.Stmt HP.Postgres
upMemberOnfloorStatusP = [H.stmt|update "member" set "onfloor_status" = ? where "id" = ?|]

-- *** Event

-- | Update a given event's title.
upEventTitleP :: T.Text -- ^ Title
              -> Word64 -- ^ Event ID
              -> H.Stmt HP.Postgres
upEventTitleP = [H.stmt|update "event" set "title" = ? where "id" = ?|]

-- | Update a given event's held time.
upEventHeldP :: UTCTime -- ^ Event held time
             -> Word64  -- ^ Event ID
             -> H.Stmt HP.Postgres
upEventHeldP = [H.stmt|update "event" set "held" = ? where "id" = ?|]

-- | Update a given event's category.
upEventCategoryP :: T.Text -- ^ Event category
                 -> Word64 -- ^ Event ID
                 -> H.Stmt HP.Postgres
upEventCategoryP = [H.stmt|update "event" set "category" = ? where "id" = ?|]

-- | Update a given event's committee.
upEventCommitteeP :: T.Text -- ^ Event committee
                  -> Word64 -- ^ Event ID
                  -> H.Stmt HP.Postgres
upEventCommitteeP = [H.stmt|update "event" set "committee" = ? where "id" = ?|]

-- | Update a given event's description.
upEventDescriptionP :: T.Text -- ^ Description
                    -> Word64 -- ^ Event ID
                    -> H.Stmt HP.Postgres
upEventDescriptionP = [H.stmt|update "event" set "description" = ? where "id" = ?|]

-- *** Project

-- | Update a given project's title.
upProjectTitleP :: T.Text -- ^ Title
                -> Word64 -- ^ Project ID
                -> H.Stmt HP.Postgres
upProjectTitleP = [H.stmt|update "project" set "title" = ? where "id" = ?|]

-- | Update a given project's description..
upProjectDescriptionP :: T.Text -- ^ Description
                      -> Word64 -- ^ Project ID
                      -> H.Stmt HP.Postgres
upProjectDescriptionP = [H.stmt|update "project" set "description" = ? where "id" = ?|]

-- | Update a given project's submission time.
upProjectSubmittedP :: UTCTime -- ^ Project submission time
                    -> Word64  -- ^ Project ID
                    -> H.Stmt HP.Postgres
upProjectSubmittedP = [H.stmt|update "project" set "submitted" = ? where "id" = ?|]

-- | Update a given project's passed time.
upProjectPassedP :: UTCTime -- ^ Project passed time
                 -> Word64  -- ^ Project ID
                 -> H.Stmt HP.Postgres
upProjectPassedP = [H.stmt|update "project" set "passed" = ? where "id" = ?|]

-- | Update a given project's committee.
upProjectCommitteeP :: T.Text -- ^ Project committee
                    -> Word64 -- ^ Project ID
                    -> H.Stmt HP.Postgres
upProjectCommitteeP = [H.stmt|update "project" set "committee" = ? where "id" = ?|]

-- | Update a given project's project type.
upProjectTypeP :: T.Text -- ^ Project type
               -> Word64 -- ^ Project ID
               -> H.Stmt HP.Postgres
upProjectTypeP = [H.stmt|update "project" set "project_type" = ? where "id" = ?|]

-- | Update a given project's comments.
upProjectCommentsP :: T.Text -- ^ Comments
                   -> Word64 -- ^ Project ID
                   -> H.Stmt HP.Postgres
upProjectCommentsP = [H.stmt|update "project" set "comments" = ? where "id" = ?|]

-- | Update a given project's status.
upProjectStatusP :: T.Text -- ^ Status
                 -> Word64 -- ^ Project ID
                 -> H.Stmt HP.Postgres
upProjectStatusP = [H.stmt|update "project" set "status" = ? where "id" = ?|]

-- *** Evaluation

-- | Update a given evaluation's comments.
upEvaluationCommentsP :: T.Text -- ^ Comments
                      -> Word64 -- ^ Evaluation ID
                      -> H.Stmt HP.Postgres
upEvaluationCommentsP = [H.stmt|update "evaluation" set "comments" = ? where "id" = ?|]

-- | Update a given evaluation's deadline.
upEvaluationDeadlineP :: UTCTime -- ^ Deadline
                      -> Word64  -- ^ Evaluation ID
                      -> H.Stmt HP.Postgres
upEvaluationDeadlineP = [H.stmt|update "evaluation" set "deadline" = ? where "id" = ?|]

-- | Update a given evaluation's public availability.
upEvaluationAvailableP :: Bool   -- ^ Availability flag
                       -> Word64 -- ^ Evaluation ID
                       -> H.Stmt HP.Postgres
upEvaluationAvailableP = [H.stmt|update "evaluation" set "available" = ? where "id" = ?|]

-- | Update a given evaluation's status.
upEvaluationStatusP :: T.Text -- ^ Status
                    -> Word64 -- ^ Evaluation ID
                    -> H.Stmt HP.Postgres
upEvaluationStatusP = [H.stmt|update "evaluation" set "status" = ? where "id" = ?|]

-- | Update a given evaluation's type.
upEvaluationTypeP :: T.Text -- ^ Evaluation type
                  -> Word64 -- ^ Evaluation ID
                  -> H.Stmt HP.Postgres
upEvaluationTypeP = [H.stmt|update "evaluation" set "eval_type" = ? where "id" = ?|]

-- *** Conditional

-- | Update a given conditional's deadline.
upConditionalDeadlineP :: UTCTime -- ^ Deadline
                       -> Word64  -- ^ Conditional ID
                       -> H.Stmt HP.Postgres
upConditionalDeadlineP = [H.stmt|update "conditional" set "deadline" = ? where "id" = ?|]

-- | Update a given conditional's description.
upConditionalDescriptionP :: T.Text -- ^ Description
                          -> Word64 -- ^ Conditional ID
                          -> H.Stmt HP.Postgres
upConditionalDescriptionP = [H.stmt|update "conditional" set "description" = ? where "id" = ?|]

-- | Update a given conditional's comments.
upConditionalCommentsP :: T.Text -- ^ Comments
                       -> Word64 -- ^ Conditional ID
                       -> H.Stmt HP.Postgres
upConditionalCommentsP = [H.stmt|update "conditional" set "comments" = ? where "id" = ?|]

-- *** Freshman Project

-- | Update a given freshman project's description.
upFreshmanProjectDescriptionP :: T.Text -- ^ Description
                              -> Word64 -- ^ Freshman Project ID
                              -> H.Stmt HP.Postgres
upFreshmanProjectDescriptionP = [H.stmt|update "freshman_project" set "description" = ? where "id" = ?|]

-- | Update a given freshman project's date.
upFreshmanProjectDateP :: UTCTime -- ^ Project date
                       -> Word64  -- ^
                       -> H.Stmt HP.Postgres
upFreshmanProjectDateP = [H.stmt|update "freshman_project" set "project_date" = ? where "id" = ?|]

-- *** Packet

-- | Update a given packet's due date.
upPacketDueDateP :: UTCTime -- ^ Due date
                 -> Word64  -- ^ Packet ID
                 -> H.Stmt HP.Postgres
upPacketDueDateP = [H.stmt|update "packet" set "due_date" = ? where "id" = ?|]

-- | Update a given packet's percent required.
upPacketPercentReqP :: Int    -- ^ Percent required
                    -> Word64 -- ^ Packet ID
                    -> H.Stmt HP.Postgres
upPacketPercentReqP = [H.stmt|update "packet" set "percent_req" = ? where "id" = ?|]

-- *** Queue

-- | Update a given queue's entry date.
upQueueEnteredP :: UTCTime -- ^ Queue entrance time
                -> Word64  -- ^ Queue ID
                -> H.Stmt HP.Postgres
upQueueEnteredP = [H.stmt|update "queue" set "entered" = ? where "id" = ?|]

-- | Update a given queue's exit date.
upQueueExitedP :: UTCTime -- ^ Queue exit time
               -> Word64  -- ^ Queue ID
               -> H.Stmt HP.Postgres
upQueueExitedP = [H.stmt|update "queue" set "exited" = ? where "id" = ?|]

-- *** Application

-- | Update a given application's creation date.
upApplicationCreatedP :: UTCTime -- ^ Application creation time
                      -> Word64  -- ^ Application ID
                      -> H.Stmt HP.Postgres
upApplicationCreatedP = [H.stmt|update "application" set "created" = ? where "id" = ?|]

-- | Update a given application's status date.
upApplicationStatusP :: T.Text -- ^ Status
                     -> Word64 -- ^ Application ID
                     -> H.Stmt HP.Postgres
upApplicationStatusP = [H.stmt|update "application" set "status" = ? where "id" = ?|]

-- *** Metric

-- | Update a given metric's name.
upMetricNameP :: T.Text -- ^ Name
              -> Word64 -- ^ Metric ID
              -> H.Stmt HP.Postgres
upMetricNameP = [H.stmt|update "metric" set "name" = ? where "id" = ?|]

-- | Update a given metric's activity.
upMetricActiveP :: Bool   -- ^ Metric activity
                -> Word64 -- ^ Metric ID
                -> H.Stmt HP.Postgres
upMetricActiveP = [H.stmt|update "metric" set "active" = ? where "id" = ?|]

-- *** Review

-- | Update a given review's start date.
upReviewReviewStartP :: UTCTime -- ^ Start time
                     -> Word64  -- ^ Review ID
                     -> H.Stmt HP.Postgres
upReviewReviewStartP = [H.stmt|update "review" set "review_start" = ? where "id" = ?|]

-- | Update a given review's submission date.
upReviewReviewSubmitP :: UTCTime -- ^ Submission time
                      -> Word64  -- ^ Review ID
                      -> H.Stmt HP.Postgres
upReviewReviewSubmitP = [H.stmt|update "review" set "review_submit" = ? where "id" = ?|]

-- *** Interview

-- | Update a given interview's date.
upInterviewDateP :: UTCTime -- ^ Date
                 -> Word64  -- ^ Interview ID
                 -> H.Stmt HP.Postgres
upInterviewDateP = [H.stmt|update "interviewer" set "interview_date" = ? where "id" = ?|]

-- *** Question

-- | Update a given question's activity.
upQuestionActiveP :: Bool   -- ^ Question activity
                  -> Word64 -- ^ Question ID
                  -> H.Stmt HP.Postgres
upQuestionActiveP = [H.stmt|update "question" set "active" = ? where "id" = ?|]

-- | Update a given question's text.
upQuestionQueryP :: T.Text -- ^ Query
                 -> Word64 -- ^ Question ID
                 -> H.Stmt HP.Postgres
upQuestionQueryP = [H.stmt|update "question" set "query" = ? where "id" = ?|]

-- *** Term

-- | Update a given term's start date.
upTermStartDateP :: UTCTime -- ^ Start date
                 -> Word64  -- ^ Term ID
                 -> H.Stmt HP.Postgres
upTermStartDateP = [H.stmt|update "term" set "start_date" = ? where "id" = ?|]

-- | Update a given term's end date.
upTermEndDateP :: UTCTime -- ^ End date
               -> Word64  --  Term ID
               -> H.Stmt HP.Postgres
upTermEndDateP = [H.stmt|update "term" set "end_date" = ? where "id" = ?|]

-- *** Statement

-- | Update a given 'Statement's 'StatementGroup' record field name.
upStatementSgRecordP :: T.Text -- ^ Statement name
                     -> Word64 -- ^ Statement ID
                     -> H.Stmt HP.Postgres
upStatementSgRecordP = [H.stmt|update "statement" set "sg_record" = ? where "id" = ?|]

-- | Update a given 'Statement's side effects flag.
upStatementSideEffectsP :: Bool   -- ^ Side effects flag
                        -> Word64 -- ^ Statement ID
                        -> H.Stmt HP.Postgres
upStatementSideEffectsP = [H.stmt|update "statement" set "side_effects" = ? where "id" = ?|]

-- ** Context Mutators

-- The following prepared statements mutate specific contexts in the model.
-- Depending on the context, various specialized mutators may be provided to
-- handle common cases.

-- *** Event Attendee

-- | Update whether or not a given member was a host of a given event.
upEventAttendeeHostP :: Bool   -- ^ Host flag
                     -> Word64 -- ^ Member ID
                     -> Word64 -- ^ Event ID
                     -> H.Stmt HP.Postgres
upEventAttendeeHostP = [H.stmt|update "event_attendee" set "host" = ? where "member_id" = ? and "event_id" = ?|]

-- *** Project Participant

-- | Update a given member's project participation description for a given
--   project.
upProjectParticipantDescriptionP :: T.Text -- ^ Description
                                 -> Word64 -- ^ Member ID
                                 -> Word64 -- ^ Project ID
                                 -> H.Stmt HP.Postgres
upProjectParticipantDescriptionP = [H.stmt|update "project_participant" set "description" = ? where "member_id" = ? and "project_id" = ?|]

-- *** Freshman Project Participant

-- | Update whether or not a given member served on a given freshman project's
--   Eboard.
upFreshmanProjectParticipantEboardP :: Bool   -- ^ Eboard flag
                                    -> Word64 -- ^ Freshman Project ID
                                    -> Word64 -- ^ Evaluation ID
                                    -> H.Stmt HP.Postgres
upFreshmanProjectParticipantEboardP = [H.stmt|update "freshman_project_participant" set "eboard" = ? where "freshman_project_id" = ? and "evaluation_id" = ?|]

-- | Update whether or not a given member passed a given freshman project.
upFreshmanProjectParticipantResultP :: T.Text -- ^ Result
                                    -> Word64 -- ^ Freshman Project ID
                                    -> Word64 -- ^ Evaluation ID
                                    -> H.Stmt HP.Postgres
upFreshmanProjectParticipantResultP = [H.stmt|update "freshman_project_participant" set "result" = ? where "freshman_project_id" = ? and "evaluation_id" = ?|]

-- | Update comments on a given member's participation in a given freshman
--   project.
upFreshmanProjectParticipantCommentsP :: T.Text -- ^ Comments
                                      -> Word64 -- ^ Freshman Project ID
                                      -> Word64 -- ^ Evaluation ID
                                      -> H.Stmt HP.Postgres
upFreshmanProjectParticipantCommentsP = [H.stmt|update "freshman_project_participant" set "comments" = ? where "freshman_project_id" = ? and "evaluation_id" = ?|]

-- *** Signature

-- | Update whether or not a given member's signature was required on a given
--   packet.
upSignatureRequiredP :: Bool   -- ^ Required
                     -> Word64 -- ^ Member Id
                     -> Word64 -- ^ Packet ID
                     -> H.Stmt HP.Postgres
upSignatureRequiredP = [H.stmt|update "signature" set "required" = ? where "member_id" = ? and "packet_id" = ?|]

-- | Update whether or not a given member signed a given packet.
upSignatureSignedP :: Bool   -- ^ Signed
                   -> Word64 -- ^ Member ID
                   -> Word64 -- ^ Packet ID
                   -> H.Stmt HP.Postgres
upSignatureSignedP = [H.stmt|update "signature" set "signed" = ? where "member_id" = ? and "packet_id" = ?|]

-- *** Review Metric

-- | Update a given reviewer's score with respect to a given metric.
upReviewMetricScoreP :: Int    -- ^ Score
                     -> Word64 -- ^ Metric ID
                     -> Word64 -- ^ Review ID
                     -> H.Stmt HP.Postgres
upReviewMetricScoreP = [H.stmt|update "review_metric" set "score" = ? where "metric_id" = ? and "reviewer_id" = ?|]

-- *** Interview Metric

-- | Update a given interviewer's score with respect to a given metric.
upInterviewMetricScoreP :: Int    -- ^ Score
                          -> Word64 -- ^ Metric ID
                          -> Word64 -- ^ Interview ID
                          -> H.Stmt HP.Postgres
upInterviewMetricScoreP = [H.stmt|update "interviewer_metric" set "score" = ? where "metric_id" = ? and "interviewer_id" = ?|]

-- *** Answer

-- | Update a given applicant's response to a given question.
upAnswerResponseP :: T.Text -- ^ Response
                  -> Word64 -- ^ Application ID
                  -> Word64 -- ^ Question iD
                  -> H.Stmt HP.Postgres
upAnswerResponseP = [H.stmt|update "answer" set "response" = ? where "application_id" = ? and "question_id" = ?|]

-- *** Dues

-- | Update whether or not a given member payed dues during a given term.
upDuesStatusP :: T.Text -- ^ Status
              -> Word64 -- ^ Term ID
              -> Word64 -- ^ Member ID
              -> H.Stmt HP.Postgres
upDuesStatusP = [H.stmt|update "dues" set "status" = ? where "term_id" = ? and "member_id" = ?|]
