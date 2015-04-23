-- csh-eval PostgreSQL schema
-- ==========================

begin;

drop type if exists "committee_t" cascade;
create type "committee_t" as enum (
    'evals'
   ,'rnd'
   ,'social'
   ,'history'
   ,'opcomm'
   ,'imps'
   ,'financial'
   ,'chairman'
);

drop type if exists "status_t" cascade;
create type "status_t" as enum (
    'pending'
   ,'passed'
   ,'failed'
);

drop type if exists "member_t" cascade;
create type "member_t" as enum (
    'active'
   ,'alumni_good'
   ,'alumni_bad'
   ,'honorary'
   ,'advisory'
   ,'introductory'
   ,'non'
);

drop type if exists "dues_t" cascade;
create type "dues_t" as enum (
    'paid'
   ,'exempt'
);

drop type if exists "event_t" cascade;
create type "event_t" as enum (
    'house'
   ,'social'
   ,'committee'
   ,'seminar'
   ,'orientation'
);

drop type if exists "project_t" cascade;
create type "project_t" as enum (
    'major'
);

drop type if exists "eval_t" cascade;
create type "eval_t" as enum (
    'introductory'
   ,'membership'
);

drop table if exists "member" cascade;
create table "member" (
    "id"              bigserial  primary key
   ,"uuid"            uuid       default null constraint "unique_uuid" unique
   ,"username"        varchar    not null constraint "unique_username" unique
   ,"commonname"      varchar    not null constraint "unique_commonname" unique
   ,"password_hash"   bytea      default null
   ,"password_salt"   bytea      default null
   ,"housing_points"  integer    not null default 0
   ,"onfloor_status"  boolean    not null default false
);

drop table if exists "eboard" cascade;
create table "eboard" (
    "member_id"   bigint       not null
   ,"committee"   committee_t  not null
   ,"start_date"  date         not null
   ,"end_date"    date         default null
);

drop table if exists "room" cascade;
create table "room" (
    "member_id"    bigint   not null
   ,"room_number"  varchar  not null
   ,"start_date"   date     not null
   ,"end_date"     date     default null
);

drop table if exists "membership" cascade;
create table "membership" (
    "member_id"   bigint    not null
   ,"status"      member_t  not null
   ,"start_date"  date      not null
   ,"end_date"    date      default null
);

drop table if exists "event" cascade;
create table "event" (
    "id"           bigserial    primary key
   ,"title"        varchar      not null
   ,"held"         timestamp    not null
   ,"category"     event_t      not null
   ,"committee"    committee_t  not null
   ,"description"  varchar      not null
   ,constraint "unique_title_held" unique ("title", "held")
);

drop table if exists "event_attendee" cascade;
create table "event_attendee" (
    "member_id"  bigint   not null
   ,"event_id"   bigint   not null
   ,"host"       boolean  not null default false
   ,constraint "unique_event_attendee" unique ("member_id", "event_id")
);

drop table if exists "project" cascade;
create table "project" (
    "id"            bigserial    primary key
   ,"member_id"     bigint       not null
   ,"title"         varchar      not null
   ,"description"   varchar      not null
   ,"submitted"     timestamp    not null
   ,"passed"        timestamp    default null
   ,"committee"     committee_t  not null
   ,"project_type"  project_t    not null
   ,"comments"      varchar      default null
   ,"status"        status_t     not null default 'pending'
);

drop table if exists "evaluation" cascade;
create table "evaluation" (
    "id"         bigserial  primary key
   ,"member_id"  bigint     not null
   ,"comments"   varchar    default null
   ,"deadline"   timestamp  not null
   ,"status"     status_t   not null default 'pending'
   ,"eval_type"  eval_t     not null
);

drop table if exists "conditional" cascade;
create table "conditional" (
    "id"             bigserial  primary key
   ,"evaluation_id"  bigint     not null
   ,"deadline"       timestamp  not null
   ,"description"    varchar    not null
   ,"comments"       varchar    default null
);

drop table if exists "freshman_project" cascade;
create table "freshman_project" (
    "id"            bigserial  primary key
   ,"description"   varchar    not null
   ,"project_date"  date       not null
);

drop table if exists "freshman_project_participant" cascade;
create table "freshman_project_participant" (
    "freshman_project_id"  bigint    not null
   ,"evaluation_id"        bigint    not null
   ,"eboard"               boolean   not null default false
   ,"result"               status_t  not null default 'pending'
   ,"comments"             varchar   default null
);

drop table if exists "packet" cascade;
create table "packet" (
    "id"           bigserial  primary key
   ,"member_id"    bigint     not null
   ,"due_date"     date       not null
   ,"percent_req"  integer    not null
);

drop table if exists "signature" cascade;
create table "signature" (
    "member_id"  bigint     not null
   ,"packet_id"  bigint     not null
   ,"required"   boolean    not null
   ,"signed"     timestamp
);

drop table if exists "queue" cascade;
create table "queue" (
    "id"         bigserial  primary key
   ,"member_id"  bigint     not null
   ,"entered"    timestamp  not null
   ,"exited"     timestamp  default null
);

drop table if exists "application" cascade;
create table "application" (
    "id"         bigserial  primary key
   ,"member_id"  bigint     not null
   ,"created"    timestamp  not null
   ,"status"     status_t   not null default 'pending'
);

drop table if exists "reviewer" cascade;
create table "reviewer" (
    "member_id"       bigint     not null
   ,"application_id"  bigint     not null
   ,"review_start"    timestamp  not null -- Why do we track both?
   ,"revew_submit"    timestamp  not null
   ,"social"          integer    not null -- Maybe these fields should be
   ,"technical"       integer    not null -- broken out to facilitate changes
   ,"creativity"      integer    not null -- later on?
   ,"versatility"     integer    not null
   ,"leadership"      integer    not null
   ,"motivation"      integer    not null
   ,"overall_feel"    integer    not null
);

drop table if exists "interviewer" cascade;
create table "interviewer" (
    "member_id"       bigint     not null
   ,"application_id"  bigint     not null
   ,"interview_date"  timestamp  not null
   ,"social"          integer    not null
   ,"technical"       integer    not null
   ,"creativity"      integer    not null
   ,"versatility"     integer    not null
   ,"leadership"      integer    not null
   ,"motivation"      integer    not null
   ,"overall_feel"    integer    not null
);

drop table if exists "question" cascade;
create table "question" (
    "id"              bigserial  primary key
   ,"application_id"  bigint     not null
   ,"query"           varchar    not null
);

drop table if exists "answer" cascade;
create table "answer" (
    "application_id"  bigint   not null
   ,"question_id"     bigint   not null
   ,"response"        varchar  not null
);

drop table if exists "housing_eval" cascade;
create table "housing_eval" (
    "id"         bigserial  primary key
   ,"eval_date"  date       not null
);

drop table if exists "housing_evaluator" cascade;
create table "housing_evaluator" (
    "housing_eval_id"  bigint   not null
   ,"member_id"        bigint   not null
   ,"score"            integer  not null
   ,"voted"            boolean  not null default false
);

drop table if exists "term" cascade;
create table "term" (
    "id"          bigint  primary key
   ,"start_date"  date    not null
   ,"end_date"    date    default null -- Assume current term.
);

drop table if exists "dues" cascade;
create table "dues" (
    "term_id"    bigint  not null
   ,"member_id"  bigint  not null
   ,"status"     dues_t  not null
);

alter table "eboard" add foreign key ("member_id") references "member" ("id");

alter table "room" add foreign key ("member_id") references "member" ("id");

alter table "membership" add foreign key ("member_id") references "member" ("id");

alter table "event_attendee" add foreign key ("member_id") references "member" ("id");
alter table "event_attendee" add foreign key ("event_id") references "event" ("id");

alter table "project" add foreign key ("member_id") references "member" ("id");

alter table "evaluation" add foreign key ("member_id") references "member" ("id");

alter table "conditional" add foreign key ("evaluation_id") references "evaluation" ("id");

alter table "freshman_project_participant" add foreign key ("freshman_project_id") references "freshman_project" ("id");
alter table "freshman_project_participant" add foreign key ("evaluation_id") references "evaluation" ("id");

alter table "packet" add foreign key ("member_id") references "member" ("id");

alter table "signature" add foreign key ("member_id") references "member" ("id");
alter table "signature" add foreign key ("packet_id") references "packet" ("id");

alter table "queue" add foreign key ("member_id") references "member" ("id");

alter table "application" add foreign key ("member_id") references "member" ("id");

alter table "reviewer" add foreign key ("member_id") references "member" ("id");
alter table "reviewer" add foreign key ("application_id") references "application" ("id");

alter table "interviewer" add foreign key ("member_id") references "member" ("id");
alter table "interviewer" add foreign key ("application_id") references "application" ("id");

alter table "question" add foreign key ("application_id") references "application" ("id");

alter table "answer" add foreign key ("application_id") references "application" ("id");
alter table "answer" add foreign key ("question_id") references "question" ("id");

alter table "housing_evaluator" add foreign key ("housing_eval_id") references "housing_eval" ("id");
alter table "housing_evaluator" add foreign key ("member_id") references "member" ("id");

alter table "dues" add foreign key ("term_id") references "term" ("id");

commit;
