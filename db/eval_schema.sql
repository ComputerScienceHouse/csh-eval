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
   ,"uuid"            uuid       default null constraint "unique_member_uuid" unique
   ,"username"        varchar    not null constraint "unique_member_username" unique
   ,"commonname"      varchar    not null constraint "unique_member_commonname" unique
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
   ,constraint "no_simultaneous_eboard_positions" unique ("member_id", "start_date")
);

drop table if exists "room" cascade;
create table "room" (
    "member_id"    bigint   not null
   ,"room_number"  varchar  not null constraint "unique_room_room_number" unique
   ,"start_date"   date     not null
   ,"end_date"     date     default null
   ,constraint "no_simultaneous_room_occupation" unique ("member_id", "start_date")
);

drop table if exists "membership" cascade;
create table "membership" (
    "member_id"   bigint    not null
   ,"status"      member_t  not null
   ,"start_date"  date      not null
   ,"end_date"    date      default null
   ,constraint "no_simultaneous_membership_status" unique ("member_id", "start_date")
);

drop table if exists "event" cascade;
create table "event" (
    "id"           bigserial    primary key
   ,"title"        varchar      not null
   ,"held"         timestamp    not null
   ,"category"     event_t      not null
   ,"committee"    committee_t  not null
   ,"description"  varchar      not null
   ,constraint "unique_event_title_held" unique ("title", "held")
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
   ,"evaluation_id"  bigint     not null constraint "one_conditional_per_eval" unique
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
   ,constraint "one_freshman_project_per_eval" unique ("freshman_project_id", "evaluation_id")
);

drop table if exists "packet" cascade;
create table "packet" (
    "id"           bigserial  primary key
   ,"member_id"    bigint     not null
   ,"due_date"     date       not null
   ,"percent_req"  integer    not null
   ,constraint "no_simultaneous_packets" unique ("member_id", "due_date")
);

drop table if exists "signature" cascade;
create table "signature" (
    "member_id"  bigint     not null
   ,"packet_id"  bigint     not null
   ,"required"   boolean    not null
   ,"signed"     timestamp  default null
   ,constraint "one_signature_per_packet_per_member" unique ("member_id", "packet_id")
);

drop table if exists "queue" cascade;
create table "queue" (
    "id"         bigserial  primary key
   ,"member_id"  bigint     not null
   ,"entered"    timestamp  not null
   ,"exited"     timestamp  default null
   ,constraint "no_simultaneous_queue_positions" unique ("member_id", "entered")
);

drop table if exists "application" cascade;
create table "application" (
    "id"         bigserial  primary key
   ,"member_id"  bigint     not null
   ,"created"    timestamp  not null
   ,"status"     status_t   not null default 'pending'
   ,constraint "no_simultaneous_applications" unique ("member_id", "created")
);

drop table if exists "metric" cascade;
create table "metric" (
    "id"      bigserial  primary key
   ,"name"    varchar    not null constraint "unique_metric_name" unique
   ,"active"  boolean    default true
);

drop table if exists "reviewer_metric" cascade;
create table "reviewer_metric" (
    "metric_id"    bigint   not null
   ,"reviewer_id"  bigint   not null
   ,"score"        integer  not null
   ,constraint "one_score_per_reviewer_per_metric" unique ("metric_id", "reviewer_id")
); -- Make FK and indices for this.

drop table if exists "interviewer_metric" cascade;
create table "interviewer_metric" (
    "metric_id"       bigint   not null
   ,"interviewer_id"  bigint   not null
   ,"score"           integer  not null
   ,constraint "one_score_per_interviewer_per_metric" unique ("metric_id", "interviewer_id")
); -- Make FK and indices for this.

drop table if exists "reviewer" cascade;
create table "reviewer" (
    "id"              bigserial  primary key
   ,"member_id"       bigint     not null
   ,"application_id"  bigint     not null
   ,"review_start"    timestamp  not null
   ,"revew_submit"    timestamp  not null
   ,constraint "one_review_per_member_per_application" unique ("member_id", "application_id")
);

drop table if exists "interviewer" cascade;
create table "interviewer" (
    "id"              bigserial  primary key
   ,"member_id"       bigint     not null
   ,"application_id"  bigint     not null
   ,"interview_date"  timestamp  not null
   ,constraint "one_interview_per_member_per_application" unique ("member_id", "application_id")
);

drop table if exists "question" cascade;
create table "question" (
    "id"      bigserial  primary key
   ,"active"  boolean    default true
   ,"query"   varchar    not null
);

drop table if exists "answer" cascade;
create table "answer" (
    "application_id"  bigint   not null
   ,"question_id"     bigint   not null
   ,"response"        varchar  not null
   ,constraint "one_response_per_application_per_question" unique ("application_id", "question_id")
);

drop table if exists "housing_eval" cascade;
create table "housing_eval" (
    "id"         bigserial  primary key
   ,"eval_date"  date       not null constraint "no_simultaneous_housing_evals" unique
);

drop table if exists "housing_evaluator" cascade;
create table "housing_evaluator" (
    "housing_eval_id"  bigint   not null
   ,"member_id"        bigint   not null
   ,"score"            integer  not null
   ,"voted"            boolean  not null default false
   ,constraint "one_score_per_housing_eval" unique ("housing_eval_id", "member_id")
);

drop table if exists "term" cascade;
create table "term" (
    "id"          bigint  primary key
   ,"start_date"  date    not null constraint "no_simultaneous_terms" unique
   ,"end_date"    date    default null
);

drop table if exists "dues" cascade;
create table "dues" (
    "term_id"    bigint  not null
   ,"member_id"  bigint  not null
   ,"status"     dues_t  not null
   ,constraint "one_dues_status_per_term" unique ("term_id", "member_id")
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

alter table "reviewer_metric" add foreign key ("metric_id") references "metric" ("id");
alter table "reviewer_metric" add foreign key ("reviewer_id") references "reviewer" ("id");

alter table "interviewer_metric" add foreign key ("metric_id") references "metric" ("id");
alter table "interviewer_metric" add foreign key ("interviewer_id") references "interviewer" ("id");

alter table "reviewer" add foreign key ("member_id") references "member" ("id");
alter table "reviewer" add foreign key ("application_id") references "application" ("id");

alter table "interviewer" add foreign key ("member_id") references "member" ("id");
alter table "interviewer" add foreign key ("application_id") references "application" ("id");

alter table "answer" add foreign key ("application_id") references "application" ("id");
alter table "answer" add foreign key ("question_id") references "question" ("id");

alter table "housing_evaluator" add foreign key ("housing_eval_id") references "housing_eval" ("id");
alter table "housing_evaluator" add foreign key ("member_id") references "member" ("id");

alter table "dues" add foreign key ("term_id") references "term" ("id");

create index "member_id_index" on "member" ("id");
create index "member_uuid_index" on"member" ("uuid");
create index "member_username_index" on "member" ("username");
create index "member_commonname_index" on "member" ("commonname");
create index "member_onfloor_status_index" on "member" ("onfloor_status");

create index "eboard_member_id_index" on "eboard" ("member_id");

create index "room_member_id_index" on "room" ("member_id");
create index "room_room_number_index" on "room" ("room_number");

create index "membership_member_id_index" on "membership" ("member_id");
create index "membership_status_index" on "membership" ("status");

create index "event_id_index" on "event" ("id");
create index "event_title_index" on "event" ("title");
create index "event_category_index" on "event" ("category");

create index "event_attendee_member_id_index" on "event_attendee" ("member_id");
create index "event_attendee_event_id_index" on "event_attendee" ("event_id");

create index "project_id_index" on "project" ("id");
create index "project_member_id_index" on "project" ("member_id");
create index "project_title_index" on "project" ("title");
create index "project_status_index" on "project" ("status");

create index "evaluation_id_index" on "evaluation" ("id");
create index "evaluation_member_id_index" on "evaluation" ("member_id");
create index "evaluation_eval_type_index" on "evaluation" ("eval_type");

create index "conditional_id_index" on "conditional" ("id");
create index "conditional_evaluation_id" on "conditional" ("evaluation_id");

create index "freshman_project_id_index" on "freshman_project" ("id");

create index "freshman_project_participant_id_index" on "freshman_project_participant" ("freshman_project_id");
create index "freshman_project_participant_evaluation_id_index" on "freshman_project_participant" ("evaluation_id");

create index "packet_id_index" on "packet" ("id");
create index "packet_member_id_index" on "packet" ("member_id");

create index "signature_member_id_index" on "signature" ("member_id");
create index "signature_packet_id_index" on "signature" ("packet_id");

create index "queue_id_index" on "queue" ("id");
create index "queue_member_id_index" on "queue" ("member_id");

create index "application_id_index" on "application" ("id");
create index "application_member_id_index" on "application" ("member_id");

create index "metric_id_index" on "metric" ("id");

create index "reviewer_metric_metric_id_index" on "reviewer_metric" ("metric_id");
create index "reviewer_metric_reviewer_id_index" on "reviewer_metric" ("reviewer_id");

create index "interviewer_metric_metric_id_index" on "interviewer_metric" ("metric_id");
create index "interviewer_metric_interviewer_id_index" on "interviewer_metric" ("interviewer_id");

create index "reviewer_id_index" on "reviewer" ("id");
create index "reviewer_member_id_index" on "reviewer" ("member_id");
create index "reviewer_application_id_index" on "reviewer" ("application_id");

create index "interviewer_id_index" on "interviewer" ("id");
create index "interviewer_member_id_index" on "interviewer" ("member_id");
create index "interviewer_application_id_index" on "interviewer" ("application_id");

create index "question_id_index" on "question" ("id");

create index "answer_application_id_index" on "answer" ("application_id");
create index "answer_question_id_index" on "answer" ("question_id");

create index "housing_eval_id_index" on "housing_eval" ("id");

create index "housing_evaluator_housing_eval_id_index" on "housing_evaluator" ("housing_eval_id");
create index "housing_evaluator_member_id_index" on "housing_evaluator" ("member_id");

create index "term_id_index" on "term" ("id");

create index "dues_term_id_index" on "dues" ("term_id");
create index "dues_member_id_index" on "dues" ("member_id");

commit;
