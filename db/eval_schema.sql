-- csh-eval postgresql schema
-- ==========================

-- extensions
create extension pgcrypto;
create extension "uuid-ossp";
create extension citext;

create type committee_t as enum (
    'evals',
    'rnd',
    'social',
    'history',
    'opcomm',
    'imps',
    'financial'
);

create type status_t as enum (
    'pending',
    'passed',
    'failed'
);

create type member_t as enum (
    'active',
    'alumni',
    'honorary',
    'advisory',
    'introductory',
    'non'
);

create type dues_t as enum (
    'paid',
    'unpaid',
    'exempt'
);

create type event_t as enum (
    'house',
    'social',
    'committee',
    'seminar',
    'orientation'
);

create type project_t as enum (
    'major'
);

create type eval_t as enum (
    'introductory',
    'membership'
);

create table member (
    id             serial      primary key,
    uuid           varchar     default null,
    username       varchar     not null,
    commonname     varchar     not null,
    password       varchar     default null,
    dues           dues_t      not null default 'unpaid',
    housing_points integer     not null default 0,
    on_floor       boolean     not null default false
);

create unique index ldapid
              on member (uuid);

create table eboard (
    member_id   integer     not null references member (id),
    committee   committee_t not null,
    start_date  date        not null,
    end_date    date        default null
);

create table room (
    member_id   integer     not null references member (id),
    room_number varchar     not null,
    start_date  date        not null,
    end_date    date        default null
);

create table membership (
    member_id   integer     not null references member (id),
    status      member_t    not null,
    start_date  date        not null,
    end_date    date        default null
);

create table event (
    id          serial      primary key,
    title       varchar     not null,
    held        timestamp   not null default now(),
    category    event_t     not null,
    committee   committee_t not null,
    description varchar
);

create table event_attendee (
    member_id   integer     not null references member (id),
    event       integer     not null references event (id)
    host        boolean     not null default false;
);

create table project (
    id              serial      primary key,
    member_id       integer     not null references member (id),
    title           varchar     not null,
    description     varchar     not null,
    submitted       timestamp   not null default now(),
    passed          timestamp   default null,
    committee       committee_t not null,
    project_type    project_t   not null,
    comments        varchar     default null,
    status          status_t    not null default 'pending'
);

create table evaluation (
    id              serial      primary key,
    member_id       integer     not null references member (id),
    comments        varchar     default '',
    deadline        timestamp   not null,
    status          status_t    not null default 'pending',
    eval_type       eval_t      not null
);

create table conditional (
    id              serial      primary key,
    evaluation_id   integer     not null references evaluation (id),
    deadline        timestamp   not null,
    description     varchar     not null,
    comments        varchar     default '',
);

create table freshman_project (
    id              serial  primary key,
    description     varchar not null default ''
    project_date    date    not null default now()
);

create table freshman_project_participant (
    project_id  integer     not null references freshman_project (id),
    eval_id     integer     not null references evaluation (id),
    eboard      boolean     not null default false,
    result      status_t    not null default 'pending',
    comments    varchar     default ''
);

create table packet (
    id          serial      primary key,
    member_id   integer     references member (id),
    due_date    date        not null,
    percent_req integer     not null
);

create table signature (
    member_id   integer     not null references member (id),
    packet_id   integer     not null references packet (id),
    required    boolean     not null default false,
    signed      timestamp
);

create table queue (
    id              serial      primary key,
    member          integer     references member (id),
    entered         timestamp   not null default now(),
    exited          timestamp   default null
);

create table application (
    id              serial      primary key,
    member          integer     not null references member (id),
    created         timestamp   not null default now(),
    status          status_t    not null default 'pending',
);

create table reviewer (
    member_id       integer     not null references member (id),
    applicant_id    integer     not null references applicant (id),
    review_start    timestamp,
    revew_submit    timestamp,
    social          integer,
    technical       integer,
    creativity      integer,
    versatility     integer,
    leadership      integer,
    motivation      integer,
    overall_feel    integer
);

create table interviewer (
    member_id       integer     not null  references member (id),
    application_id  integer     not null references application (id),
    interview_date  date        not null default now(),
    social          integer,
    technical       integer,
    creativity      integer,
    versatility     integer,
    leadership      integer,
    motivation      integer,
    overall_feel    integer
);

create table question (
    application_id  integer     not null references application (id),
    query           varchar     not null
);

create table answer (
    application_id  integer     not null references application (id),
    question_id     integer     not null references question (id),
    response        varchar     not null
);

create table housing_eval (
    id          serial  primary key,
    eval_date   date    not null
);

create table housing_evaluator (
    housing_eval_id integer     not null references housing_eval (id),
    member_id       integer     not null references member (id),
    score           integer     not null,
    voted           boolean     not null default false
);
