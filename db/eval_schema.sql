-- csh-eval PostgreSQL schema
-- ==========================

-- extensions
CREATE EXTENSION pgcrypto;
CREATE EXTENSION "uuid-ossp";
CREATE EXTENSION citext;

CREATE TYPE committee_t AS ENUM (
    'evals',
    'rnd',
    'social',
    'history',
    'opcomm',
    'imps',
    'financial'
);

CREATE TYPE status_t AS ENUM (
    'pending',
    'passed',
    'failed'
);

CREATE TYPE member_t AS ENUM (
    'active',
    'alumni',
    'honorary',
    'advisory',
    'introductory',
    'non'
);

CREATE TYPE dues_t AS ENUM (
    'paid',
    'unpaid',
    'exempt'
);

CREATE TYPE event_t AS ENUM (
    'house',
    'social',
    'committee',
    'seminar',
    'orientation'
);

CREATE TYPE project_t as ENUM (
    'major'
);

CREATE TYPE eval_t as ENUM (
    'introductory',
    'membership'
);

CREATE TABLE member (
    id             serial      PRIMARY KEY,
    uuid           varchar     DEFAULT NULL,
    username       varchar     NOT NULL,
    commonname     varchar     NOT NULL,
    password       varchar     DEFAULT NULL,
    dues           dues_t      NOT NULL DEFAULT 'unpaid',
    housing_points integer     NOT NULL DEFAULT 0,
    on_floor       boolean     NOT NULL DEFAULT false
);

CREATE UNIQUE INDEX ldapid
              ON member (uuid);

CREATE TABLE eboard (
    member_id   integer     NOT NULL REFERENCES member (id),
    committee   committee_t NOT NULL,
    start_date  date        NOT NULL,
    end_date    date        DEFAULT NULL
);

CREATE TABLE room (
    member_id   integer     NOT NULL REFERENCES member (id),
    room_number varchar     NOT NULL,
    start_date  date        NOT NULL,
    end_date    date        DEFAULT NULL
);

CREATE TABLE membership (
    member_id   integer     NOT NULL REFERENCES member (id),
    status      member_t    NOT NULL,
    start_date  date        NOT NULL,
    end_date    date        DEFAULT NULL
);

CREATE TABLE event (
    id          serial      PRIMARY KEY,
    title       varchar     NOT NULL,
    held        timestamp   NOT NULL DEFAULT now(),
    category    event_t     NOT NULL,
    description varchar
);

CREATE TABLE event_attendee (
    member_id   integer     NOT NULL REFERENCES member (id),
    event       integer     NOT NULL REFERENCES event (id)
    host        boolean     NOT NULL DEFAULT FALSE;
);

CREATE TABLE project (
    id              serial      PRIMARY KEY,
    member_id       integer     NOT NULL REFERENCES member (id),
    title           varchar     NOT NULL,
    description     varchar     NOT NULL,
    submitted       timestamp   NOT NULL DEFAULT now(),
    passed          timestamp   DEFAULT NULL,
    committee       committee_t NOT NULL,
    project_type    project_t   NOT NULL,
    comments        varchar     DEFAULT NULL,
    status          status_t    NOT NULL DEFAULT 'pending'
);

CREATE TABLE evaluation (
    id              serial      PRIMARY KEY,
    member_id       integer     NOT NULL REFERENCES member (id),
    comments        varchar     DEFAULT '',
    deadline        timestamp   NOT NULL,
    status          status_t    NOT NULL DEFAULT 'pending',
    eval_type       eval_t      NOT NULL
);

CREATE TABLE conditional (
    id              serial      PRIMARY KEY,
    evaluation_id   integer     NOT NULL REFERENCES evaluation (id),
    deadline        timestamp   NOT NULL,
    description     varchar     NOT NULL,
    comments        varchar     DEFAULT '',
);

CREATE TABLE freshman_project (
    id              serial  PRIMARY KEY,
    description     varchar NOT NULL DEFAULT ''
    project_date    date    NOT NULL DEFAULT now()
);

CREATE TABLE freshman_project_participant (
    project_id  integer     NOT NULL REFERENCES freshman_project (id),
    eval_id     integer     NOT NULL REFERENCES evaluation (id),
    eboard      boolean     NOT NULL DEFAULT false,
    result      status_t    NOT NULL DEFAULT 'pending',
    comments    varchar     DEFAULT ''
);

CREATE TABLE packet (
    id          serial      PRIMARY KEY,
    member_id   integer     REFERENCES member (id),
    due_date    date        NOT NULL,
    percent_req integer     NOT NULL
);

CREATE TABLE signature (
    member_id   integer     NOT NULL REFERENCES member (id),
    packet_id   integer     NOT NULL REFERENCES packet (id),
    required    boolean     NOT NULL DEFAULT false,
    signed      timestamp
);

CREATE TABLE queue (
    id              serial      PRIMARY KEY,
    member          integer     REFERENCES member (id),
    entered         timestamp   NOT NULL DEFAULT now(),
    exited          timestamp   DEFAULT NULL
);

CREATE TABLE application (
    id              serial      PRIMARY KEY,
    member          integer     NOT NULL REFERENCES member (id),
    created         timestamp   NOT NULL DEFAULT now(),
    status          status_t    NOT NULL DEFAULT 'pending',
);

CREATE TABLE reviewer (
    member_id       integer     NOT NULL REFERENCES member (id),
    applicant_id    integer     NOT NULL REFERENCES applicant (id),
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

CREATE TABLE interviewer (
    member_id       integer     NOT NULL  REFERENCES member (id),
    application_id  integer     NOT NULL REFERENCES application (id),
    interview_date  date        NOT NULL DEFAULT now(),
    social          integer,
    technical       integer,
    creativity      integer,
    versatility     integer,
    leadership      integer,
    motivation      integer,
    overall_feel    integer
);

CREATE TABLE question (
    application_id  integer     NOT NULL REFERENCES application (id),
    query           varchar     NOT NULL
);

CREATE TABLE answer (
    application_id  integer     NOT NULL REFERENCES application (id),
    question_id     integer     NOT NULL REFERENCES question (id),
    response        varchar     NOT NULL
);

CREATE TABLE housing_eval (
    id          serial  PRIMARY KEY,
    eval_date   date    NOT NULL
);

CREATE TABLE housing_evaluator (
    housing_eval_id integer     NOT NULL REFERENCES housing_eval (id),
    member_id       integer     NOT NULL REFERENCES member (id),
    score           integer     NOT NULL,
    voted           boolean     NOT NULL DEFAULT FALSE -- I missed this part of the discussion, so maybe this is wrong. 
);
