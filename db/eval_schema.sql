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

CREATE TYPE event_t AS ENUM (
    'house',
    'eboard',
    'evals',
    'rnd',
    'social',
    'history',
    'opcomm',
    'imps',
    'financial',
    'seminar',
    'orientation'
);

CREATE TABLE member (
    id          serial      PRIMARY KEY,
    uuid        varchar     DEFAULT NULL,
    username    varchar     NOT NULL,
    password    varchar     DEFAULT NULL,
    join_time   timestamp   DEFAULT now()
);

CREATE UNIQUE INDEX ldapid
              ON member (uuid);

CREATE TABLE freshman (
    id          integer     PRIMARY KEY REFERENCES member (id),
    eval_date   date        NOT NULL,
    status      status_t    NOT NULL DEFAULT 'pending'
);

CREATE TABLE packet (
    id          serial      PRIMARY KEY,
    owner_id    integer     REFERENCES member (id),
    due_date    date        NOT NULL
);

CREATE TABLE signature (
    id          serial      PRIMARY KEY,
    signer      integer     NOT NULL REFERENCES member (id),
    packet      integer     NOT NULL REFERENCES packet (id),
    signed      timestamp   NOT NULL DEFAULT now()
);

CREATE TABLE event (
    id          serial      PRIMARY KEY,
    title       varchar     NOT NULL,
    host        integer     REFERENCES member (id),
    held        timestamp   DEFAULT now(),
    category    event_t
);

CREATE TABLE event_attendee (
    id          serial      PRIMARY KEY,
    attendee    integer     NOT NULL REFERENCES member (id),
    event       integer     NOT NULL REFERENCES event (id)
);

CREATE TABLE project (
    id              serial      PRIMARY KEY,
    owner           integer     NOT NULL REFERENCES member (id),
    title           varchar     NOT NULL,
    description     varchar     NOT NULL,
    submitted       timestamp   NOT NULL DEFAULT now(),
    approved        timestamp   DEFAULT NULL,
    committee       committee_t NOT NULL,
    major           boolean     NOT NULL,
    comments        varchar     DEFAULT NULL,
    status          status_t    NOT NULL DEFAULT 'pending'
);

CREATE TABLE conditional (
    id              serial      PRIMARY KEY,
    member          integer     NOT NULL REFERENCES member (id),
    evaluation      integer     NOT NULL REFERENCES evaluation (id),
    deadline        timestamp   NOT NULL,
    description     varchar     NOT NULL,
    comments        varchar     DEFAULT '',
    status          status_t    NOT NULL DEFAULT 'pending'
);

CREATE TABLE evaluation (
    id              serial      PRIMARY KEY,
    member          integer     NOT NULL REFERENCES member (id),
    comments        varchar     DEFAULT '',
    deadline        timestamp   NOT NULL,
    status          status_t    NOT NULL DEFAULT 'pending'
);

CREATE TABLE queue (
    id              serial      PRIMARY KEY,
    member          integer     REFERENCES member (id),
    entered         timestamp   NOT NULL DEFAULT now(),
    exited          timestamp   DEFAULT NULL
);
