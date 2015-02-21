# coding: utf-8
from sqlalchemy import Boolean, \
                       Column, \
                       Date, \
                       DateTime, \
                       Enum, \
                       ForeignKey, \
                       Integer, \
                       String, \
                       text
from sqlalchemy.orm import relationship
from sqlalchemy.ext.declarative import declarative_base


Base = declarative_base()
metadata = Base.metadata

Committee = Enum(u'evals',
                 u'rnd',
                 u'social',
                 u'history',
                 u'opcomm',
                 u'imps',
                 u'financial',
                 name='committee_t')

Status = Enum(u'pending',
              u'passed',
              u'failed',
              name='status_t')

class Applicant(Base):
    __tablename__ = 'applicant'

    id = Column(Integer,
            primary_key=True,
            server_default=text("nextval('applicant_id_seq'::regclass)"))

    member = Column(ForeignKey(u'member.id'),
                    nullable=False)

    created = Column(DateTime,
                     nullable=False,
                     server_default=text("now()"))

    status = Column(Status,
                    nullable=False,
                    server_default=text("'pending'::status_t"))

    social = Column(Integer)
    technical = Column(Integer)
    creativity = Column(Integer)
    versatility = Column(Integer)
    leadership = Column(Integer)
    motivation = Column(Integer)
    overall_feel = Column(Integer)

    member1 = relationship(u'Member')


class Conditional(Base):
    __tablename__ = 'conditional'

    id = Column(Integer,
                primary_key=True,
                server_default=text("nextval('conditional_id_seq'::regclass)"))

    member = Column(ForeignKey(u'member.id'),
                    nullable=False)

    evaluation = Column(ForeignKey(u'evaluation.id'),
                        nullable=False)

    deadline = Column(DateTime,
                      nullable=False)

    description = Column(String,
                         nullable=False)

    comments = Column(String,
                      server_default=text("''::character varying"))

    status = Column(Status,
                    nullable=False,
                    server_default=text("'pending'::status_t"))

    evaluation1 = relationship(u'Evaluation')

    member1 = relationship(u'Member')


class Evaluation(Base):
    __tablename__ = 'evaluation'

    id = Column(Integer,
                primary_key=True,
                server_default=text("nextval('evaluation_id_seq'::regclass)"))

    member = Column(ForeignKey(u'member.id'),
                    nullable=False)

    comments = Column(String,
                      server_default=text("''::character varying"))

    deadline = Column(DateTime,
                      nullable=False)

    status = Column(Status,
                    nullable=False,
                    server_default=text("'pending'::status_t"))

    member1 = relationship(u'Member')


class Event(Base):
    __tablename__ = 'event'

    id = Column(Integer,
            primary_key=True,
            server_default=text("nextval('event_id_seq'::regclass)"))

    title = Column(String,
                   nullable=False)

    host = Column(ForeignKey(u'member.id'))

    held = Column(DateTime,
                  server_default=text("now()"))

    category = Column(Enum(u'house',
                           u'eboard',
                           u'evals',
                           u'rnd',
                           u'social',
                           u'history',
                           u'opcomm',
                           u'imps',
                           u'financial',
                           u'seminar',
                           u'orientation',
                           name='event_t'))

    member = relationship(u'Member')


class EventAttendee(Base):
    __tablename__ = 'event_attendee'

    id = Column(Integer,
                primary_key=True,
                server_default=text("nextval('event_attendee_id_seq'::regclass)"))

    attendee = Column(ForeignKey(u'member.id'),
                      nullable=False)

    event = Column(ForeignKey(u'event.id'),
                   nullable=False)

    member = relationship(u'Member')

    event1 = relationship(u'Event')


class FreshmanProject(Base):
    __tablename__ = 'freshman_project'

    id = Column(Integer,
                primary_key=True,
                server_default=text("nextval('freshman_project_id_seq'::regclass)"))

    eval_id = Column(ForeignKey(u'evaluation.id'),
                     nullable=False)

    eboard = Column(Boolean,
                    nullable=False,
                    server_default=text("false"))

    result = Column(Status,
                    nullable=False,
                    server_default=text("'pending'::status_t"))

    comments = Column(String,
                      nullable=False,
                      server_default=text("''::character varying"))

    eval = relationship(u'Evaluation')


class Member(Base):
    __tablename__ = 'member'

    id = Column(Integer,
                primary_key=True,
                server_default=text("nextval('member_id_seq'::regclass)"))

    uuid = Column(String,
                  unique=True)

    username = Column(String,
                      nullable=False)

    commonname = Column(String,
                        nullable=False)

    password = Column(String)

    join_time = Column(DateTime,
                       nullable=False,
                       server_default=text("now()"))

    resident = Column(Boolean,
                      nullable=False)

    dues = Column(Enum(u'paid',
                       u'unpaid',
                       u'exempt',
                       name='dues_t'),
                  nullable=False,
                  server_default=text("'unpaid'::dues_t"))

    membership = Column(Enum(u'active',
                             u'alumni',
                             u'honorary',
                             u'advisory',
                             u'introductory',
                             name='member_t'),
                        nullable=False)

    housing_points = Column(Integer,
                            nullable=False,
                            server_default=text("0"))

    room_numer = Column(Integer)


class Packet(Base):
    __tablename__ = 'packet'

    id = Column(Integer,
            primary_key=True,
            server_default=text("nextval('packet_id_seq'::regclass)"))

    owner_id = Column(ForeignKey(u'member.id'))

    due_date = Column(Date,
                      nullable=False)

    owner = relationship(u'Member')


class Project(Base):
    __tablename__ = 'project'

    id = Column(Integer,
            primary_key=True,
            server_default=text("nextval('project_id_seq'::regclass)"))

    owner = Column(ForeignKey(u'member.id'),
                   nullable=False)

    title = Column(String,
                   nullable=False)

    description = Column(String,
                         nullable=False)

    submitted = Column(DateTime,
                       nullable=False,
                       server_default=text("now()"))

    approved = Column(DateTime)

    committee = Column(Committee,
                        nullable=False)

    major = Column(Boolean,
                   nullable=False)

    comments = Column(String)

    status = Column(Status,
                    nullable=False,
                    server_default=text("'pending'::status_t"))

    member = relationship(u'Member')


class Question(Base):
    __tablename__ = 'question'

    id = Column(Integer,
            primary_key=True,
            server_default=text("nextval('question_id_seq'::regclass)"))

    applicant = Column(ForeignKey(u'applicant.id'),
            nullable=False)

    query = Column(String,
            nullable=False)

    response = Column(String,
            nullable=False)

    applicant1 = relationship(u'Applicant')


class Queue(Base):
    __tablename__ = 'queue'

    id = Column(Integer,
                primary_key=True,
                server_default=text("nextval('queue_id_seq'::regclass)"))

    member = Column(ForeignKey(u'member.id'))

    entered = Column(DateTime,
                     nullable=False,
                     server_default=text("now()"))

    exited = Column(DateTime)

    member1 = relationship(u'Member')


class Reviewer(Base):
    __tablename__ = 'reviewer'

    id = Column(Integer,
                primary_key=True,
                server_default=text("nextval('reviewer_id_seq'::regclass)"))

    member_id = Column(ForeignKey(u'member.id'),
                       nullable=False)

    applicant_id = Column(ForeignKey(u'applicant.id'),
                          nullable=False)

    review_start = Column(DateTime)

    revew_submit = Column(DateTime)

    applicant = relationship(u'Applicant')

    member = relationship(u'Member')


class Signature(Base):
    __tablename__ = 'signature'

    id = Column(Integer,
                primary_key=True,
                server_default=text("nextval('signature_id_seq'::regclass)"))

    signer = Column(ForeignKey(u'member.id'),
                    nullable=False)

    packet = Column(ForeignKey(u'packet.id'),
                    nullable=False)

    required = Column(Boolean,
                      nullable=False,
                      server_default=text("false"))

    signed = Column(DateTime)

    packet1 = relationship(u'Packet')
    member = relationship(u'Member')
