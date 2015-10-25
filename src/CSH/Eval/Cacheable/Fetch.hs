{-|
Module      : CSH.Eval.Cacheable.Fetch
Description : Cacheable Actions to Fetch Objects
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.Cacheable.Fetch defines 'Cacheable' computations for fetching state
objects.
-}

{-# LANGUAGE OverloadedStrings #-}

module CSH.Eval.Cacheable.Fetch (
    -- * Object Fetching Functions
    -- ** Member
    getMemberID
    -- ** Event
  , getEventID
    -- ** Project
  , getProjectID
    -- ** Evaluation
  , getEvaluationID
  , getMemberEvaluations
    -- ** Conditional
  , getConditionalID
  , getEvaluationConditionals
    -- ** FreshmanProject
  , getFreshmanProjectID
    -- ** Packet
  , getPacketID
  , getMemberPackets
    -- ** Application
  , getApplicationID
  , getMemberApplications
    -- ** Metric
  , getMetricID
    -- ** Review
  , getReviewID
  , getApplicationReviews
    -- ** Interview
  , getInterviewID
  , getApplicationInterviews
    -- ** Question
  , getQuestionID
    -- ** Term
  , getTermID
    -- * Context Fetching Functions
    -- ** Eboard
  , getMemberEboards
    -- ** Room
  , getMemberRooms
    -- ** Queue
  , getQueueID
  , getMemberQueues
    -- ** Membership
  , getMemberMemberships
    -- ** EventAttendee
  , getEventEventAttendees
  , getMemberEventAttendees
    -- ** ProjectParticipant
  , getProjectProjectParticipants
  , getMemberProjectParticipants
    -- ** FreshmanProjectParticipant
  , getFreshmanProjectFreshmanProjectParticipants
  , getEvaluationFreshmanProjectParticipants
    -- ** Signature
  , getPacketSignatures
  , getMemberSignatures
    -- ** ReviewMetric
  , getReviewReviewMetrics
  , getMetricReviewMetrics
    -- ** InterviewMetric
  , getInterviewInterviewMetrics
  , getMetricInterviewMetrics
    -- ** Answer
  , getApplicationAnswers
  , getQuestionAnswers
    -- ** Dues
  , getMemberDues
  , getTermDues
    -- * FromRow Functions (Probably shouldn't be exported...)
  , memberFromRow
  , eventFromRow
  , projectFromRow
  , evaluationFromRow
  , conditionalFromRow
  , freshmanProjectFromRow
  , packetFromRow
  , queueFromRow
  , applicationFromRow
  , metricFromRow
  , reviewFromRow
  , interviewFromRow
  , questionFromRow
  , termFromRow
  , eboardFromRow
  , roomFromRow
  , membershipFromRow
  , eventAttendeeFromRow
  , projectParticipantFromRow
  , freshmanProjectParticipantFromRow
  , signatureFromRow
  , reviewMetricFromRow
  , interviewMetricFromRow
  , answerFromRow
  , duesFromRow
    -- * FromVal Functions (Probably shouldn't be exported...)
  , committeeFromVal
  , evaluationTypeFromVal
  , evaluationStatusFromVal
  , memberStatusFromVal
  , duesStatusFromVal
  , eventTypeFromVal
  , projectTypeFromVal
  ) where

import Control.Applicative ((<$>))

import Control.Concurrent.MVar

import Control.Monad.Trans.Either

import Control.Monad.IO.Class (liftIO)

import Data.Maybe (fromMaybe)

import qualified Data.Map as M

import Data.Word

import Data.UUID

import Data.Time

import qualified Data.ByteString as B

import qualified Data.Text as T

import Hasql
import Hasql.Postgres

import CSH.Eval.Model

import CSH.Eval.DB.Statements

import CSH.Eval.Cacheable.Prim

getMemberID :: Word64 -> Cacheable Member
getMemberID i c = do
    mc <- hitSegment memberIDCache c
    m  <- hitRecordFallback i mc (sneakyGhostM memberIDCache i db c)
    right m
    where db = maybeFallback (getMemberIDP i) (noSuchID "Member" i) memberFromRow c

getEventID :: Word64 -> Cacheable Event
getEventID i c = do
    ec <- hitSegment eventIDCache c
    e  <- hitRecordFallback i ec (sneakyGhostM eventIDCache i db c)
    right e
    where db = maybeFallback (getEventIDP i) (noSuchID "Event" i) eventFromRow c

getProjectID :: Word64 -> Cacheable Project
getProjectID i c = do
    pc <- hitSegment projectIDCache c
    p  <- hitRecordFallback i pc (sneakyGhostM projectIDCache i db c)
    right p
    where db = maybeFallback (getProjectIDP i) (noSuchID "Project" i) projectFromRow c

getEvaluationID :: Word64 -> Cacheable Evaluation
getEvaluationID i c = do
    ec <- hitSegment evaluationIDCache c
    e  <- hitRecordFallback i ec (sneakyGhostM evaluationIDCache i db c)
    right e
    where db = maybeFallback (getEvaluationIDP i) (noSuchID "Evaluation" i) evaluationFromRow c

getMemberEvaluations :: Word64 -> Cacheable [Evaluation]
getMemberEvaluations i c = do
    ec <- hitSegment evaluationMemberIDCache c
    e <- hitRecordFallback i ec (sneakyGhostM evaluationMemberIDCache i db c)
    right e
    where db = listFallback (getEvaluationsMemberIDP i) evaluationFromRow c

getConditionalID :: Word64 -> Cacheable Conditional
getConditionalID i c = do
    cc <- hitSegment conditionalIDCache c
    c' <- hitRecordFallback i cc (sneakyGhostM conditionalIDCache i db c)
    right c'
    where db = maybeFallback (getConditionalIDP i) (noSuchID "Conditional" i) conditionalFromRow c

getEvaluationConditionals :: Word64 -> Cacheable [Conditional]
getEvaluationConditionals i c = do
    cc <- hitSegment conditionalEvaluationIDCache c
    c' <- hitRecordFallback i cc (sneakyGhostM conditionalEvaluationIDCache i db c)
    right c'
    where db = listFallback (getConditionalEvaluationIDP i) conditionalFromRow c

getFreshmanProjectID :: Word64 -> Cacheable FreshmanProject
getFreshmanProjectID i c = do
    fc <- hitSegment freshmanProjectIDCache c
    f  <- hitRecordFallback i fc (sneakyGhostM freshmanProjectIDCache i db c)
    right f
    where db = maybeFallback (getFreshmanProjectIDP i) (noSuchID "FreshmanProject" i) freshmanProjectFromRow c

getPacketID :: Word64 -> Cacheable Packet
getPacketID i c = do
    pc <- hitSegment packetIDCache c
    p  <- hitRecordFallback i pc (sneakyGhostM packetIDCache i db c)
    right p
    where db = maybeFallback (getPacketIDP i) (noSuchID "Packet" i) packetFromRow c

getMemberPackets :: Word64 -> Cacheable [Packet]
getMemberPackets i c = do
    pc <- hitSegment packetMemberIDCache c
    p  <- hitRecordFallback i pc (sneakyGhostM packetMemberIDCache i db c)
    right p
    where db = listFallback (getPacketsMemberIDP i) packetFromRow c

getApplicationID :: Word64 -> Cacheable Application
getApplicationID i c = do
    ac <- hitSegment applicationIDCache c
    a  <- hitRecordFallback i ac (sneakyGhostM applicationIDCache i db c)
    right a
    where db = maybeFallback (getApplicationIDP i) (noSuchID "Application" i) applicationFromRow c

getMemberApplications :: Word64 -> Cacheable [Application]
getMemberApplications i c = do
    ac <- hitSegment applicationMemberIDCache c
    a  <- hitRecordFallback i ac (sneakyGhostM applicationMemberIDCache i db c)
    right a
    where db = listFallback (getApplicationsMemberIDP i) applicationFromRow c

getMetricID :: Word64 -> Cacheable Metric
getMetricID i c = do
    mc <- hitSegment metricIDCache c
    m  <- hitRecordFallback i mc (sneakyGhostM metricIDCache i db c)
    right m
    where db = maybeFallback (getMetricIDP i) (noSuchID "Metric" i) metricFromRow c

getReviewID :: Word64 -> Cacheable Review
getReviewID i c = do
    rc <- hitSegment reviewIDCache c
    r  <- hitRecordFallback i rc (sneakyGhostM reviewIDCache i db c)
    right r
    where db = maybeFallback (getReviewIDP i) (noSuchID "Review" i) reviewFromRow c

getApplicationReviews :: Word64 -> Cacheable [Review]
getApplicationReviews i c = do
    rc <- hitSegment reviewApplicationIDCache c
    r  <- hitRecordFallback i rc (sneakyGhostM reviewApplicationIDCache i db c)
    right r
    where db = listFallback (getReviewsApplicationIDP i) reviewFromRow c

getInterviewID :: Word64 -> Cacheable Interview
getInterviewID i c = do
    ic <- hitSegment interviewIDCache c
    i' <- hitRecordFallback i ic (sneakyGhostM interviewIDCache i db c)
    right i'
    where db = maybeFallback (getInterviewIDP i) (noSuchID "Interview" i) interviewFromRow c

getApplicationInterviews :: Word64 -> Cacheable [Interview]
getApplicationInterviews i c = do
    ic <- hitSegment interviewApplicationIDCache c
    i' <- hitRecordFallback i ic (sneakyGhostM interviewApplicationIDCache i db c)
    right i'
    where db = listFallback (getInterviewsApplicationIDP i) interviewFromRow c

getQuestionID :: Word64 -> Cacheable Question
getQuestionID i c = do
    qc <- hitSegment questionIDCache c
    q  <- hitRecordFallback i qc (sneakyGhostM questionIDCache i db c)
    right q
    where db = maybeFallback (getQuestionIDP i) (noSuchID "Question" i) questionFromRow c

getTermID :: Word64 -> Cacheable Term
getTermID i c = do
    tc <- hitSegment termIDCache c
    t  <- hitRecordFallback i tc (sneakyGhostM termIDCache i db c)
    right t
    where db = maybeFallback (getTermIDP i) (noSuchID "Question" i) termFromRow c

getMemberEboards :: Word64 -> Cacheable [Eboard]
getMemberEboards i c = do
    ec <- hitSegment eboardMemberIDCache c
    e  <- hitRecordFallback i ec (sneakyGhostM eboardMemberIDCache i db c)
    right e
    where db = listFallback (getEboardsMemberIDP i) eboardFromRow c

getMemberRooms :: Word64 -> Cacheable [Room]
getMemberRooms i c = do
    rc <- hitSegment roomMemberIDCache c
    r  <- hitRecordFallback i rc (sneakyGhostM roomMemberIDCache i db c)
    right r
    where db = listFallback (getRoomsMemberIDP i) roomFromRow c

getQueueID :: Word64 -> Cacheable Queue
getQueueID i c = do
    qc <- hitSegment queueIDCache c
    q  <- hitRecordFallback i qc (sneakyGhostM queueIDCache i db c)
    right q
    where db = maybeFallback (getQueueIDP i) (noSuchID "Queue" i) queueFromRow c

getMemberQueues :: Word64 -> Cacheable [Queue]
getMemberQueues i c = do
    qc <- hitSegment queueMemberIDCache c
    q  <- hitRecordFallback i qc (sneakyGhostM queueMemberIDCache i db c)
    right q
    where db = listFallback (getQueuesMemberIDP i) queueFromRow c

getMemberMemberships :: Word64 -> Cacheable [Membership]
getMemberMemberships i c = do
    mc <- hitSegment membershipMemberIDCache c
    m  <- hitRecordFallback i mc (sneakyGhostM membershipMemberIDCache i db c)
    right m
    where db = listFallback (getMembershipsMemberIDP i) membershipFromRow c

getEventEventAttendees :: Word64 -> Cacheable [EventAttendee]
getEventEventAttendees i c = do
    ec <- hitSegment eventAttendeeEventIDCache c
    e  <- hitRecordFallback i ec (sneakyGhostM eventAttendeeMemberIDCache i db c)
    right e
    where db = listFallback (getEventAttendeesEventIDP i) eventAttendeeFromRow c

getMemberEventAttendees :: Word64 -> Cacheable [EventAttendee]
getMemberEventAttendees i c = do
    ec <- hitSegment eventAttendeeMemberIDCache c
    e  <- hitRecordFallback i ec (sneakyGhostM eventAttendeeMemberIDCache i db c)
    right e
    where db = listFallback (getEventAttendeesMemberIDP i) eventAttendeeFromRow c

getProjectProjectParticipants :: Word64 -> Cacheable [ProjectParticipant]
getProjectProjectParticipants i c = do
    pc <- hitSegment projectParticipantProjectIDCache c
    p  <- hitRecordFallback i pc (sneakyGhostM projectParticipantProjectIDCache i db c)
    right p
    where db = listFallback (getProjectParticipantsProjectIDP i) projectParticipantFromRow c

getMemberProjectParticipants :: Word64 -> Cacheable [ProjectParticipant]
getMemberProjectParticipants i c = do
    pc <- hitSegment projectParticipantMemberIDCache c
    p  <- hitRecordFallback i pc (sneakyGhostM projectParticipantMemberIDCache i db c)
    right p
    where db = listFallback (getProjectParticipantsMemberIDP i) projectParticipantFromRow c

getFreshmanProjectFreshmanProjectParticipants :: Word64 -> Cacheable [FreshmanProjectParticipant]
getFreshmanProjectFreshmanProjectParticipants i c = do
    fc <- hitSegment freshProjParticipantProjectIDCache c
    f  <- hitRecordFallback i fc (sneakyGhostM freshProjParticipantProjectIDCache i db c)
    right f
    where db = listFallback (getFreshmanProjectParticipantsFreshmanProjectIDP i) freshmanProjectParticipantFromRow c

getEvaluationFreshmanProjectParticipants :: Word64 -> Cacheable [FreshmanProjectParticipant]
getEvaluationFreshmanProjectParticipants i c = do
    fc <- hitSegment freshProjParticipantEvaluationIDCache c
    f  <- hitRecordFallback i fc (sneakyGhostM freshProjParticipantEvaluationIDCache i db c)
    right f
    where db = listFallback (getFreshmanProjectParticipantsEvaluationIDP i) freshmanProjectParticipantFromRow c

getPacketSignatures :: Word64 -> Cacheable [Signature]
getPacketSignatures i c = do
    sc <- hitSegment signaturePacketIDCache c
    s  <- hitRecordFallback i sc (sneakyGhostM signaturePacketIDCache i db c)
    right s
    where db = listFallback (getSignaturesPacketIDP i) signatureFromRow c

getMemberSignatures :: Word64 -> Cacheable [Signature]
getMemberSignatures i c = do
    sc <- hitSegment signatureMemberIDCache c
    s  <- hitRecordFallback i sc (sneakyGhostM signatureMemberIDCache i db c)
    right s
    where db = listFallback (getSignaturesMemberIDP i) signatureFromRow c

getReviewReviewMetrics :: Word64 -> Cacheable [ReviewMetric]
getReviewReviewMetrics i c = do
    rc <- hitSegment reviewMetricReviewIDCache c
    r  <- hitRecordFallback i rc (sneakyGhostM reviewMetricReviewIDCache i db c)
    right r
    where db = listFallback (getReviewMetricsReviewIDP i) reviewMetricFromRow c

getMetricReviewMetrics :: Word64 -> Cacheable [ReviewMetric]
getMetricReviewMetrics i c = do
    rc <- hitSegment reviewMetricMetricIDCache c
    r  <- hitRecordFallback i rc (sneakyGhostM reviewMetricMetricIDCache i db c)
    right r
    where db = listFallback (getReviewMetricsMetricIDP i) reviewMetricFromRow c

getInterviewInterviewMetrics :: Word64 -> Cacheable [InterviewMetric]
getInterviewInterviewMetrics i c = do
    ic <- hitSegment interviewMetricInterviewIDCache c
    i' <- hitRecordFallback i ic (sneakyGhostM interviewMetricInterviewIDCache i db c)
    right i'
    where db = listFallback (getInterviewMetricsInterviewIDP i) interviewMetricFromRow c

getMetricInterviewMetrics :: Word64 -> Cacheable [InterviewMetric]
getMetricInterviewMetrics i c = do
    ic <- hitSegment interviewMetricMetricIDCache c
    i' <- hitRecordFallback i ic (sneakyGhostM interviewMetricMetricIDCache i db c)
    right i'
    where db = listFallback (getInterviewMetricsMetricIDP i) interviewMetricFromRow c

getApplicationAnswers :: Word64 -> Cacheable [Answer]
getApplicationAnswers i c = do
    ac <- hitSegment answerApplicationIDCache c
    a  <- hitRecordFallback i ac (sneakyGhostM answerApplicationIDCache i db c)
    right a
    where db = listFallback (getAnswersApplicationIDP i) answerFromRow c

getQuestionAnswers :: Word64 -> Cacheable [Answer]
getQuestionAnswers i c = do
    ac <- hitSegment answerQuestionIDCache c
    a  <- hitRecordFallback i ac (sneakyGhostM answerQuestionIDCache i db c)
    right a
    where db = listFallback (getAnswersQuestionIDP i) answerFromRow c

getMemberDues :: Word64 -> Cacheable [Dues]
getMemberDues i c = do
    dc <- hitSegment duesMemberIDCache c
    d  <- hitRecordFallback i dc (sneakyGhostM duesMemberIDCache i db c)
    right d
    where db = listFallback (getDuesMemberIDP i) duesFromRow c

getTermDues :: Word64 -> Cacheable [Dues]
getTermDues i c = do
    dc <- hitSegment duesTermIDCache c
    d  <- hitRecordFallback i dc (sneakyGhostM duesTermIDCache i db c)
    right d
    where db = listFallback (getDuesTermIDP i) duesFromRow c

memberFromRow :: (Word64, UUID, T.Text, T.Text, B.ByteString, B.ByteString, Int, Bool) -> Member
memberFromRow (i, u, un, cn, _, _, hp, os) = Member
    i
    u
    un
    cn
    Nothing
    hp
    os
    (getMemberEboards i)
    (getMemberRooms i)
    (getMemberMemberships i)
    (getMemberEvaluations i)
    (getMemberPackets i)
    (getMemberQueues i)
    (getMemberApplications i)
    (getMemberDues i)

eventFromRow :: (Word64, T.Text, UTCTime, T.Text, T.Text, T.Text) -> Event
eventFromRow (i, t, h, ca, co, d) = Event
    i
    t
    h
    (eventTypeFromVal ca)
    (committeeFromVal co)
    d
    (getEventEventAttendees i)

projectFromRow :: (Word64, T.Text, T.Text, UTCTime, Maybe UTCTime, T.Text, T.Text, T.Text, T.Text) -> Project
projectFromRow (i, t, d, s, p, c, pt, co, st) = Project
    i
    t
    d
    s
    p
    (committeeFromVal c)
    (projectTypeFromVal pt)
    co
    (evaluationStatusFromVal st)
    (getProjectProjectParticipants i)

evaluationFromRow :: (Word64, Word64, T.Text, UTCTime, Bool, T.Text, T.Text) -> Evaluation
evaluationFromRow (i, m, c, t, a, s, ty) = Evaluation
    i
    c
    t
    a
    (evaluationStatusFromVal s)
    (evaluationTypeFromVal ty)
    (getMemberID m)
    (getEvaluationConditionals i)
    (getEvaluationFreshmanProjectParticipants i)

conditionalFromRow :: (Word64, Word64, UTCTime, T.Text, T.Text) -> Conditional
conditionalFromRow (i, e, t, d, c) = Conditional
    i
    t
    d
    c
    (getEvaluationID e)

freshmanProjectFromRow :: (Word64, T.Text, Word64, Word64) -> FreshmanProject
freshmanProjectFromRow (i, d, t, e)= FreshmanProject
    i
    d
    (getTermID t)
    (getEventID e)
    (getFreshmanProjectFreshmanProjectParticipants i)

packetFromRow :: (Word64, Word64, Day, Int) -> Packet
packetFromRow (i, m, d, f) = Packet
    i
    d
    f
    (getMemberID m)
    (getPacketSignatures i)

queueFromRow :: (Word64, UTCTime, Maybe UTCTime) -> Queue
queueFromRow (m, en, ex) = Queue
    en
    ex
    (getMemberID m)

applicationFromRow :: (Word64, Word64, UTCTime, T.Text) -> Application
applicationFromRow (i, m, t, s) = Application
    i
    t
    (evaluationStatusFromVal s)
    (getMemberID m)
    (getApplicationReviews i)
    (getApplicationInterviews i)
    (getApplicationAnswers i)

metricFromRow :: (Word64, T.Text, Bool) -> Metric
metricFromRow (i, n, a) = Metric
    i
    n
    a

reviewFromRow :: (Word64, Word64, Word64, UTCTime, UTCTime) -> Review
reviewFromRow (i, m, a, s, f) = Review
    i
    s
    f
    (getMemberID m)
    (getApplicationID a)
    (getReviewReviewMetrics i)

interviewFromRow :: (Word64, Word64, Word64, UTCTime) -> Interview
interviewFromRow (i, m, a, t) = Interview
    i
    t
    (getMemberID m)
    (getApplicationID a)
    (getInterviewInterviewMetrics i)

questionFromRow :: (Word64, Bool, T.Text) -> Question
questionFromRow (i, a, t) = Question
    i
    t
    a

termFromRow :: (Word64, Day, Maybe Day) -> Term
termFromRow (i, s, e) =Term
    i
    s
    e

eboardFromRow :: (Word64, T.Text, Day, Maybe Day) -> Eboard
eboardFromRow (m, c, s, e) = Eboard
    (committeeFromVal c)
    s
    e
    (getMemberID m)

roomFromRow :: (Word64, T.Text, Day, Day) -> Room
roomFromRow (m, r, s, e) = Room
    r
    s
    e
    (getMemberID m)

membershipFromRow :: (Word64, T.Text, Day, Maybe Day) -> Membership
membershipFromRow (m, st, s, e) = Membership
    (memberStatusFromVal st)
    s
    e
    (getMemberID m)

eventAttendeeFromRow :: (Word64, Word64, Bool) -> EventAttendee
eventAttendeeFromRow (m, e, h) = EventAttendee
    h
    (getMemberID m)
    (getEventID e)

projectParticipantFromRow :: (Word64, Word64, T.Text) -> ProjectParticipant
projectParticipantFromRow (m, p, d) = ProjectParticipant
    d
    (getMemberID m)
    (getProjectID p)

freshmanProjectParticipantFromRow :: (Word64, Word64, Bool, T.Text, T.Text) -> FreshmanProjectParticipant
freshmanProjectParticipantFromRow (f, ev, eb, s, c) = FreshmanProjectParticipant
    eb
    (evaluationStatusFromVal s)
    c
    (getFreshmanProjectID f)
    (getEvaluationID ev)

signatureFromRow :: (Word64, Word64, Bool, Maybe UTCTime) -> Signature
signatureFromRow (m, p, r, s) = Signature
    r
    s
    (getMemberID m)
    (getPacketID p)

reviewMetricFromRow :: (Word64, Word64, Int) -> ReviewMetric
reviewMetricFromRow (m, r, s) = ReviewMetric
    s
    (getMetricID m)
    (getReviewID r)

interviewMetricFromRow :: (Word64, Word64, Int) -> InterviewMetric
interviewMetricFromRow (m, i, s) = InterviewMetric
    s
    (getMetricID m)
    (getInterviewID i)

answerFromRow :: (Word64, Word64, T.Text) -> Answer
answerFromRow (a, q, r) = Answer
    r
    (getQuestionID q)
    (getApplicationID a)

duesFromRow :: (Word64, Word64, T.Text) -> Dues
duesFromRow (m, t, s) = Dues
    (duesStatusFromVal s)
    (getMemberID m)
    (getTermID t)

-- | This function is partial! Explain why...
committeeFromVal :: T.Text -> Committee
committeeFromVal "evals"     = Evals
committeeFromVal "rnd"       = RnD
committeeFromVal "social"    = Social
committeeFromVal "history"   = History
committeeFromVal "opcomm"    = OpComm
committeeFromVal "imps"      = Imps
committeeFromVal "financial" = Imps
committeeFromVal "chairmam"  = Chairman

-- | This function is partial! Explain why...
evaluationTypeFromVal :: T.Text -> EvaluationType
evaluationTypeFromVal "introductory" = IntroEval
evaluationTypeFromVal "membership"   = MembershipEval

-- | This function is partial! Explain why...
evaluationStatusFromVal :: T.Text -> EvaluationStatus
evaluationStatusFromVal "pending" = Pending
evaluationStatusFromVal "passed"  = Passed
evaluationStatusFromVal "failed"  = Failed

-- | This function is partial! Explain why...
memberStatusFromVal :: T.Text -> MemberStatus
memberStatusFromVal "active"       = Active
memberStatusFromVal "alumni_good"  = AlumniGood
memberStatusFromVal "alumni_bad"   = AlumniBad
memberStatusFromVal "honorary"     = Honorary
memberStatusFromVal "advisory"     = Advisory
memberStatusFromVal "introductory" = Intro
memberStatusFromVal "non"          = Non

-- This function is partial! Explain why...
duesStatusFromVal :: T.Text -> DuesStatus
duesStatusFromVal "paid"   = Paid
duesStatusFromVal "evempt" = Exempt

-- | This function is partial! Explain why...
eventTypeFromVal :: T.Text -> EventType
eventTypeFromVal "house"       = HouseMeeting
eventTypeFromVal "social"      = SocialEvent
eventTypeFromVal "committee"   = CommitteeMeeting
eventTypeFromVal "seminar"     = Seminar
eventTypeFromVal "orientation" = Orientation
-- | This function is partial! Explain why...
projectTypeFromVal :: T.Text -> ProjectType
projectTypeFromVal "major" = Major
