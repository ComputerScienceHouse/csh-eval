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
  , getMemberUUID
  , getMemberUsername
  , getMembersCommonname
  , getMembersOnfloor
    -- ** Event
  , getEventID
  , getEventsTitle
    -- ** Project
  , getProjectID
  , getProjectsTitle
  , getProjectsStatus
    -- ** Evaluation
  , getEvaluationID
  , getMemberEvaluations
  , getEvaluationsStatus
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
  , getApplicationsStatus
  , getMemberStatusApplications
    -- ** Metric
  , getMetricID
  , getMetricsActive
    -- ** Review
  , getReviewID
  , getApplicationReviews
  , getMemberReviews
  , getMemberApplicationReview
    -- ** Interview
  , getInterviewID
  , getApplicationInterviews
  , getMemberInterviews
  , getMemberApplicationInterview
    -- ** Question
  , getQuestionID
  , getQuestionsActive
    -- ** Term
  , getTermID
    -- * Context Fetching Functions
    -- ** Eboard
  , getMemberEboards
  , getCurrentEboards
    -- ** Room
  , getMemberRooms
  , getRoomsRoomNumber
  , getCurrentRooms
    -- ** Queue
  , getQueueID
  , getMemberQueues
    -- ** Membership
  , getMemberMemberships
  , getCurrentMemberships
    -- ** EventAttendee
  , getEventEventAttendees
  , getMemberEventAttendees
    -- ** ProjectParticipant
  , getProjectProjectParticipants
  , getMemberProjectParticipants
    -- ** FreshmanProjectParticipant
  , getFreshmanProjectFreshmanProjectParticipants
  , getEvaluationFreshmanProjectParticipants
  , getFreshmanProjectFreshmanProjectEboards
    -- ** Signature
  , getPacketSignatures
  , getPacketSignaturesSigned
  , getPacketRequiredSignatures
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
    -- * ToVal Functions (Move these somewhere else...)
  , committeeToVal
  , evaluationTypeToVal
  , evaluationStatusToVal
  , memberStatusToVal
  , duesStatusToVal
  , eventTypeToVal
  , projectTypeToVal
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
    hitRecordFallback i mc (sneakyGhostM memberIDCache i db c)
    where db = liftMaybeQ (getMemberIDP i) (noSuchID "Member" i) memberFromRow c

getMemberUUID :: UUID -> Cacheable Member
getMemberUUID u c = liftMaybeQ (getMemberUUIDP u) (noSuchThing "member" "UUID" u) memberFromRow c

getMemberUsername :: T.Text -> Cacheable Member
getMemberUsername u c = liftMaybeQ (getMemberUsernameP u) (noSuchThing "member" "username" u) memberFromRow c

getMembersCommonname :: T.Text -> Cacheable [Member]
getMembersCommonname cn c = liftListQ (getMembersCommonnameP cn) memberFromRow c

getMembersOnfloor :: Bool -> Cacheable [Member]
getMembersOnfloor o c = liftListQ (getMembersOnfloorP o) memberFromRow c

getEventID :: Word64 -> Cacheable Event
getEventID i c = do
    ec <- hitSegment eventIDCache c
    hitRecordFallback i ec (sneakyGhostM eventIDCache i db c)
    where db = liftMaybeQ (getEventIDP i) (noSuchID "Event" i) eventFromRow c

getEventsTitle :: T.Text -> Cacheable [Event]
getEventsTitle t c = liftListQ (getEventsTitleP t) eventFromRow c

getProjectID :: Word64 -> Cacheable Project
getProjectID i c = do
    pc <- hitSegment projectIDCache c
    hitRecordFallback i pc (sneakyGhostM projectIDCache i db c)
    where db = liftMaybeQ (getProjectIDP i) (noSuchID "Project" i) projectFromRow c

getProjectsTitle :: T.Text -> Cacheable [Project]
getProjectsTitle t c = liftListQ (getProjectsTitleP t) projectFromRow c

getProjectsStatus :: EvaluationStatus -> Cacheable [Project]
getProjectsStatus e c = liftListQ (getProjectsStatusP (evaluationStatusToVal e)) projectFromRow c

getEvaluationID :: Word64 -> Cacheable Evaluation
getEvaluationID i c = do
    ec <- hitSegment evaluationIDCache c
    hitRecordFallback i ec (sneakyGhostM evaluationIDCache i db c)
    where db = liftMaybeQ (getEvaluationIDP i) (noSuchID "Evaluation" i) evaluationFromRow c

getMemberEvaluations :: Word64 -> Cacheable [Evaluation]
getMemberEvaluations i c = do
    ec <- hitSegment evaluationMemberIDCache c
    hitRecordFallback i ec (sneakyGhostM evaluationMemberIDCache i db c)
    where db = liftListQ (getEvaluationsMemberIDP i) evaluationFromRow c

getEvaluationsStatus :: EvaluationStatus -> Cacheable [Evaluation]
getEvaluationsStatus e c = liftListQ (getEvaluationsStatusP (evaluationStatusToVal e)) evaluationFromRow c

getConditionalID :: Word64 -> Cacheable Conditional
getConditionalID i c = do
    cc <- hitSegment conditionalIDCache c
    hitRecordFallback i cc (sneakyGhostM conditionalIDCache i db c)
    where db = liftMaybeQ (getConditionalIDP i) (noSuchID "Conditional" i) conditionalFromRow c

getEvaluationConditionals :: Word64 -> Cacheable [Conditional]
getEvaluationConditionals i c = do
    cc <- hitSegment conditionalEvaluationIDCache c
    hitRecordFallback i cc (sneakyGhostM conditionalEvaluationIDCache i db c)
    where db = liftListQ (getConditionalEvaluationIDP i) conditionalFromRow c

getFreshmanProjectID :: Word64 -> Cacheable FreshmanProject
getFreshmanProjectID i c = do
    fc <- hitSegment freshmanProjectIDCache c
    hitRecordFallback i fc (sneakyGhostM freshmanProjectIDCache i db c)
    where db = liftMaybeQ (getFreshmanProjectIDP i) (noSuchID "FreshmanProject" i) freshmanProjectFromRow c

getPacketID :: Word64 -> Cacheable Packet
getPacketID i c = do
    pc <- hitSegment packetIDCache c
    hitRecordFallback i pc (sneakyGhostM packetIDCache i db c)
    where db = liftMaybeQ (getPacketIDP i) (noSuchID "Packet" i) packetFromRow c

getMemberPackets :: Word64 -> Cacheable [Packet]
getMemberPackets i c = do
    pc <- hitSegment packetMemberIDCache c
    hitRecordFallback i pc (sneakyGhostM packetMemberIDCache i db c)
    where db = liftListQ (getPacketsMemberIDP i) packetFromRow c

getApplicationID :: Word64 -> Cacheable Application
getApplicationID i c = do
    ac <- hitSegment applicationIDCache c
    hitRecordFallback i ac (sneakyGhostM applicationIDCache i db c)
    where db = liftMaybeQ (getApplicationIDP i) (noSuchID "Application" i) applicationFromRow c

getMemberApplications :: Word64 -> Cacheable [Application]
getMemberApplications i c = do
    ac <- hitSegment applicationMemberIDCache c
    hitRecordFallback i ac (sneakyGhostM applicationMemberIDCache i db c)
    where db = liftListQ (getApplicationsMemberIDP i) applicationFromRow c

getApplicationsStatus :: EvaluationStatus -> Cacheable [Application]
getApplicationsStatus e c = liftListQ (getApplicationsStatusP (evaluationStatusToVal e)) applicationFromRow c

getMemberStatusApplications :: Word64 -> EvaluationStatus -> Cacheable [Application]
getMemberStatusApplications m e c = liftListQ
                                    (getApplicationsMemberIDStatusP m (evaluationStatusToVal e))
                                    applicationFromRow
                                    c

getMetricID :: Word64 -> Cacheable Metric
getMetricID i c = do
    mc <- hitSegment metricIDCache c
    hitRecordFallback i mc (sneakyGhostM metricIDCache i db c)
    where db = liftMaybeQ (getMetricIDP i) (noSuchID "Metric" i) metricFromRow c

getMetricsActive :: Bool -> Cacheable [Metric]
getMetricsActive a c = liftListQ (getMetricsActiveP a) metricFromRow c

getReviewID :: Word64 -> Cacheable Review
getReviewID i c = do
    rc <- hitSegment reviewIDCache c
    hitRecordFallback i rc (sneakyGhostM reviewIDCache i db c)
    where db = liftMaybeQ (getReviewIDP i) (noSuchID "Review" i) reviewFromRow c

getApplicationReviews :: Word64 -> Cacheable [Review]
getApplicationReviews i c = do
    rc <- hitSegment reviewApplicationIDCache c
    hitRecordFallback i rc (sneakyGhostM reviewApplicationIDCache i db c)
    where db = liftListQ (getReviewsApplicationIDP i) reviewFromRow c

getMemberReviews :: Word64 -> Cacheable [Review]
getMemberReviews m c = liftListQ (getReviewsMemberIDP m) reviewFromRow c

getMemberApplicationReview :: Word64 -> Word64 -> Cacheable Review
getMemberApplicationReview m a c = liftMaybeQ
    (getReviewMemberIDApplicationIDP m a)
    (Nonexistent ("no such application review by member " ++ (show m) ++ " for application " ++ (show a)))
    reviewFromRow
    c

getInterviewID :: Word64 -> Cacheable Interview
getInterviewID i c = do
    ic <- hitSegment interviewIDCache c
    hitRecordFallback i ic (sneakyGhostM interviewIDCache i db c)
    where db = liftMaybeQ (getInterviewIDP i) (noSuchID "Interview" i) interviewFromRow c

getApplicationInterviews :: Word64 -> Cacheable [Interview]
getApplicationInterviews i c = do
    ic <- hitSegment interviewApplicationIDCache c
    hitRecordFallback i ic (sneakyGhostM interviewApplicationIDCache i db c)
    where db = liftListQ (getInterviewsApplicationIDP i) interviewFromRow c

getMemberInterviews :: Word64 -> Cacheable [Interview]
getMemberInterviews m c = liftListQ (getInterviewsMemberIDP m) interviewFromRow c

getMemberApplicationInterview :: Word64 -> Word64 -> Cacheable Interview
getMemberApplicationInterview m a c = liftMaybeQ
    (getInterviewMemberIDApplicationIDP m a)
    (Nonexistent ("no such application interview by member " ++ (show m) ++ " for application " ++ (show a)))
    interviewFromRow
    c

getQuestionID :: Word64 -> Cacheable Question
getQuestionID i c = do
    qc <- hitSegment questionIDCache c
    hitRecordFallback i qc (sneakyGhostM questionIDCache i db c)
    where db = liftMaybeQ (getQuestionIDP i) (noSuchID "Question" i) questionFromRow c

getQuestionsActive :: Bool -> Cacheable [Question]
getQuestionsActive a c = liftListQ (getQuestionsActiveP a) questionFromRow c

getTermID :: Word64 -> Cacheable Term
getTermID i c = do
    tc <- hitSegment termIDCache c
    hitRecordFallback i tc (sneakyGhostM termIDCache i db c)
    where db = liftMaybeQ (getTermIDP i) (noSuchID "Question" i) termFromRow c

getMemberEboards :: Word64 -> Cacheable [Eboard]
getMemberEboards i c = do
    ec <- hitSegment eboardMemberIDCache c
    hitRecordFallback i ec (sneakyGhostM eboardMemberIDCache i db c)
    where db = liftListQ (getEboardsMemberIDP i) eboardFromRow c

getCurrentEboards :: Cacheable [Eboard]
getCurrentEboards c = liftListQ getCurrentEboardsP eboardFromRow c

getMemberRooms :: Word64 -> Cacheable [Room]
getMemberRooms i c = do
    rc <- hitSegment roomMemberIDCache c
    hitRecordFallback i rc (sneakyGhostM roomMemberIDCache i db c)
    where db = liftListQ (getRoomsMemberIDP i) roomFromRow c

getRoomsRoomNumber :: T.Text -> Cacheable [Room]
getRoomsRoomNumber r c = liftListQ (getRoomsRoomNumberP r) roomFromRow c

getCurrentRooms :: Cacheable [Room]
getCurrentRooms c = liftListQ getCurrentRoomsP roomFromRow c

getQueueID :: Word64 -> Cacheable Queue
getQueueID i c = do
    qc <- hitSegment queueIDCache c
    hitRecordFallback i qc (sneakyGhostM queueIDCache i db c)
    where db = liftMaybeQ (getQueueIDP i) (noSuchID "Queue" i) queueFromRow c

getMemberQueues :: Word64 -> Cacheable [Queue]
getMemberQueues i c = do
    qc <- hitSegment queueMemberIDCache c
    hitRecordFallback i qc (sneakyGhostM queueMemberIDCache i db c)
    where db = liftListQ (getQueuesMemberIDP i) queueFromRow c

getMemberMemberships :: Word64 -> Cacheable [Membership]
getMemberMemberships i c = do
    mc <- hitSegment membershipMemberIDCache c
    hitRecordFallback i mc (sneakyGhostM membershipMemberIDCache i db c)
    where db = liftListQ (getMembershipsMemberIDP i) membershipFromRow c

getCurrentMemberships :: Cacheable [Membership]
getCurrentMemberships c = liftListQ getCurrentMembershipsP membershipFromRow c

getEventEventAttendees :: Word64 -> Cacheable [EventAttendee]
getEventEventAttendees i c = do
    ec <- hitSegment eventAttendeeEventIDCache c
    hitRecordFallback i ec (sneakyGhostM eventAttendeeMemberIDCache i db c)
    where db = liftListQ (getEventAttendeesEventIDP i) eventAttendeeFromRow c

getMemberEventAttendees :: Word64 -> Cacheable [EventAttendee]
getMemberEventAttendees i c = do
    ec <- hitSegment eventAttendeeMemberIDCache c
    hitRecordFallback i ec (sneakyGhostM eventAttendeeMemberIDCache i db c)
    where db = liftListQ (getEventAttendeesMemberIDP i) eventAttendeeFromRow c

getProjectProjectParticipants :: Word64 -> Cacheable [ProjectParticipant]
getProjectProjectParticipants i c = do
    pc <- hitSegment projectParticipantProjectIDCache c
    hitRecordFallback i pc (sneakyGhostM projectParticipantProjectIDCache i db c)
    where db = liftListQ (getProjectParticipantsProjectIDP i) projectParticipantFromRow c

getMemberProjectParticipants :: Word64 -> Cacheable [ProjectParticipant]
getMemberProjectParticipants i c = do
    pc <- hitSegment projectParticipantMemberIDCache c
    hitRecordFallback i pc (sneakyGhostM projectParticipantMemberIDCache i db c)
    where db = liftListQ (getProjectParticipantsMemberIDP i) projectParticipantFromRow c

getFreshmanProjectFreshmanProjectParticipants :: Word64 -> Cacheable [FreshmanProjectParticipant]
getFreshmanProjectFreshmanProjectParticipants i c = do
    fc <- hitSegment freshProjParticipantProjectIDCache c
    hitRecordFallback i fc (sneakyGhostM freshProjParticipantProjectIDCache i db c)
    where db = liftListQ (getFreshmanProjectParticipantsFreshmanProjectIDP i) freshmanProjectParticipantFromRow c

getEvaluationFreshmanProjectParticipants :: Word64 -> Cacheable [FreshmanProjectParticipant]
getEvaluationFreshmanProjectParticipants i c = do
    fc <- hitSegment freshProjParticipantEvaluationIDCache c
    hitRecordFallback i fc (sneakyGhostM freshProjParticipantEvaluationIDCache i db c)
    where db = liftListQ (getFreshmanProjectParticipantsEvaluationIDP i) freshmanProjectParticipantFromRow c

getFreshmanProjectFreshmanProjectEboards :: Word64 -> Cacheable [FreshmanProjectParticipant]
getFreshmanProjectFreshmanProjectEboards f c = liftListQ
                                               (getFreshmanProjectParticipantsEboardP f)
                                               freshmanProjectParticipantFromRow
                                               c

getPacketSignatures :: Word64 -> Cacheable [Signature]
getPacketSignatures i c = do
    sc <- hitSegment signaturePacketIDCache c
    hitRecordFallback i sc (sneakyGhostM signaturePacketIDCache i db c)
    where db = liftListQ (getSignaturesPacketIDP i) signatureFromRow c

getPacketSignaturesSigned :: Word64 -> Cacheable [Signature]
getPacketSignaturesSigned p c = liftListQ (getSignaturesSignedP p) signatureFromRow c

getPacketRequiredSignatures :: Word64 -> Cacheable [Signature]
getPacketRequiredSignatures p c = liftListQ (getSignaturesRequiredP p) signatureFromRow c

getMemberSignatures :: Word64 -> Cacheable [Signature]
getMemberSignatures i c = do
    sc <- hitSegment signatureMemberIDCache c
    hitRecordFallback i sc (sneakyGhostM signatureMemberIDCache i db c)
    where db = liftListQ (getSignaturesMemberIDP i) signatureFromRow c

getReviewReviewMetrics :: Word64 -> Cacheable [ReviewMetric]
getReviewReviewMetrics i c = do
    rc <- hitSegment reviewMetricReviewIDCache c
    hitRecordFallback i rc (sneakyGhostM reviewMetricReviewIDCache i db c)
    where db = liftListQ (getReviewMetricsReviewIDP i) reviewMetricFromRow c

getMetricReviewMetrics :: Word64 -> Cacheable [ReviewMetric]
getMetricReviewMetrics i c = do
    rc <- hitSegment reviewMetricMetricIDCache c
    hitRecordFallback i rc (sneakyGhostM reviewMetricMetricIDCache i db c)
    where db = liftListQ (getReviewMetricsMetricIDP i) reviewMetricFromRow c

getInterviewInterviewMetrics :: Word64 -> Cacheable [InterviewMetric]
getInterviewInterviewMetrics i c = do
    ic <- hitSegment interviewMetricInterviewIDCache c
    hitRecordFallback i ic (sneakyGhostM interviewMetricInterviewIDCache i db c)
    where db = liftListQ (getInterviewMetricsInterviewIDP i) interviewMetricFromRow c

getMetricInterviewMetrics :: Word64 -> Cacheable [InterviewMetric]
getMetricInterviewMetrics i c = do
    ic <- hitSegment interviewMetricMetricIDCache c
    hitRecordFallback i ic (sneakyGhostM interviewMetricMetricIDCache i db c)
    where db = liftListQ (getInterviewMetricsMetricIDP i) interviewMetricFromRow c

getApplicationAnswers :: Word64 -> Cacheable [Answer]
getApplicationAnswers i c = do
    ac <- hitSegment answerApplicationIDCache c
    hitRecordFallback i ac (sneakyGhostM answerApplicationIDCache i db c)
    where db = liftListQ (getAnswersApplicationIDP i) answerFromRow c

getQuestionAnswers :: Word64 -> Cacheable [Answer]
getQuestionAnswers i c = do
    ac <- hitSegment answerQuestionIDCache c
    hitRecordFallback i ac (sneakyGhostM answerQuestionIDCache i db c)
    where db = liftListQ (getAnswersQuestionIDP i) answerFromRow c

getMemberDues :: Word64 -> Cacheable [Dues]
getMemberDues i c = do
    dc <- hitSegment duesMemberIDCache c
    hitRecordFallback i dc (sneakyGhostM duesMemberIDCache i db c)
    where db = liftListQ (getDuesMemberIDP i) duesFromRow c

getTermDues :: Word64 -> Cacheable [Dues]
getTermDues i c = do
    dc <- hitSegment duesTermIDCache c
    hitRecordFallback i dc (sneakyGhostM duesTermIDCache i db c)
    where db = liftListQ (getDuesTermIDP i) duesFromRow c

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
committeeFromVal "financial" = Financial
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

committeeToVal :: Committee -> T.Text
committeeToVal Evals     = "evals"
committeeToVal RnD       = "rnd"
committeeToVal Social    = "social"
committeeToVal History   = "history"
committeeToVal OpComm    = "opcomm"
committeeToVal Imps      = "imps"
committeeToVal Financial = "financial"
committeeToVal Chairman  = "chairmam"

evaluationTypeToVal :: EvaluationType -> T.Text
evaluationTypeToVal IntroEval      = "introductory"
evaluationTypeToVal MembershipEval = "membership"

evaluationStatusToVal :: EvaluationStatus -> T.Text
evaluationStatusToVal Pending = "pending"
evaluationStatusToVal Passed  = "passed"
evaluationStatusToVal Failed  = "failed"

memberStatusToVal :: MemberStatus -> T.Text
memberStatusToVal Active     = "active"
memberStatusToVal AlumniGood = "alumni_good"
memberStatusToVal AlumniBad  = "alumni_bad"
memberStatusToVal Honorary   = "honorary"
memberStatusToVal Advisory   = "advisory"
memberStatusToVal Intro      = "introductory"
memberStatusToVal Non        = "non"

duesStatusToVal :: DuesStatus -> T.Text
duesStatusToVal Paid   = "paid"
duesStatusToVal Exempt = "evempt"

eventTypeToVal :: EventType -> T.Text
eventTypeToVal HouseMeeting     = "house"
eventTypeToVal SocialEvent      = "social"
eventTypeToVal CommitteeMeeting = "committee"
eventTypeToVal Seminar          = "seminar"
eventTypeToVal Orientation      = "orientation"

projectTypeToVal :: ProjectType -> T.Text
projectTypeToVal Major = "major"
