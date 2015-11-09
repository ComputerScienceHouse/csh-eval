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
    -- * FromRow Functions
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
    -- * FromVal Functions
  , committeeFromVal
  , evaluationTypeFromVal
  , evaluationStatusFromVal
  , memberStatusFromVal
  , duesStatusFromVal
  , eventTypeFromVal
  , projectTypeFromVal
    -- * ToVal Functions
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

-- | Fetch a 'Member' by ID.
getMemberID :: Word64 -- ^ Member ID.
            -> Cacheable Member
getMemberID i c = do
    mc <- hitSegment memberIDCache c
    hitRecordFallback i mc (sneakyGhostM memberIDCache i db c)
    where db = liftMaybeQ (getMemberIDP i) (noSuchID "Member" i) memberFromRow c

-- | Fetch a 'Member' by UUID.
getMemberUUID :: UUID -- ^ Member UUID.
              -> Cacheable Member
getMemberUUID u c = liftMaybeQ (getMemberUUIDP u) (noSuchThing "member" "UUID" u) memberFromRow c

-- | Fetch a 'Member' by username.
getMemberUsername :: T.Text -- ^ Member username.
                  -> Cacheable Member
getMemberUsername u c = liftMaybeQ (getMemberUsernameP u) (noSuchThing "member" "username" u) memberFromRow c

-- | Fetch the 'Member'(s) with the provided common name.
getMembersCommonname :: T.Text -- ^ Member common name.
                     -> Cacheable [Member]
getMembersCommonname cn c = liftListQ (getMembersCommonnameP cn) memberFromRow c

-- | Fetch all on-floor or off-floor 'Member's.
getMembersOnfloor :: Bool -- ^ Onfloor status.
                  -> Cacheable [Member]
getMembersOnfloor o c = liftListQ (getMembersOnfloorP o) memberFromRow c

-- | Fetch an 'Event' by ID.
getEventID :: Word64 -- ^ Event ID.
           -> Cacheable Event
getEventID i c = do
    ec <- hitSegment eventIDCache c
    hitRecordFallback i ec (sneakyGhostM eventIDCache i db c)
    where db = liftMaybeQ (getEventIDP i) (noSuchID "Event" i) eventFromRow c

-- | Fetch the 'Event'(s) with the provided title.
getEventsTitle :: T.Text -- ^ Event title.
               -> Cacheable [Event]
getEventsTitle t c = liftListQ (getEventsTitleP t) eventFromRow c

-- | Fetch a 'Project' by ID.
getProjectID :: Word64 -- ^ Project ID.
             -> Cacheable Project
getProjectID i c = do
    pc <- hitSegment projectIDCache c
    hitRecordFallback i pc (sneakyGhostM projectIDCache i db c)
    where db = liftMaybeQ (getProjectIDP i) (noSuchID "Project" i) projectFromRow c

-- | Fetch the 'Project'(s) with the provided title.
getProjectsTitle :: T.Text -- ^ Project title.
                 -> Cacheable [Project]
getProjectsTitle t c = liftListQ (getProjectsTitleP t) projectFromRow c

-- | Fetch the 'Project'(s) with the provided 'EvlauationStatus'.
getProjectsStatus :: EvaluationStatus -- ^ Project evaluation status.
                  -> Cacheable [Project]
getProjectsStatus e c = liftListQ (getProjectsStatusP (evaluationStatusToVal e)) projectFromRow c

-- | Fetch an 'Evaluation' by ID.
getEvaluationID :: Word64 -- ^ Evaluation ID.
                -> Cacheable Evaluation
getEvaluationID i c = do
    ec <- hitSegment evaluationIDCache c
    hitRecordFallback i ec (sneakyGhostM evaluationIDCache i db c)
    where db = liftMaybeQ (getEvaluationIDP i) (noSuchID "Evaluation" i) evaluationFromRow c

-- | Fetch the 'Evaluation's associated with a provided 'Member'.
getMemberEvaluations :: Word64 -- ^ Member ID.
                     -> Cacheable [Evaluation]
getMemberEvaluations i c = do
    ec <- hitSegment evaluationMemberIDCache c
    hitRecordFallback i ec (sneakyGhostM evaluationMemberIDCache i db c)
    where db = liftListQ (getEvaluationsMemberIDP i) evaluationFromRow c

-- | Fetch the 'Evaluation's with a provided 'EvaluationStatus'.
getEvaluationsStatus :: EvaluationStatus -- ^ Evaluation status.
                     -> Cacheable [Evaluation]
getEvaluationsStatus e c = liftListQ (getEvaluationsStatusP (evaluationStatusToVal e)) evaluationFromRow c

-- | Fetch a 'Conditional' by ID.
getConditionalID :: Word64 -- ^ Conditional ID.
                 -> Cacheable Conditional
getConditionalID i c = do
    cc <- hitSegment conditionalIDCache c
    hitRecordFallback i cc (sneakyGhostM conditionalIDCache i db c)
    where db = liftMaybeQ (getConditionalIDP i) (noSuchID "Conditional" i) conditionalFromRow c

-- | Fetch the 'Conditional'(s) associated with a provided 'Evaluation'.
getEvaluationConditionals :: Word64 -- ^ Evaluation ID.
                          -> Cacheable [Conditional]
getEvaluationConditionals i c = do
    cc <- hitSegment conditionalEvaluationIDCache c
    hitRecordFallback i cc (sneakyGhostM conditionalEvaluationIDCache i db c)
    where db = liftListQ (getConditionalEvaluationIDP i) conditionalFromRow c

-- | Fetch a 'FreshmanProject' by ID.
getFreshmanProjectID :: Word64 -- ^ Freshman project ID.
                     -> Cacheable FreshmanProject
getFreshmanProjectID i c = do
    fc <- hitSegment freshmanProjectIDCache c
    hitRecordFallback i fc (sneakyGhostM freshmanProjectIDCache i db c)
    where db = liftMaybeQ (getFreshmanProjectIDP i) (noSuchID "FreshmanProject" i) freshmanProjectFromRow c

-- | Fetch a 'Packet' by ID.
getPacketID :: Word64 -- ^ Packet ID.
            -> Cacheable Packet
getPacketID i c = do
    pc <- hitSegment packetIDCache c
    hitRecordFallback i pc (sneakyGhostM packetIDCache i db c)
    where db = liftMaybeQ (getPacketIDP i) (noSuchID "Packet" i) packetFromRow c

-- | Fetch the 'Packet'(s) associated with a provided 'Member'.
getMemberPackets :: Word64 -- ^ Member ID.
                 -> Cacheable [Packet]
getMemberPackets i c = do
    pc <- hitSegment packetMemberIDCache c
    hitRecordFallback i pc (sneakyGhostM packetMemberIDCache i db c)
    where db = liftListQ (getPacketsMemberIDP i) packetFromRow c

-- | Fetch an 'Application' by ID.
getApplicationID :: Word64 -- ^ Application ID.
                 -> Cacheable Application
getApplicationID i c = do
    ac <- hitSegment applicationIDCache c
    hitRecordFallback i ac (sneakyGhostM applicationIDCache i db c)
    where db = liftMaybeQ (getApplicationIDP i) (noSuchID "Application" i) applicationFromRow c

-- | Fetch the 'Application'(s) associated with a provided 'Member'.
getMemberApplications :: Word64 -- ^ Member ID.
                      -> Cacheable [Application]
getMemberApplications i c = do
    ac <- hitSegment applicationMemberIDCache c
    hitRecordFallback i ac (sneakyGhostM applicationMemberIDCache i db c)
    where db = liftListQ (getApplicationsMemberIDP i) applicationFromRow c

-- | Fetch the 'Appliation's with the provided 'EvaluationStatus'.
getApplicationsStatus :: EvaluationStatus -- ^ Evaluation status.
                      -> Cacheable [Application]
getApplicationsStatus e c = liftListQ (getApplicationsStatusP (evaluationStatusToVal e)) applicationFromRow c

-- | Fetch the 'Application'(s) associated with a provided 'Member' and of a
--   provided 'EvaluationStatus'.
getMemberStatusApplications :: Word64           -- ^ Member ID.
                            -> EvaluationStatus -- ^ Evaluation status.
                            -> Cacheable [Application]
getMemberStatusApplications m e c = liftListQ
                                    (getApplicationsMemberIDStatusP m (evaluationStatusToVal e))
                                    applicationFromRow
                                    c

-- | Fetch a 'Metric' by ID.
getMetricID :: Word64 -- ^ Metric ID.
            -> Cacheable Metric
getMetricID i c = do
    mc <- hitSegment metricIDCache c
    hitRecordFallback i mc (sneakyGhostM metricIDCache i db c)
    where db = liftMaybeQ (getMetricIDP i) (noSuchID "Metric" i) metricFromRow c

-- | Fetch all active or inactive 'Metric's.
getMetricsActive :: Bool -- Whether or not a metric is active.
                 -> Cacheable [Metric]
getMetricsActive a c = liftListQ (getMetricsActiveP a) metricFromRow c

-- | Fetch a 'Review' by ID.
getReviewID :: Word64 -- ^ Review ID.
            -> Cacheable Review
getReviewID i c = do
    rc <- hitSegment reviewIDCache c
    hitRecordFallback i rc (sneakyGhostM reviewIDCache i db c)
    where db = liftMaybeQ (getReviewIDP i) (noSuchID "Review" i) reviewFromRow c

-- | Fetch the 'Review's associated with a provided 'Application'.
getApplicationReviews :: Word64 -- ^ Application ID.
                      -> Cacheable [Review]
getApplicationReviews i c = do
    rc <- hitSegment reviewApplicationIDCache c
    hitRecordFallback i rc (sneakyGhostM reviewApplicationIDCache i db c)
    where db = liftListQ (getReviewsApplicationIDP i) reviewFromRow c

-- | Fetch the 'Review's written by a provided 'Member'.
getMemberReviews :: Word64 -- ^ Member ID.
                 -> Cacheable [Review]
getMemberReviews m c = liftListQ (getReviewsMemberIDP m) reviewFromRow c

-- | Fetch a particular 'Member's review of a particular 'Application'.
getMemberApplicationReview :: Word64 -- ^ Member ID.
                           -> Word64 -- ^ Application ID.
                           -> Cacheable Review
getMemberApplicationReview m a c = liftMaybeQ
    (getReviewMemberIDApplicationIDP m a)
    (Nonexistent ("no such application review by member " ++ (show m) ++ " for application " ++ (show a)))
    reviewFromRow
    c

-- | Fetch an 'Interview' by ID.
getInterviewID :: Word64 -- ^ Interview ID.
               -> Cacheable Interview
getInterviewID i c = do
    ic <- hitSegment interviewIDCache c
    hitRecordFallback i ic (sneakyGhostM interviewIDCache i db c)
    where db = liftMaybeQ (getInterviewIDP i) (noSuchID "Interview" i) interviewFromRow c

-- | Fetch the 'Interview's associated with a provided 'Applicatoin'.
getApplicationInterviews :: Word64 -- ^ Application ID.
                         -> Cacheable [Interview]
getApplicationInterviews i c = do
    ic <- hitSegment interviewApplicationIDCache c
    hitRecordFallback i ic (sneakyGhostM interviewApplicationIDCache i db c)
    where db = liftListQ (getInterviewsApplicationIDP i) interviewFromRow c

-- | Fetch the 'Interview's conducted by a provided 'Member'.
getMemberInterviews :: Word64 -- ^ Member ID.
                    -> Cacheable [Interview]
getMemberInterviews m c = liftListQ (getInterviewsMemberIDP m) interviewFromRow c

-- | Fetch a particular 'Member's review of a particular 'Application'.
getMemberApplicationInterview :: Word64 -- ^ Member ID.
                              -> Word64 -- ^ Application ID.
                              -> Cacheable Interview
getMemberApplicationInterview m a c = liftMaybeQ
    (getInterviewMemberIDApplicationIDP m a)
    (Nonexistent ("no such application interview by member " ++ (show m) ++ " for application " ++ (show a)))
    interviewFromRow
    c

-- | Fetch a 'Question' by ID.
getQuestionID :: Word64 -- ^ Question ID.
              -> Cacheable Question
getQuestionID i c = do
    qc <- hitSegment questionIDCache c
    hitRecordFallback i qc (sneakyGhostM questionIDCache i db c)
    where db = liftMaybeQ (getQuestionIDP i) (noSuchID "Question" i) questionFromRow c

-- | Fetch all active or inactive 'Question's.
getQuestionsActive :: Bool -- ^ Whether or not a question is active.
                   -> Cacheable [Question]
getQuestionsActive a c = liftListQ (getQuestionsActiveP a) questionFromRow c

-- | Fetch a 'Term' by ID.
getTermID :: Word64 -- ^ Term ID.
          -> Cacheable Term
getTermID i c = do
    tc <- hitSegment termIDCache c
    hitRecordFallback i tc (sneakyGhostM termIDCache i db c)
    where db = liftMaybeQ (getTermIDP i) (noSuchID "Question" i) termFromRow c

-- | Fetch a provided 'Member's 'Eboard' terms.
getMemberEboards :: Word64 -- ^ Member ID.
                 -> Cacheable [Eboard]
getMemberEboards i c = do
    ec <- hitSegment eboardMemberIDCache c
    hitRecordFallback i ec (sneakyGhostM eboardMemberIDCache i db c)
    where db = liftListQ (getEboardsMemberIDP i) eboardFromRow c

-- | Fetch the current 'Eboard's terms.
getCurrentEboards :: Cacheable [Eboard]
getCurrentEboards c = liftListQ getCurrentEboardsP eboardFromRow c

-- | Fetch the 'Room' residences associated with a provided 'Member.
getMemberRooms :: Word64 -- ^ Member ID.
               -> Cacheable [Room]
getMemberRooms i c = do
    rc <- hitSegment roomMemberIDCache c
    hitRecordFallback i rc (sneakyGhostM roomMemberIDCache i db c)
    where db = liftListQ (getRoomsMemberIDP i) roomFromRow c

-- | Fetch the 'Room' residences associated with a provided room number.
getRoomsRoomNumber :: T.Text -- ^ Room number.
                   -> Cacheable [Room]
getRoomsRoomNumber r c = liftListQ (getRoomsRoomNumberP r) roomFromRow c

-- | Fetch the 'Room' residences of all members currently living on floor.
getCurrentRooms :: Cacheable [Room]
getCurrentRooms c = liftListQ getCurrentRoomsP roomFromRow c

-- | Fetch a 'Queue' by ID.
getQueueID :: Word64 -- ^ Queue ID.
           -> Cacheable Queue
getQueueID i c = do
    qc <- hitSegment queueIDCache c
    hitRecordFallback i qc (sneakyGhostM queueIDCache i db c)
    where db = liftMaybeQ (getQueueIDP i) (noSuchID "Queue" i) queueFromRow c

-- | Fetch a provided 'Member's 'Queue' instantiations.
getMemberQueues :: Word64 -- ^ Member ID.
                -> Cacheable [Queue]
getMemberQueues i c = do
    qc <- hitSegment queueMemberIDCache c
    hitRecordFallback i qc (sneakyGhostM queueMemberIDCache i db c)
    where db = liftListQ (getQueuesMemberIDP i) queueFromRow c

-- | Fetch the 'Membership'(s) associated with a provided 'Member'.
getMemberMemberships :: Word64 -- ^ Member ID.
                     -> Cacheable [Membership]
getMemberMemberships i c = do
    mc <- hitSegment membershipMemberIDCache c
    hitRecordFallback i mc (sneakyGhostM membershipMemberIDCache i db c)
    where db = liftListQ (getMembershipsMemberIDP i) membershipFromRow c

-- | Fetch all currently relevant 'Membership's.
getCurrentMemberships :: Cacheable [Membership]
getCurrentMemberships c = liftListQ getCurrentMembershipsP membershipFromRow c

-- | Fetch the 'EventAttendee's who attended a provided 'Event'.
getEventEventAttendees :: Word64 -- ^ Event ID.
                       -> Cacheable [EventAttendee]
getEventEventAttendees i c = do
    ec <- hitSegment eventAttendeeEventIDCache c
    hitRecordFallback i ec (sneakyGhostM eventAttendeeMemberIDCache i db c)
    where db = liftListQ (getEventAttendeesEventIDP i) eventAttendeeFromRow c

-- | Fetch the events a 'Member has attended.
getMemberEventAttendees :: Word64 -- ^ Member ID.
                        -> Cacheable [EventAttendee]
getMemberEventAttendees i c = do
    ec <- hitSegment eventAttendeeMemberIDCache c
    hitRecordFallback i ec (sneakyGhostM eventAttendeeMemberIDCache i db c)
    where db = liftListQ (getEventAttendeesMemberIDP i) eventAttendeeFromRow c

-- | Fetch a 'Project's 'ProjectParticipant's.
getProjectProjectParticipants :: Word64 -- ^ Project ID.
                              -> Cacheable [ProjectParticipant]
getProjectProjectParticipants i c = do
    pc <- hitSegment projectParticipantProjectIDCache c
    hitRecordFallback i pc (sneakyGhostM projectParticipantProjectIDCache i db c)
    where db = liftListQ (getProjectParticipantsProjectIDP i) projectParticipantFromRow c

-- | Fetch the 'Project's a 'Member' has participated in.
getMemberProjectParticipants :: Word64 -- ^ Member ID.
                             -> Cacheable [ProjectParticipant]
getMemberProjectParticipants i c = do
    pc <- hitSegment projectParticipantMemberIDCache c
    hitRecordFallback i pc (sneakyGhostM projectParticipantMemberIDCache i db c)
    where db = liftListQ (getProjectParticipantsMemberIDP i) projectParticipantFromRow c

-- | Fetch a 'FreshmanProject's 'FreshmanProjectParticipant's.
getFreshmanProjectFreshmanProjectParticipants :: Word64 -- ^ Freshman project ID.
                                              -> Cacheable [FreshmanProjectParticipant]
getFreshmanProjectFreshmanProjectParticipants i c = do
    fc <- hitSegment freshProjParticipantProjectIDCache c
    hitRecordFallback i fc (sneakyGhostM freshProjParticipantProjectIDCache i db c)
    where db = liftListQ (getFreshmanProjectParticipantsFreshmanProjectIDP i) freshmanProjectParticipantFromRow c

-- | Fetch the 'FreshmanProjectParticipant'(s) associated with a provided
--   'Evaluation'.
getEvaluationFreshmanProjectParticipants :: Word64 -- ^ Evaluation ID.
                                         -> Cacheable [FreshmanProjectParticipant]
getEvaluationFreshmanProjectParticipants i c = do
    fc <- hitSegment freshProjParticipantEvaluationIDCache c
    hitRecordFallback i fc (sneakyGhostM freshProjParticipantEvaluationIDCache i db c)
    where db = liftListQ (getFreshmanProjectParticipantsEvaluationIDP i) freshmanProjectParticipantFromRow c

-- | Fetch a 'FreshmanProject's Eboard.
getFreshmanProjectFreshmanProjectEboards :: Word64 -- ^ Freshman Project.
                                         -> Cacheable [FreshmanProjectParticipant]
getFreshmanProjectFreshmanProjectEboards f c = liftListQ
                                               (getFreshmanProjectParticipantsEboardP f)
                                               freshmanProjectParticipantFromRow
                                               c

-- | Fetch a 'Packet's 'Signature's.
getPacketSignatures :: Word64 -- ^ Packet ID.
                    -> Cacheable [Signature]
getPacketSignatures i c = do
    sc <- hitSegment signaturePacketIDCache c
    hitRecordFallback i sc (sneakyGhostM signaturePacketIDCache i db c)
    where db = liftListQ (getSignaturesPacketIDP i) signatureFromRow c

-- | Fetch a 'Packet's signed 'Signatures'.
getPacketSignaturesSigned :: Word64 -- ^ Packet ID.
                          -> Cacheable [Signature]
getPacketSignaturesSigned p c = liftListQ (getSignaturesSignedP p) signatureFromRow c

-- | Fetch a 'Packet's required 'Signature's.
getPacketRequiredSignatures :: Word64 -- ^ Packet ID.
                            -> Cacheable [Signature]
getPacketRequiredSignatures p c = liftListQ (getSignaturesRequiredP p) signatureFromRow c

-- | Fetch the 'Signature's provided by a particular 'Member'.
getMemberSignatures :: Word64 -- ^ MemberID.
                    -> Cacheable [Signature]
getMemberSignatures i c = do
    sc <- hitSegment signatureMemberIDCache c
    hitRecordFallback i sc (sneakyGhostM signatureMemberIDCache i db c)
    where db = liftListQ (getSignaturesMemberIDP i) signatureFromRow c

-- | Fetch a 'Review's 'ReviewMetric's.
getReviewReviewMetrics :: Word64 -- ^ Review ID.
                       -> Cacheable [ReviewMetric]
getReviewReviewMetrics i c = do
    rc <- hitSegment reviewMetricReviewIDCache c
    hitRecordFallback i rc (sneakyGhostM reviewMetricReviewIDCache i db c)
    where db = liftListQ (getReviewMetricsReviewIDP i) reviewMetricFromRow c

-- | Fetch a 'Metric's lifetime 'ReviewMetric's.
getMetricReviewMetrics :: Word64 -- ^ Metric ID.
                       -> Cacheable [ReviewMetric]
getMetricReviewMetrics i c = do
    rc <- hitSegment reviewMetricMetricIDCache c
    hitRecordFallback i rc (sneakyGhostM reviewMetricMetricIDCache i db c)
    where db = liftListQ (getReviewMetricsMetricIDP i) reviewMetricFromRow c

-- | Fetch an 'Interview's 'InterviewMetric's.
getInterviewInterviewMetrics :: Word64 -- ^ Interview ID.
                             -> Cacheable [InterviewMetric]
getInterviewInterviewMetrics i c = do
    ic <- hitSegment interviewMetricInterviewIDCache c
    hitRecordFallback i ic (sneakyGhostM interviewMetricInterviewIDCache i db c)
    where db = liftListQ (getInterviewMetricsInterviewIDP i) interviewMetricFromRow c

-- | Fetch a 'Metric's lifetime 'InterviewMetric's.
getMetricInterviewMetrics :: Word64 -- ^ Metric ID.
                          -> Cacheable [InterviewMetric]
getMetricInterviewMetrics i c = do
    ic <- hitSegment interviewMetricMetricIDCache c
    hitRecordFallback i ic (sneakyGhostM interviewMetricMetricIDCache i db c)
    where db = liftListQ (getInterviewMetricsMetricIDP i) interviewMetricFromRow c

-- | Fetch the 'Answer's to an 'Application's 'Question's.
getApplicationAnswers :: Word64 -- ^ Application ID.
                      -> Cacheable [Answer]
getApplicationAnswers i c = do
    ac <- hitSegment answerApplicationIDCache c
    hitRecordFallback i ac (sneakyGhostM answerApplicationIDCache i db c)
    where db = liftListQ (getAnswersApplicationIDP i) answerFromRow c

-- | Fetch a 'Question's lifetime 'Answer's.
getQuestionAnswers :: Word64 -> Cacheable [Answer]
getQuestionAnswers i c = do
    ac <- hitSegment answerQuestionIDCache c
    hitRecordFallback i ac (sneakyGhostM answerQuestionIDCache i db c)
    where db = liftListQ (getAnswersQuestionIDP i) answerFromRow c

-- | Fetch a 'Member's 'Due's.
getMemberDues :: Word64 -- ^ Member ID.
              -> Cacheable [Dues]
getMemberDues i c = do
    dc <- hitSegment duesMemberIDCache c
    hitRecordFallback i dc (sneakyGhostM duesMemberIDCache i db c)
    where db = liftListQ (getDuesMemberIDP i) duesFromRow c

-- | Fetch a 'Term's 'Due's.
getTermDues :: Word64 -- ^ Term ID.
            -> Cacheable [Dues]
getTermDues i c = do
    dc <- hitSegment duesTermIDCache c
    hitRecordFallback i dc (sneakyGhostM duesTermIDCache i db c)
    where db = liftListQ (getDuesTermIDP i) duesFromRow c

memberFromRow :: (Word64, UUID, T.Text, T.Text, Maybe B.ByteString, Maybe B.ByteString, Int, Bool) -> Member
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

packetFromRow :: (Word64, Word64, UTCTime, Int) -> Packet
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

roomFromRow :: (Word64, T.Text, Day, Maybe Day) -> Room
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

committeeFromVal :: T.Text -> Committee
committeeFromVal "evals"     = Evals
committeeFromVal "rnd"       = RnD
committeeFromVal "social"    = Social
committeeFromVal "history"   = History
committeeFromVal "opcomm"    = OpComm
committeeFromVal "imps"      = Imps
committeeFromVal "financial" = Financial
committeeFromVal "chairmam"  = Chairman

evaluationTypeFromVal :: T.Text -> EvaluationType
evaluationTypeFromVal "introductory" = IntroEval
evaluationTypeFromVal "membership"   = MembershipEval

evaluationStatusFromVal :: T.Text -> EvaluationStatus
evaluationStatusFromVal "pending" = Pending
evaluationStatusFromVal "passed"  = Passed
evaluationStatusFromVal "failed"  = Failed

memberStatusFromVal :: T.Text -> MemberStatus
memberStatusFromVal "active"       = Active
memberStatusFromVal "alumni_good"  = AlumniGood
memberStatusFromVal "alumni_bad"   = AlumniBad
memberStatusFromVal "honorary"     = Honorary
memberStatusFromVal "advisory"     = Advisory
memberStatusFromVal "introductory" = Intro
memberStatusFromVal "non"          = Non

duesStatusFromVal :: T.Text -> DuesStatus
duesStatusFromVal "paid"   = Paid
duesStatusFromVal "evempt" = Exempt

eventTypeFromVal :: T.Text -> EventType
eventTypeFromVal "house"       = HouseMeeting
eventTypeFromVal "social"      = SocialEvent
eventTypeFromVal "committee"   = CommitteeMeeting
eventTypeFromVal "seminar"     = Seminar
eventTypeFromVal "orientation" = Orientation

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
