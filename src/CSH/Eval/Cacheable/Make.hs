{-|
Module      : CSH.Eval.Cacheable.Make
Description : Cacheable Actions to Create Objects
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.Cacheable.Make defines 'Cacheable' computations for introducing state
objects.
-}

{-# LANGUAGE OverloadedStrings #-}

module CSH.Eval.Cacheable.Make (
    -- * Object Instantiating Functions
    -- ** Member
    mkIntroMember
  , mkExtantMember
    -- ** Event
  , mkEvent
    -- ** Project
  , mkProject
    -- ** Evaluation
  , mkEvaluation
    -- ** Conditional
  , mkConditional
    -- ** FreshmanProject
  , mkFreshmanProject
    -- ** Packet
  , mkPacket
    -- ** Application
  , mkApplication
    -- ** Metric
  , mkMetric
    -- ** Review
  , mkReview
    -- ** Interview
  , mkInterview
    -- ** Question
  , mkQuestion
    -- ** Term
  , mkTerm
    -- * Context Instantiating Functions
    -- ** Eboard
  , grEboard
    -- ** Room
  , grRoom
    -- ** Queue
  , grQueue
    -- ** Membership
  , grMembership
    -- ** EventAttendee
  , grEventAttendee
    -- ** ProjectParticipant
  , grProjectParticipant
    -- ** FreshmanProjectParticipant
  , grFreshmanProjectParticipant
    -- ** Signature
  , grSignature
    -- ** ReviewMetric
  , grReviewMetric
    -- ** InterviewMetric
  , grInterviewMetric
    -- ** Answer
  , grAnswer
    -- ** Dues
  , grDues
    -- * ToRow Functions
  ) where

import Data.Word

import Data.UUID

import Data.Time

import qualified Data.ByteString as B

import qualified Data.Text as T

import CSH.Eval.Model

import CSH.Eval.DB.Statements

import CSH.Eval.Cacheable.Prim
import CSH.Eval.Cacheable.Fetch

mkIntroMember :: UUID         -- ^ Member UUID.
              -> T.Text       -- ^ Username.
              -> T.Text       -- ^ Common name.
              -> B.ByteString -- ^ Passowrd hash.
              -> B.ByteString -- ^ Passowrd salt
              -> Cacheable ()
mkIntroMember uu u cn ph ps c = do
    mid <- liftInsertSingleQ (mkIntroMemberP uu u cn ph ps) c
    let m = Member mid
                   uu
                   u
                   cn
                   Nothing
                   0
                   False
                   (getMemberEboards mid)
                   (getMemberRooms mid)
                   (getMemberMemberships mid)
                   (getMemberEvaluations mid)
                   (getMemberPackets mid)
                   (getMemberQueues mid)
                   (getMemberApplications mid)
                   (getMemberDues mid)
    singletonGhost memberIDCache mid m c

mkExtantMember :: UUID   -- ^ Member UUID.
               -> T.Text -- ^ Username.
               -> T.Text -- ^ Common name.
               -> Int    -- ^ Housing points.
               -> Bool   -- ^ On floor status.
               -> Cacheable ()
mkExtantMember uu u cn hp os c = do
    mid <- liftInsertSingleQ (mkExtantMemberP uu u cn hp os) c
    let m = Member mid
                   uu
                   u
                   cn
                   Nothing
                   hp
                   os
                   (getMemberEboards mid)
                   (getMemberRooms mid)
                   (getMemberMemberships mid)
                   (getMemberEvaluations mid)
                   (getMemberPackets mid)
                   (getMemberQueues mid)
                   (getMemberApplications mid)
                   (getMemberDues mid)
    singletonGhost memberIDCache mid m c

mkEvent :: T.Text    -- ^ Title.
        -> UTCTime   -- ^ Held time.
        -> EventType -- ^ Event type
        -> Committee -- ^ Event Committee
        -> T.Text    -- ^ Description
        -> Cacheable ()
mkEvent t h et ec d c = do
    eid <- liftInsertSingleQ (mkEventP t h (eventTypeToVal et) (committeeToVal ec) d) c
    let e = Event eid
                  t
                  h
                  et
                  ec
                  d
                  (getEventEventAttendees eid)
    singletonGhost eventIDCache eid e c

mkProject :: T.Text      -- ^ Title.
          -> T.Text      -- ^ Description.
          -> UTCTime     -- ^ Submission time.
          -> Committee   -- ^ Project committee.
          -> ProjectType -- ^ Project type.
          -> Cacheable ()
mkProject t d st pc pt c = do
    pid <- liftInsertSingleQ (mkProjectP t d st (committeeToVal pc) (projectTypeToVal pt)) c
    let p = Project pid
                    t
                    d
                    st
                    Nothing
                    pc
                    pt
                    T.empty
                    Pending
                    (getProjectProjectParticipants pid)
    singletonGhost projectIDCache pid p c

mkEvaluation :: Word64         -- ^ Member ID.
             -> UTCTime        -- ^ Evaluation deadline.
             -> EvaluationType -- ^ Evaluation type.
             -> Cacheable ()
mkEvaluation mid t et c = do
    eid <- liftInsertSingleQ (mkEvaluationP mid t (evaluationTypeToVal et)) c
    let e = Evaluation eid
                       T.empty
                       t
                       False
                       Pending
                       et
                       (getMemberID mid)
                       (getEvaluationConditionals eid)
                       (getEvaluationFreshmanProjectParticipants eid)
    singletonGhost evaluationIDCache eid e c

mkConditional :: Word64  -- ^ Evaluation ID.
              -> UTCTime -- ^ Due date.
              -> T.Text  -- ^ Description.
              -> Cacheable ()
mkConditional eid dt d c = do
    cid <- liftInsertSingleQ (mkConditionalP eid dt d) c
    let c' = Conditional cid
                         dt
                         d
                         T.empty
                         (getEvaluationID eid)
    singletonGhost conditionalIDCache cid c' c

mkFreshmanProject :: Word64 -- ^ Term ID.
                  -> Word64 -- ^ Event ID.
                  -> T.Text -- ^ Description.
                  -> Cacheable ()
mkFreshmanProject tid eid d c = do
    fpid <- liftInsertSingleQ (mkFreshmanProjectP tid eid d) c
    let fp = FreshmanProject fpid
                             d
                             (getTermID tid)
                             (getEventID eid)
                             (getFreshmanProjectFreshmanProjectParticipants fpid)
    singletonGhost freshmanProjectIDCache fpid fp c

mkPacket :: Word64  -- ^ Member ID.
         -> UTCTime -- ^ Due date.
         -> Int     -- ^ Percent required.
         -> Cacheable ()
mkPacket mid d p c = do
    pid <- liftInsertSingleQ (mkPacketP mid d p) c
    let p' = Packet pid
                   d
                   p
                   (getMemberID mid)
                   (getPacketSignatures pid)
    singletonGhost packetIDCache pid p' c

mkApplication :: Word64  -- ^ Member ID.
              -> UTCTime -- ^ Creation time.
              -> Cacheable ()
mkApplication mid t c = do
    aid <- liftInsertSingleQ (mkApplicationP mid t) c
    let a = Application aid
                        t
                        Pending
                        (getMemberID mid)
                        (getApplicationReviews aid)
                        (getApplicationInterviews aid)
                        (getApplicationAnswers aid)
    singletonGhost applicationIDCache aid a c

mkMetric :: T.Text -- ^ Description.
         -> Cacheable ()
mkMetric d c = do
    mid <- liftInsertSingleQ (mkMetricP d) c
    let m = Metric mid
                   d
                   True
    singletonGhost metricIDCache mid m c

mkReview :: Word64  -- ^ Member ID(reviewer).
         -> Word64  -- ^ Application ID.
         -> UTCTime -- ^ Start time.
         -> UTCTime -- ^ Submission time.
         -> Cacheable ()
mkReview mid aid st et c = do
    rid <- liftInsertSingleQ (mkReviewP mid aid st et) c
    let r = Review rid
                   st
                   et
                   (getMemberID mid)
                   (getApplicationID aid)
                   (getReviewReviewMetrics rid)
    singletonGhost reviewIDCache rid r c

mkInterview :: Word64  -- ^ Member ID(interviewer).
            -> Word64  -- ^ Application ID.
            -> UTCTime -- ^ Interview time.
            -> Cacheable ()
mkInterview mid aid t c = do
    iid <- liftInsertSingleQ (mkInterviewP mid aid t) c
    let i = Interview iid
                      t
                      (getMemberID mid)
                      (getApplicationID aid)
                      (getInterviewInterviewMetrics iid)
    singletonGhost interviewIDCache iid i c

mkQuestion :: T.Text -- ^ Query.
           -> Cacheable ()
mkQuestion q c = do
    qid <- liftInsertSingleQ (mkQuestionP q) c
    let q' = Question qid
                     q
                     True
    singletonGhost questionIDCache qid q' c

mkTerm :: Day -- ^ Start date.
       -> Cacheable ()
mkTerm s c = do
    tid <- liftInsertSingleQ (mkTermP s) c
    let t = Term tid
                 s
                 Nothing
    singletonGhost termIDCache tid t c

grEboard :: Word64    -- ^ Member ID.
         -> Committee -- ^ Committee.
         -> Day       -- ^ Service start date.
         -> Cacheable ()
grEboard mid cm s c = do
    liftUnitQ (grEboardP mid (committeeToVal cm) s) c
    let e = Eboard cm
                   s
                   Nothing
                   (getMemberID mid)
    appendGhost eboardMemberIDCache mid e c

grRoom :: Word64 -- ^ Member ID.
       -> T.Text -- ^ Room number.
       -> Day    -- ^ Residence start date.
       -> Cacheable ()
grRoom mid rn s c = do
    liftUnitQ (grRoomP mid rn s) c
    let r = Room rn
                 s
                 Nothing
                 (getMemberID mid)
    appendGhost roomMemberIDCache mid r c

grQueue :: Word64  -- ^ Member ID.
        -> UTCTime -- ^ Entrance time.
        -> Cacheable ()
grQueue mid t c = do
    liftUnitQ (grQueueP mid t) c
    let q = Queue t
                  Nothing
                  (getMemberID mid)
    appendGhost queueMemberIDCache mid q c

grMembership :: Word64       -- ^ Member ID.
             -> MemberStatus -- ^ Membership status.
             -> Day          -- ^ Effective date.
             -> Cacheable ()
grMembership mid ms t c = do
    liftUnitQ (grMembershipP mid (memberStatusToVal ms) t) c
    let m = Membership ms
                       t
                       Nothing
                       (getMemberID mid)
    appendGhost membershipMemberIDCache mid m c

grEventAttendee :: Word64 -- ^ Member ID.
                -> Word64 -- ^ Event ID.
                -> Bool   -- ^ Host status.
                -> Cacheable ()
grEventAttendee mid eid h c = do
    liftUnitQ (grEventAttendeeP mid eid h) c
    let e = EventAttendee h
                          (getMemberID mid)
                          (getEventID eid)
    appendGhost eventAttendeeMemberIDCache mid e c
    appendGhost eventAttendeeEventIDCache eid e c

grProjectParticipant :: Word64 -- ^ Member ID.
                     -> Word64 -- ^ Project ID.
                     -> T.Text -- ^ Description.
                     -> Cacheable ()
grProjectParticipant mid pid d c = do
    liftUnitQ (grProjectParticipantP mid pid d) c
    let p = ProjectParticipant d
                               (getMemberID mid)
                               (getProjectID pid)
    appendGhost projectParticipantMemberIDCache mid p c
    appendGhost projectParticipantProjectIDCache pid p c

grFreshmanProjectParticipant :: Word64 -- ^ Freshman Project ID.
                             -> Word64 -- ^ Evaluation ID.
                             -> Cacheable ()
grFreshmanProjectParticipant pid eid c = do
    liftUnitQ (grFreshmanProjectParticipantP pid eid) c
    let f = FreshmanProjectParticipant False
                                       Pending
                                       T.empty
                                       (getFreshmanProjectID pid)
                                       (getEvaluationID eid)
    appendGhost freshProjParticipantProjectIDCache pid f c
    appendGhost freshProjParticipantEvaluationIDCache eid f c

grSignature :: Word64 -- ^ Member ID.
            -> Word64 -- ^ Packet ID.
            -> Bool   -- ^ Required.
            -> Cacheable ()
grSignature mid pid r c = do
    liftUnitQ (grSignatureP mid pid r) c
    let s = Signature r
                      Nothing
                      (getMemberID mid)
                      (getPacketID pid)
    appendGhost signatureMemberIDCache mid s c
    appendGhost signaturePacketIDCache pid s c

grReviewMetric :: Word64 -- ^ Metric ID.
               -> Word64 -- ^ Review ID.
               -> Int    -- ^ Score.
               -> Cacheable ()
grReviewMetric mid rid s c = do
    liftUnitQ (grReviewMetricP mid rid s) c
    let r = ReviewMetric s
                         (getMetricID mid)
                         (getReviewID rid)
    appendGhost reviewMetricMetricIDCache mid r c
    appendGhost reviewMetricReviewIDCache rid r c

grInterviewMetric :: Word64 -- ^ Metric ID.
                  -> Word64 -- ^ Interview ID.
                  -> Int    -- ^ Score.
                  -> Cacheable ()
grInterviewMetric mid iid s c = do
    liftUnitQ (grInterviewMetricP mid iid s) c
    let i = InterviewMetric s
                            (getMetricID mid)
                            (getInterviewID iid)
    appendGhost interviewMetricMetricIDCache mid i c
    appendGhost interviewMetricInterviewIDCache iid i c

grAnswer :: Word64 -- ^ Application ID
         -> Word64 -- ^ Question ID
         -> T.Text -- ^ Response
         -> Cacheable ()
grAnswer aid qid r c = do
    liftUnitQ (grAnswerP aid qid r) c
    let a = Answer r
                   (getQuestionID qid)
                   (getApplicationID aid)
    appendGhost answerQuestionIDCache qid a c
    appendGhost answerApplicationIDCache aid a c

grDues :: Word64     -- ^ Term ID.
       -> Word64     -- ^ Member ID.
       -> DuesStatus -- ^ Dues status.
       -> Cacheable ()
grDues tid mid s c = do
    liftUnitQ (grDuesP tid mid (duesStatusToVal s)) c
    let d = Dues s
                 (getMemberID mid)
                 (getTermID tid)
    appendGhost duesMemberIDCache mid d c
    appendGhost duesTermIDCache tid d c
