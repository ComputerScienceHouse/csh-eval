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

grEboard = undefined

grRoom = undefined

grQueue = undefined

grMembership = undefined

grEventAttendee = undefined

grProjectParticipant = undefined

grFreshmanProjectParticipant = undefined

grSignature = undefined

grReviewMetric = undefined

grInterviewMetric = undefined

grAnswer = undefined

grDues = undefined
