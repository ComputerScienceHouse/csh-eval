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
    -- ** FreshmanProject
  , getFreshmanProjectID
    -- ** Packet
  , getPacketID
  , getMemberPackets
    -- ** Queue
  , getQueueID
  , getMemberQueues
    -- ** Application
  , getApplicationID
  , getMemberApplications
    -- ** Metric
  , getMetricID
    -- ** Review
  , getReviewID
    -- ** Interview
  , getInterviewID
    -- ** Question
  , getQuestionID
    -- ** Term
  , getTermID
    -- * Context Fetching Functions
    -- ** Eboard
  , getMemberEboards
    -- ** Room
  , getMemberRooms
    -- ** Membership
  , getMemberMemberships
    -- ** EventAttendee
  , getEventEventAttendees
    -- ** ProjectParticipant
  , getProjectProjectParticipants
    -- ** FreshmanProjectParticipant
    -- ** Signature
    -- ** ReviewMetric
    -- ** InterviewMetric
    -- ** Answer
    -- ** Dues
  , getMemberDues
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
    -- * FromVal Functions (Probably shouldn't be exported...)
  , eventTypeFromVal
  , committeeFromVal
  , projectTypeFromVal
  , evaluationStatusFromVal
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
    m  <- hitRecordFallback i mc db
    singletonGhost memberIDCache i m c
    right m
    where db = maybeFallback (getMemberIDP i) (noSuchID "Member" i) memberFromRow c

getEventID :: Word64 -> Cacheable Event
getEventID i c = do
    ec <- hitSegment eventIDCache c
    e  <- hitRecordFallback i ec db
    singletonGhost eventIDCache i e c
    right e
    where db = maybeFallback (getEventIDP i) (noSuchID "Event" i) eventFromRow c

getProjectID :: Word64 -> Cacheable Project
getProjectID i c = do
    pc <- hitSegment projectIDCache c
    p  <- hitRecordFallback i pc db
    singletonGhost projectIDCache i p c
    right p
    where db = maybeFallback (getProjectIDP i) (noSuchID "Project" i) projectFromRow c

getEvaluationID :: Word64 -> Cacheable Evaluation
getEvaluationID i c = do
    ec <- hitSegment evaluationIDCache c
    e  <- hitRecordFallback i ec db
    singletonGhost evaluationIDCache i e c
    right e
    where db = maybeFallback (getEvaluationIDP i) (noSuchID "Evaluation" i) evaluationFromRow c

getMemberEvaluations :: Word64 -> Cacheable [Evaluation]
getMemberEvaluations = undefined

getConditionalID :: Word64 -> Cacheable Conditional
getConditionalID i c = do
    cc <- hitSegment conditionalIDCache c
    c' <- hitRecordFallback i cc db
    singletonGhost conditionalIDCache i c' c
    right c'
    where db = maybeFallback (getConditionalIDP i) (noSuchID "Conditional" i) conditionalFromRow c

getFreshmanProjectID :: Word64 -> Cacheable FreshmanProject
getFreshmanProjectID i c = do
    fc <- hitSegment freshmanProjectIDCache c
    f  <- hitRecordFallback i fc db
    singletonGhost freshmanProjectIDCache i f c
    right f
    where db = maybeFallback (getFreshmanProjectIDP i) (noSuchID "FreshmanProject" i) freshmanProjectFromRow c

getPacketID :: Word64 -> Cacheable Packet
getPacketID i c = do
    pc <- hitSegment packetIDCache c
    p  <- hitRecordFallback i pc db
    singletonGhost packetIDCache i p c
    right p
    where db = maybeFallback (getPacketIDP i) (noSuchID "Packet" i) packetFromRow c

getMemberPackets :: Word64 -> Cacheable [Packet]
getMemberPackets = undefined

getQueueID :: Word64 -> Cacheable Queue
getQueueID i c = do
    qc <- hitSegment queueIDCache c
    q  <- hitRecordFallback i qc db
    singletonGhost queueIDCache i q c
    right q
    where db = maybeFallback (getQueueIDP i) (noSuchID "Queue" i) queueFromRow c

getMemberQueues :: Word64 -> Cacheable [Queue]
getMemberQueues = undefined

getApplicationID :: Word64 -> Cacheable Application
getApplicationID i c = do
    ac <- hitSegment applicationIDCache c
    a  <- hitRecordFallback i ac db
    singletonGhost applicationIDCache i a c
    right a
    where db = maybeFallback (getApplicationIDP i) (noSuchID "Application" i) applicationFromRow c

getMemberApplications :: Word64 -> Cacheable [Application]
getMemberApplications = undefined

getMetricID :: Word64 -> Cacheable Metric
getMetricID i c = do
    mc <- hitSegment metricIDCache c
    m  <- hitRecordFallback i mc db
    singletonGhost metricIDCache i m c
    right m
    where db = maybeFallback (getMetricIDP i) (noSuchID "Metric" i) metricFromRow c

getReviewID :: Word64 -> Cacheable Review
getReviewID i c = do
    rc <- hitSegment reviewIDCache c
    r <- hitRecordFallback i rc db
    singletonGhost reviewIDCache i r c
    right r
    where db = maybeFallback (getReviewIDP i) (noSuchID "Review" i) reviewFromRow c

getInterviewID :: Word64 -> Cacheable Interview
getInterviewID i c = do
    ic <- hitSegment interviewIDCache c
    i' <- hitRecordFallback i ic db
    singletonGhost interviewIDCache i i' c
    right i'
    where db = maybeFallback (getInterviewIDP i) (noSuchID "Interview" i) interviewFromRow c

getQuestionID :: Word64 -> Cacheable Question
getQuestionID i c = do
    qc <- hitSegment questionIDCache c
    q  <- hitRecordFallback i qc db
    singletonGhost questionIDCache i q c
    right q
    where db = maybeFallback (getQuestionIDP i) (noSuchID "Question" i) questionFromRow c

getTermID :: Word64 -> Cacheable Term
getTermID i c = do
    tc <- hitSegment termIDCache c
    t  <- hitRecordFallback i tc db
    singletonGhost termIDCache i t c
    right t
    where db = maybeFallback (getTermIDP i) (noSuchID "Question" i) termFromRow c

getMemberEboards :: Word64 -> Cacheable [Eboard]
getMemberEboards = undefined

getMemberRooms :: Word64 -> Cacheable [Room]
getMemberRooms = undefined

getMemberMemberships :: Word64 -> Cacheable [Membership]
getMemberMemberships = undefined

getEventEventAttendees :: Word64 -> Cacheable [EventAttendee]
getEventEventAttendees = undefined

getProjectProjectParticipants :: Word64 -> Cacheable [ProjectParticipant]
getProjectProjectParticipants = undefined

getMemberDues :: Word64 -> Cacheable [Dues]
getMemberDues = undefined

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
evaluationFromRow = undefined

conditionalFromRow :: (Word64, Word64, UTCTime, T.Text, T.Text) -> Conditional
conditionalFromRow = undefined

freshmanProjectFromRow :: (Word64, T.Text, Word64, Word64) -> FreshmanProject
freshmanProjectFromRow = undefined

packetFromRow :: (Word64, Word64, Day, Int) -> Packet
packetFromRow = undefined

queueFromRow :: (Word64, UTCTime, UTCTime) -> Queue
queueFromRow = undefined

applicationFromRow :: (Word64, Word64, UTCTime, T.Text) -> Application
applicationFromRow = undefined

metricFromRow :: (Word64, T.Text, Bool) -> Metric
metricFromRow = undefined

reviewFromRow :: (Word64, Word64, Word64, UTCTime, UTCTime) -> Review
reviewFromRow = undefined

interviewFromRow :: (Word64, Word64, Word64, UTCTime) -> Interview
interviewFromRow = undefined

questionFromRow :: (Word64, Bool, T.Text) -> Question
questionFromRow = undefined

termFromRow :: (Word64, Day, Maybe Day) -> Term
termFromRow = undefined

-- | This function is partial! Explain why...
eventTypeFromVal :: T.Text -> EventType
eventTypeFromVal = undefined

-- | This function is partial! Explain why...
committeeFromVal :: T.Text -> Committee
committeeFromVal = undefined

-- | This function is partial! Explain why...
projectTypeFromVal :: T.Text -> ProjectType
projectTypeFromVal = undefined

-- | This function is partial! Explain why...
evaluationStatusFromVal :: T.Text -> EvaluationStatus
evaluationStatusFromVal = undefined
