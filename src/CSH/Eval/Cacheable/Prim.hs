{-|
Module      : CSH.Eval.Cacheable.Prim
Description : Primitive Cacheable Operations for Business Logic Coupling
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.Cacheable.Prim defines primitive operations in the 'CacheM' and
'Cacheable' monads.
-}

{-# LANGUAGE FlexibleContexts #-}

module CSH.Eval.Cacheable.Prim (
    -- * Cache Initialization and Release
    initCache
  , releaseCache
    -- * Atomic Reads
  , hitSegment
  , hitRecordFallback
    -- * Fallback Combinators
  , liftMaybeQ
  , liftListQ
    -- * Error Reporting
  , noSuchID
  , noSuchThing
    -- * Ghosts
  , reaper
  , singletonGhost
  , sneakyGhostM
  , sneakyGhostC
  ) where

import Control.Applicative ((<$>), (<*>))

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Exception

import Control.Monad.Trans.Control
import Control.Monad.Trans.Either

import Control.Monad.IO.Class (liftIO)

import Data.Maybe (fromMaybe)

import qualified Data.Map as M

import Data.Word

import Hasql
import Hasql.Postgres

import CSH.Eval.Model

-- | Initialize a 'Cache', including the enclosed 'Pool'.
initCache :: Settings
          -> PoolSettings
          -> IO Cache
initCache cs ps = let newIDCache = newMVar M.empty
    in Cache <$>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       newIDCache <*>
       acquirePool cs ps

-- | Release the 'Pool' enclosed in a 'Cache'.
releaseCache :: Cache -> IO ()
releaseCache = releasePool . pool

-- | Atomically read a cache segment.
hitSegment :: (Cache -> MVar s) -- ^ Cache segment accessor
           -> Cacheable s
hitSegment = ((liftIO . readMVar) .)

-- | Atomically read a segment record, passing control to interior transformer
--   on failure.
hitRecordFallback :: Ord k
                  => k                -- ^ The sought out object's ID
                  -> M.Map k (MVar v) -- ^ Cache segment
                  -> CacheM v         -- ^ Action to run to retrieve target
                                      --   value on failure
                  -> CacheM v
hitRecordFallback = ((flip fromMaybe . ((liftIO . readMVar) <$>)) .) . M.lookup

-- | Lift the execution of a SQL query into the 'Cacheable' monad. The query is
--   expected to return zero or one result(s). This function does not return
--   into the 'Maybe' monad; it is named for 'maybeEx' and must be called with
--   a statement obeying the assumptions 'maybeEx' makes.
liftMaybeQ :: CxRow Postgres t
           -- | SQL Statement to execute. The statement must return exactly
           --   zero or one record(s).
           => Stmt Postgres
           -- | The error to return if the statement provides no results.
           -> CacheError
           -- | Function from the record tuple to the thing you actually
           --   want.
           -> (t -> r)
           -> Cacheable r
liftMaybeQ s dbe fr c = do
    r <- session (pool c) (tx defTxMode (maybeEx s))
    case r of (Left e)         -> left $ HasqlError e
              (Right Nothing)  -> left dbe
              (Right (Just v)) -> right $ fr v

-- | Lift the execution of a SQL query into the 'Cacheable' monad. The query may
--   return a nondeterministic number of results.
liftListQ :: CxRow Postgres t
             => Stmt Postgres
             -> (t -> r)
             -> Cacheable [r]
liftListQ s fr c = do
    r <- session (pool c) (tx defTxMode (listEx s))
    case r of (Left e)   -> left $ HasqlError e
              (Right ts) -> right $ map fr ts

-- | Report that a record with the given ID in the given table doesn't exist.
noSuchID :: String -- ^ Table name
         -> Word64 -- ^ ID
         -> CacheError
noSuchID t i = Nonexistent $ t ++ " with id " ++ (show i) ++ " does not exist."

-- | Report that a record indexed by the given property doesn't exist
noSuchThing :: Show t
            => String -- ^ Table name
            -> String -- ^ Indexing property name
            -> t      -- ^ Indexing property value
            -> CacheError
noSuchThing o n t = Nonexistent $ o ++ " with " ++ n ++ " " ++ (show t) ++ " does not exist."

-- | Clear a cache of all it's records. This ain't no LRU. There's probably a
--   better way to do this. Fix this to be exception safe again.
reaper :: Cacheable ()
reaper c = liftIO reapAll
    where reap = (flip swapMVar) M.empty
          reapAll = (reap $ memberIDCache c)
                 >> (reap $ eventIDCache c)
                 >> (reap $ projectIDCache c)
                 >> (reap $ evaluationIDCache c)
                 >> (reap $ conditionalIDCache c)
                 >> (reap $ freshmanProjectIDCache c)
                 >> (reap $ packetIDCache c)
                 >> (reap $ queueIDCache c)
                 >> (reap $ applicationIDCache c)
                 >> (reap $ metricIDCache c)
                 >> (reap $ reviewIDCache c)
                 >> (reap $ interviewIDCache c)
                 >> (reap $ questionIDCache c)
                 >> (reap $ termIDCache c)
                 >> (reap $ eboardMemberIDCache c)
                 >> (reap $ evaluationMemberIDCache c)
                 >> (reap $ conditionalEvaluationIDCache c)
                 >> (reap $ roomMemberIDCache c)
                 >> (reap $ queueMemberIDCache c)
                 >> (reap $ membershipMemberIDCache c)
                 >> (reap $ eventAttendeeMemberIDCache c)
                 >> (reap $ eventAttendeeEventIDCache c)
                 >> (reap $ projectParticipantMemberIDCache c)
                 >> (reap $ projectParticipantProjectIDCache c)
                 >> (reap $ freshProjParticipantProjectIDCache c)
                 >> (reap $ freshProjParticipantEvaluationIDCache c)
                 >> (reap $ packetMemberIDCache c)
                 >> (reap $ signatureMemberIDCache c)
                 >> (reap $ signaturePacketIDCache c)
                 >> (reap $ applicationMemberIDCache c)
                 >> (reap $ reviewApplicationIDCache c)
                 >> (reap $ interviewApplicationIDCache c)
                 >> (reap $ reviewMetricMetricIDCache c)
                 >> (reap $ reviewMetricReviewIDCache c)
                 >> (reap $ interviewMetricMetricIDCache c)
                 >> (reap $ interviewMetricInterviewIDCache c)
                 >> (reap $ answerQuestionIDCache c)
                 >> (reap $ answerApplicationIDCache c)
                 >> (reap $ duesMemberIDCache c)
                 >> (reap $ duesTermIDCache c)
                 >> return ()

-- | Add a single previously uncached value to the cache. This is done by a new
--   thread that discards any exceptions, to prevent any cache corruption from
--   interfering with servicing user requests and to make cache misses less
--   expensive.
singletonGhost :: (Cache -> IDCache a)
               -> Word64
               -> a
               -> Cacheable ()
singletonGhost a k v c = liftIO $ forkFinally insrt1 (\_ -> putStrLn "singletonGhost died!") >> return ()
    where m1  = a c
          insrt1 = do
               bracketOnError (takeMVar m1) (putMVar m1) insrt2
          insrt2 m2 = case M.lookup k m2 of
                        Nothing   -> newMVar v >>= (\v' -> putMVar m1 (M.insert k v' m2))
                        (Just m3) -> swapMVar m3 v >> return ()

-- | Create a ghost that replays the effects of an action in the 'Cacheable'
--   monad immediately before returning control to the exterior transformer.
sneakyGhostM :: (Cache -> IDCache a)
            -> Word64
            -> CacheM a
            -> Cacheable a
sneakyGhostM a k vM c = vM >>= (\v -> singletonGhost a k v c >> return v)

-- | Create a ghost that replays the effects of an action in the 'Cacheable'
--   monad immediately before returning control to the exterior transformer.
sneakyGhostC :: (Cache -> IDCache a)
            -> Word64
            -> Cacheable a
            -> Cacheable a
sneakyGhostC a k vM c = vM c >>= (\v -> singletonGhost a k v c >> return v)
