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
  , initCacheFromConfig
  , releaseCache
    -- * Atomic Reads
  , hitSegment
  , hitRecordFallback
    -- * Query Combinators
  , liftMaybeQ
  , liftListQ
  , liftInsertSingleQ
  , liftUnitQ
    -- * Error Reporting
  , noSuchID
  , noSuchThing
    -- * Ghosts
  , reaper
  , singletonGhost
  , appendGhost
  , sneakyGhostM
  , sneakyGhostC
  ) where

import Control.Concurrent

import Control.Exception

import Control.Monad.Trans.Either

import Control.Monad.IO.Class (liftIO)

import Data.Maybe (fromMaybe)

import Data.Functor.Identity (Identity, runIdentity)

import qualified Data.Map as M

import Data.Word

import Hasql
import Hasql.Postgres

import CSH.Eval.Model
import CSH.Eval.Config

import System.Log.Logger

-- | Initialize a 'Cache', including the enclosed 'Pool'.
initCache :: Settings
          -> PoolSettings
          -> Logger
          -> IO Cache
initCache cs ps logger = Cache
                     <$> acquirePool cs ps
                     <*> pure logger
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*> newIDCache
                     <*  logL logger INFO "Initialized Cache"
                       where newIDCache = (liftIO . newMVar) M.empty

-- | Initialize a 'Cache' with settings derived from the config
initCacheFromConfig :: Logger -> IO Cache
initCacheFromConfig logger = do
    cfg <- evalConfig
    cxSets <- cxCfgSettings cfg
    poolSets <- poolCfgSettings cfg
    initCache cxSets poolSets logger

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
liftMaybeQ :: CxRow Postgres t => Stmt Postgres
           -- ^ SQL Statement to execute. The statement must return exactly
           --   zero or one record(s).
           -> CacheError
           -- ^ SQL Statement to execute. The statement must return exactly
           --   zero or one record(s).
           -> (t -> r)
           -- ^ The error to return if the statement provides no results.
           -> Cacheable r
           -- ^ Function from the record tuple to the thing you actually
           --   want.
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

-- | Fix this to provide better constraint errors.
liftInsertSingleQ :: CxRow Postgres (Identity t)
                  => Stmt Postgres
                  -> Cacheable t
liftInsertSingleQ s c = do
    r <- session (pool c) (tx defTxMode (singleEx s))
    case r of (Left e)  -> left $ HasqlError e
              (Right t) -> right (runIdentity t)

liftUnitQ :: Stmt Postgres -> Cacheable ()
liftUnitQ s c = do
    r <- session (pool c) (tx defTxMode (unitEx s))
    case r of (Left e)   -> left $ HasqlError e
              (Right ()) -> right ()

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
singletonGhost a k v c = liftIO $ forkFinally insrt1 (either (const $ putStrLn "singletonGhost died!") (const $ return ())) >> return ()
    where m1  = a c
          insrt1 = do
               bracketOnError (takeMVar m1) (putMVar m1) insrt2
          insrt2 m2 = case M.lookup k m2 of
                        Nothing   -> newMVar v >>= (\v' -> putMVar m1 (M.insert k v' m2))
                        (Just m3) -> swapMVar m3 v >> return ()

appendGhost :: (Cache -> IDCache [a])
            -> Word64
            -> a
            -> Cacheable ()
appendGhost a k v c = liftIO $ forkFinally insrt1 (either (const $ putStrLn "appendGhost died!") (const $ return ())) >> return ()
    where m1 = a c
          insrt1 = do
               bracketOnError (takeMVar m1) (putMVar m1) insrt2
          insrt2 m2 = case M.lookup k m2 of
                        Nothing   -> newMVar [v] >>= (\v' -> putMVar m1 (M.insert k v' m2))
                        (Just m3) -> modifyMVar_ m3 (return . (v:))

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
