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

module CSH.Eval.Cacheable.Prim (
    -- * Atomic Reads
    hitSegment
  , hitRecordFallback
    -- * Error Reporting
  , noSuchID
  ) where

import Control.Applicative ((<$>))

import Control.Concurrent.MVar

import Control.Monad.Trans.Either

import Control.Monad.IO.Class (liftIO)

import Data.Maybe (fromMaybe)

import qualified Data.Map as M

import Data.Word

import Hasql
import Hasql.Postgres

import CSH.Eval.Model

-- | Atomically read a cache segment.
hitSegment :: (Cache -> MVar s) -> Cacheable s
hitSegment = ((liftIO . readMVar) .)

-- | Atomically read a segment record, passing control to interior transformer
--   on failure.
hitRecordFallback :: Ord k => k -> M.Map k (MVar v) -> CacheM v -> CacheM v
hitRecordFallback = ((flip fromMaybe . ((liftIO . readMVar) <$>)) .) . M.lookup

-- | Report that a record with the given id in the given table doesn't exist.
noSuchID :: String -> Word64 -> CacheError
noSuchID t i = Nonexistent $ t ++ " with id " ++ (show i) ++ " does not exist."
