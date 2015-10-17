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
    -- * Atomic Reads
    hitSegment
  , hitRecordFallback
    -- * Fallback Combinators
  , maybeFallback
    -- * Error Reporting
  , noSuchID
  ) where

import Control.Applicative ((<$>))

import Control.Concurrent.MVar

import Control.Monad.Trans.Control
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

-- | Fix this to fork a child to stabilize the cache. Maybe document this at
--   all?
maybeFallback :: CxRow Postgres t
              -- | SQL Statement to execute. The statement must return exactly
              --   zero or one record(s).
              => Stmt Postgres
              -- | The error to return if the statement provides no results.
              -> CacheError
              -- | Function from the record tuple to the thing you actually
              --   want.
              -> (t -> r)
              -> Cacheable r
maybeFallback s dbe fr c = do
    r <- session (pool c) (tx defTxMode (maybeEx s))
    case r of (Left e)         -> left $ HasqlError e
              (Right Nothing)  -> left dbe
              (Right (Just v)) -> right $ fr v

-- | Report that a record with the given id in the given table doesn't exist.
noSuchID :: String -> Word64 -> CacheError
noSuchID t i = Nonexistent $ t ++ " with id " ++ (show i) ++ " does not exist."
