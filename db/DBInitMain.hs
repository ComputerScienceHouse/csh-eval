{-|
Module      : Main
Description : CSH Eval Schema Initialization Script
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

This is a simple script for initializing the CSH Evals database schema. It
relies on the 'psql' program.
-}

{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (FilePath)

import Control.Monad

import Data.Maybe

import qualified Data.Text as T

import System.Environment

import Shelly

-- | Path to the 'psql' program. The default assumes it is available via the
--   $PATH environment variable.
psql_path :: FilePath
psql_path = "psql"

-- | Path to the 'eval_schema.sql' SQL script that does the actual work. The
--   default assumes it is available in the same directory this program is
--   invoked from.
schema_init_path :: FilePath
schema_init_path = "eval_schema.sql"

-- | Perform a series of checks before script execution, potentially failing
--   with an error message.
preflight :: FilePath -> Sh [T.Text]
preflight script = liftM catMaybes $ sequence checks
    where checks = [
            test_px psql_path >>= (\e -> return $ if e then Nothing else Just $ "psql not found in $PATH.")
           ,test_f  script    >>= (\e -> return $ if e then Nothing else Just $
                                  (toTextIgnore script) `T.append` " not found.")
           ]

-- | Abort with an error message, supressing the "Shelly" default of printing
--   an environment modification trace.
quitWithErrors :: [T.Text] -> Sh ()
quitWithErrors = (>> quietExit 1) . echo . T.unlines

-- | The executable takes a single optional argument, an alternative path to
--   the SQL script to execute. You'll likely need this if you're working from
--   a cabal sandbox (as you should be). Cabal will only use the sandbox if
--   invoked from the top level of the package directory tree, requiring this
--   program to be called like this:
--
--   > cabal exec runhaskell db/DBInitMain.hs db/eval_schema.sql
main :: IO ()
main = getArgs >>= \case []  -> shelly $ defaultScript
                         [s] -> shelly $ (thisScript . fromText . T.pack) s
    where defaultScript = do
              preflight schema_init_path >>= \case [] -> return ()
                                                   es -> quitWithErrors es
              run_ psql_path ["-U", "pvals", "-f", toTextIgnore schema_init_path, "pvals"]
          thisScript s = do
              preflight s >>= \case [] -> return ()
                                    es -> quitWithErrors es
              run_ psql_path ["-U", "pvals", "-f", toTextIgnore s, "pvals"]
