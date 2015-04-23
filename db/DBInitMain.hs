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

psql_path :: FilePath
psql_path = "psql"

schema_init_path :: FilePath
schema_init_path = "eval_schema.sql"

preflight :: FilePath -> Sh [T.Text]
preflight script = liftM catMaybes $ sequence checks
    where checks = [
            test_px psql_path >>= (\e -> return $ if e then Nothing else Just $ "psql not found in $PATH.")
           ,test_f  script    >>= (\e -> return $ if e then Nothing else Just $
                                  (toTextIgnore script) `T.append` " not found.")
           ]

quitWithErrors :: [T.Text] -> Sh ()
quitWithErrors = (>> quietExit 1) . echo . T.unlines

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
