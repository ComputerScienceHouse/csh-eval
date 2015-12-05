{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : CSH.Eval.Config
Description : Configuration utilities
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

Provides functionality for reading config values.
-}
module CSH.Eval.Config (
    evalConfig
  , cxCfgSettings
  , poolCfgSettings
  , module Data.Configurator
  , Command (..)
  , ServerCmd (..)
  , DBInitCmd (..)
  ) where

import Data.Configurator
import Data.Configurator.Types
import Data.Word (Word16)
import Hasql
import Hasql.Postgres
import System.Environment
import qualified Data.ByteString.Char8 as C

-- | Toplevel 'csh-eval' command configuration type.
data Command = Members ServerCmd -- ^ Members only site configuration type
             | Intro ServerCmd   -- ^ Freshman site configuration type
             | InitDB DBInitCmd  -- ^ DB initialization command configuation type

-- | Configuation type for launching either site
data ServerCmd = ServerCmd { withTLS :: Bool -- ^ Launch with TLS support
                           , port    :: Int  -- ^ Which port to launch on
                           }

-- | Configuration type for running the db initialization script
data DBInitCmd = DBInitCmd
                { dbHost :: C.ByteString -- ^ Database hostname
                , dbPort :: Word16       -- ^ Database port
                , dbUser :: C.ByteString -- ^ Database username
                , dbName :: C.ByteString -- ^ Database name
                }

-- | Load a config from the path given by the 'CSH_EVAL_CONFIG' environment
--   variable, or default to 'config/csh-eval.rc'
evalConfig :: IO Config
evalConfig = lookupEnv "CSH_EVAL_CONFIG"
         >>= maybe (load [Required "config/csh-eval.rc"])
                   (\x -> load [Required x])

-- | Derive hasql postgres connection settings from configuration.
cxCfgSettings :: Config -> IO Settings
cxCfgSettings cfg = ParamSettings
                <$> (lookupDefault "localhost" cfg "db.host")
                <*> (lookupDefault 5432 cfg "db.port")
                <*> (lookupDefault "" cfg "db.user")
                <*> (lookupDefault "" cfg "db.password")
                <*> (lookupDefault "" cfg "db.dbname")

-- | Derive hasql pool settings from configuration.
poolCfgSettings :: Config -> IO PoolSettings
poolCfgSettings cfg = do
    max_con <- (lookupDefault 1 cfg "db.pool.max_con")
    idle_timeout <- (lookupDefault 10 cfg "db.pool.idle_timeout")
    case poolSettings max_con idle_timeout of
        Just x -> pure x
        Nothing -> fail ("Bad pool settings config:\n  db.pool.max_con = "
                       ++ (show max_con)
                       ++ "\n  db.pool.idle_timeout: "
                       ++ (show idle_timeout))
