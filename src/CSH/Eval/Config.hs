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
  , module Data.Configurator
  , Command (..)
  , ServerCmd (..)
  , DBInitCmd (..)
  ) where

import Data.Configurator
import Data.Word (Word16)
import qualified Data.ByteString.Char8 as C

import Network.Wai.Handler.Warp (Port)

data Command = Members ServerCmd
             | Intro ServerCmd
             | InitDB DBInitCmd

data ServerCmd = ServerCmd { withTLS :: Bool
                           , port    :: Port
                           }

data DBInitCmd = DBInitCmd
                { dbHost :: C.ByteString
                , dbPort :: Word16
                , dbUser :: C.ByteString
                , dbName :: C.ByteString
                }

evalConfig = load [Required "config/csh-eval.rc"]
