{-|
Module      : CSH.Eval.Config
Description : Configuration utilities
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

Provides functionality for reading config values
-}
module CSH.Eval.Config
( evalConfig
, module Data.Configurator
, Command (..)
, ServerCmd (..)
) where
import Data.Configurator
import Network.Wai.Handler.Warp (Port)

data Command = Members ServerCmd
             | Intro ServerCmd

data ServerCmd = ServerCmd { withTLS :: Bool
                           , port    :: Port
                           }

evalConfig = load [Required "config/csh-eval.rc"]
