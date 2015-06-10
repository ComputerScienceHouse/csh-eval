{-|
Module      : CSH.Eval.Routes
Description : Routing for the API server
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX
-}
{-# LANGUAGE DataKinds #-}
module CSH.Eval.Routes where

import Network.Wai (Application)
import Servant

-- | The Evaluations Database API WAI application 
evalAPI :: Application
evalAPI = serve evalAPIProxy server

-- | API Proxy for servant
evalAPIProxy :: Proxy EvalAPI
evalAPIProxy = Proxy

-- | The definition of the API for the Evaluations Database. 
-- Additional API calls should be added here, and their handlers put in
-- 'CSH.Eval.Routes.server'
type EvalAPI = Get '[JSON] String

-- | The definition of the handlers for each API call, corresponding with 
-- the 'CSH.Eval.Routes.EvalAPI' type
server :: Server EvalAPI
server = return "Hello world"

