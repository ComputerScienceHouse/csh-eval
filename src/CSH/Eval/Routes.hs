module CSH.Eval.Routes where

import Network.Wai (Application)
import Servant

type EvalAPI = Get String

evalAPI :: Application
evalAPI = serve evalAPIProxy server

evalAPIProxy :: Proxy EvalAPI
evalAPIProxy = Proxy

server :: Server EvalAPI
server = return "Hello world"

