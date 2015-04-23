module CSH.Eval.Routes where
import Servant

type Routes = Get String

routes :: Proxy Routes
routes = Proxy

server :: Server Routes
server = return "Hello world"
