import Network.Wai.Handler.Warp (runEnv)
import CSH.Eval.Routes (evalAPI)
import CSH.Eval.Frontend (EvalFrontend)

main = do
    runEnv 8000 evalAPI
    runEnv 8001 EvalFrontend
