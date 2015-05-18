import Network.Wai.Handler.Warp (runEnv)
import CSH.Eval.Routes (evalAPI)
import CSH.Eval.Frontend (evalFrontend)

-- | Main function for running the Evaluations Database website
-- This runs the evaluations databse frontend defined in "CSH.Eval.Frontend"
-- using warp, running on the port defined the the PORT environment variable
-- (defaulting to running on port 8000)
main = do
    frontend <- evalFrontend
    runEnv 8000 frontend
