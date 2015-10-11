import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import CSH.Eval.Routes (evalAPI)
import CSH.Eval.Frontend (evalFrontend)

-- | Main function for running the Evaluations Database website
-- This runs the evaluations databse frontend defined in "CSH.Eval.Frontend"
-- using warp, running on the port defined the the PORT environment variable
-- (defaulting to running on port 8000)
main = evalFrontend >>= runTLS (tlsSettings "server.crt" "server.key")
                               (setPort 8000 defaultSettings)
