import Network.Wai.Handler.Warp (defaultSettings
                                , setPort
                                , Port
                                , runEnv)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import CSH.Eval.Routes (evalAPI)
import CSH.Eval.Frontend (evalFrontend)
import Options.Applicative

data ServerOpts = ServerOpts { port :: Port
                             , withTLS :: Bool
                             }

runWithOptions :: ServerOpts -> IO ()
runWithOptions opts = evalFrontend
                  >>= if withTLS opts
                      then runTLS (tlsSettings "server.crt" "server.key")
                                  (setPort (port opts) defaultSettings)
                      else runEnv (port opts)


-- | Main function for running the Evaluations Database website
-- This runs the evaluations databse frontend defined in "CSH.Eval.Frontend"
-- using warp, running on the port defined the the PORT environment variable
-- (defaulting to running on port 8000)
main = execParser opts >>= runWithOptions 
   where
     parser = ServerOpts <$> optPort
                         <*> optWithTLS
     opts = info parser mempty

optPort :: Parser Port
optPort = option auto
        (  long "port"
        <> short 'p'
        <> value 8000
        <> metavar "K"
        <> help "Run site on port K"
        )

optWithTLS :: Parser Bool
optWithTLS = switch
           (  long "tls_support"
           <> short 's'
           <> help "Enable TLS support."
           )
