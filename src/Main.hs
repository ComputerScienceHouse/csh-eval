import Network.Wai.Handler.Warp (defaultSettings
                                , setPort
                                , Port
                                , runEnv)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import CSH.Eval.Routes (evalAPI)
import CSH.Eval.Frontend (evalFrontend)
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (text)

data Command = Members ServerCmd
             | Intro ServerCmd

data ServerCmd = ServerCmd { withTLS :: Bool
                           , port    :: Port
                           }

runWithOptions :: Command -> IO ()
runWithOptions (Members opts) = putStrLn cshlogo
                             >> evalFrontend
                            >>= if withTLS opts
                                then runTLS (tlsSettings "server.crt" "server.key")
                                            (setPort (port opts) defaultSettings)
                                else runEnv (port opts)
runWithOptions _ = undefined

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

-- | Main function for running the Evaluations Database website
-- This runs the evaluations databse frontend defined in "CSH.Eval.Frontend"
-- using warp, running on the port defined the the PORT environment variable
-- (defaulting to running on port 8000)
main = customExecParser pprefs opts >>= runWithOptions
   where
     pprefs = ParserPrefs { prefMultiSuffix     = " | "
                          , prefDisambiguate    = True
                          , prefShowHelpOnError = True
                          , prefBacktrack       = True
                          , prefColumns         = 80
                          }

     opts = info parser (  fullDesc
                        <> headerDoc (Just (text cshlogo))
                        )

     parser =  helper
           <*> subparser
               (  membersCmd
               <> introCmd
               )

     members = Members <$> serverCmd

     intro   = Intro <$> serverCmd

     serverCmd = (ServerCmd <$> optWithTLS
                            <*> optPort)

     membersCmd = command "members"
                          (info members (progDesc
                                "Run evals for current members"))
     introCmd = command "intro"
                  (info intro (progDesc
                        "WARNING: Unimplemented. Run evals for intro members"))


cshlogo =  "╔═══════════════════════╗\n\r"
        ++ "║   ╔═══╗ ╗             ║\n"
        ++ "║   ║╔═╗╦ ║ Computer    ║\n"
        ++ "║   ║╚═╗╠═╣ Science     ║\n"
        ++ "║   ║╚═╝╩ ║ House       ║\n"
        ++ "║   ╚═══╝ ╝             ║\n"
        ++ "║ Evaluations Database  ║\n"
        ++ "╚═══════════════════════╝"
