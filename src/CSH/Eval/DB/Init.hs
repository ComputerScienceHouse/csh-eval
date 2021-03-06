module CSH.Eval.DB.Init where

import qualified Data.ByteString.Char8 as C

import qualified Hasql          as H
import qualified Hasql.Postgres as HP

import System.IO

import Control.Exception
import Options.Applicative

import CSH.Eval.Config
import CSH.Eval.DB.Schema

initDB :: SchemaInit
initDB = sequence_ [
     committee_t
   , status_t
   , member_t
   , dues_t
   , event_t
   , project_t
   , eval_t
   , member
   , eboard
   , room
   , membership
   , event
   , event_attendee
   , project
   , project_participant
   , evaluation
   , conditional
   , freshman_project
   , freshman_project_participant
   , packet
   , signature
   , queue
   , application
   , metric
   , review_metric
   , interview_metric
   , review
   , interview
   , question
   , answer
   , term
   , dues
   , statement
   , statement_exec
   , enableForeignKeys
   , enableIndices
   ]

withEcho :: Bool -> IO a -> IO a
withEcho e a = hGetEcho stdin
           >>= (\o -> bracket_ (hSetEcho stdin e)
                               (hSetEcho stdin o)
                               a)

getPassword :: IO C.ByteString
getPassword = do
    putStr "Password: "
    hFlush stdout
    passwd <- withEcho False getLine
    putChar '\n'
    hFlush stdout
    return (C.pack passwd)

runInit :: DBInitCmd -> IO ()
runInit opts = do
    passwd <- getPassword
    putStr "Initializing evaluations database..."
    let pgs = HP.ParamSettings (dbHost opts)
                               (dbPort opts)
                               (dbUser opts)
                               (passwd)
                               (dbName opts)
    poolSettings <- maybe (fail "Malformed settings.") return (H.poolSettings 1 1)
    pool         <- H.acquirePool pgs poolSettings

    r <- H.session pool $ H.tx Nothing initDB

    print r

    H.releasePool pool

dbInitParser :: Parser Command
dbInitParser = InitDB
            <$> (DBInitCmd
            <$> optHost
            <*> optPort
            <*> optUser
            <*> optDb)
  where
    optHost = argument bstr
            (  metavar "<HOST>"
            <> help    "The hostname of the postgres database to connect to"
            )

    optPort = argument auto
            (  metavar "<PORT>"
            <> help    "The port to connect on"
            )

    optUser = argument bstr
            (  metavar "<USER>"
            <> help    "The name of the database user"
            )

    optDb = argument bstr
          (  metavar "<DB>"
          <> help    "The name of the eval database"
          )

    bstr :: ReadM C.ByteString
    bstr = eitherReader (Right . C.pack)
