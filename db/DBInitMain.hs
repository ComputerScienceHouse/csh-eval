module Main where

import qualified Data.ByteString.Char8    as C
import qualified Data.Text                as T

import qualified Hasql          as H

import qualified Hasql.Postgres as HP

import System.IO
import System.Environment

import Control.Exception
import Data.Word (Word16)
import Options.Applicative

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

data DBInitOpts = DBInitOpts
                { host :: C.ByteString
                , port :: Word16
                , user :: C.ByteString
                , db   :: C.ByteString
                }

getPassword :: IO C.ByteString
getPassword = do
    putStr "Password: "
    hFlush stdout
    passwd <- withEcho False getLine
    putChar '\n'
    hFlush stdout
    return (C.pack passwd)

main :: IO ()
main = do
    opts   <- execParser (info parser mempty)
    passwd <- getPassword
    putStr "Initializing evaluations database..."
    let pgs = HP.ParamSettings (host opts)
                               (port opts)
                               (user opts)
                               (passwd)
                               (db opts)
    poolSettings <- maybe (fail "Malformed settings.") return (H.poolSettings 1 1)
    pool         <- H.acquirePool pgs poolSettings

    r <- H.session pool $ H.tx Nothing initDB

    print r

    H.releasePool pool
  where
    parser = DBInitOpts
          <$> optHost
          <*> optPort
          <*> optUser
          <*> optDb

    optHost = argument auto
            (  metavar "<HOST>"
            <> help    "The hostname of the postgres database to connect to"
            )

    optPort = argument auto
            (  metavar "<PORT>"
            <> help    "The port to connect on"
            )

    optUser = argument auto
            (  metavar "<USER>"
            <> help    "The name of the eval user"
            )

    optDb = argument auto
          (  metavar "<DB>"
          <> help    "The name of the eval database"
          )
