module Main where

import qualified Data.ByteString.Char8    as C
import qualified Data.Text                as T

import qualified Hasql          as H

import qualified Hasql.Postgres as HP

import System.IO
import System.Environment

import Control.Exception

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

getArguments :: IO [String]
getArguments = do
    args <- getArgs
    if length args /= 4
    then putStr "usage: DBInitMain host port user db\n\n"
    else putStr "initializing evaluations database..."
    return args

getPassword :: IO String
getPassword = do
    putStr "Password: "
    hFlush stdout
    passwd <- withEcho False getLine
    putChar '\n'
    hFlush stdout
    return passwd

main :: IO ()
main = do
    [host, port, user, db] <- getArguments
    passwd                 <- getPassword

    let pgs = HP.ParamSettings (C.pack host)
                               (read port)
                               (C.pack user)
                               (C.pack passwd)
                               (C.pack db)
    poolSettings <- maybe (fail "Malformed settings.") return (H.poolSettings 1 1)
    pool         <- H.acquirePool pgs poolSettings

    r <- H.session pool $ H.tx Nothing initDB

    print r

    H.releasePool pool
