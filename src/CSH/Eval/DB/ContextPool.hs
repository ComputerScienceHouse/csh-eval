module CSH.Eval.DB.ContextPool where

import Control.Concurrent.MVar (MVar
                               ,newMVar)

import Data.Time.Clock.POSIX (posixDayLength)

import Data.Pool (Pool
                 ,createPool)

import Database.HDBC (IConnection
                     ,clone
                     ,disconnect)
import Database.HDBC.PostgreSQL (Connection)

import CSH.Eval.DB.Statements (StatementGroup
                              ,createStatementGroup
                              ,cleanupStatementGroup)

-- | A pool of database 'Context's.
data ContextPool = ContextPool {
    rootConnection :: MVar Connection -- ^ Initial connection, cloned to populate the pool.
   ,contextPool    :: Pool Context    -- ^ Context pool.
   }

-- | A database connection context. Includes the 'Connection' itself and a
--   'StatementGroup' of pre-prepared statements.
data Context = Context {
    connection     :: Connection     -- ^ Context-local connection.
   ,statementGroup :: StatementGroup -- ^ Pre-prepared statements.
   }

-- | Create a 'ContextPool' with the provided PostgreSQL connection string
--   containing at most some maximum number of connections.
createContextPool :: String         -- ^ PostgreSQL connection string.
                  -> Int            -- ^ Maximum number of pooled contexts.
                  -> IO ContextPool
createContextPool cs ps = do
    rc  <- connectPostgreSql cs
    rcm <- newMVar rc
    -- Database connection contexts live in a pool with one stripe and have a
    -- lifetime of one day.
    p   <- createPool (createContext rcm) cleanupContext 1 posixDayLength ps
    return $ ContextPool rcm p

-- | Create a 'Context'.
createContext :: MVar Connection -- ^ The root 'Connection' from a 'ContextPool'
              -> IO Context
createContext rcm = do
    c  <- takeMVar rcm >>= clone
    sg <- createStatementGroup c
    return $ Context c sg

-- | Finalize a 'Context's 'StatementGroup' and close its 'Connection'.
cleanupContext :: Context -> IO ()
cleanupContext (Context c sg) = cleanupStatementGroup sg >> disconnect c
