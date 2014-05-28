module Core.Types where

import           Database.SQLite
import           GHC.Int (Int64)
import           Control.Monad (join)
import           Data.List (intersperse, nub)
import           Control.Monad
import           Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)

type Title = String
type Text  = String
type Tag   = String
type DbHandle = SQLiteHandle

data Hole = Hole


-- the database context monad
type DbCtx a = ReaderT DbHandle IO a

dbConnection :: DbCtx DbHandle
dbConnection = ask

runInDbCtx cmd = do
    c <- openConnection ":memory:"
    runReaderT cmd c
