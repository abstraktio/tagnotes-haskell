module Core.Helpers where


import           Database.SQLite
import           GHC.Int (Int64)
import           Control.Monad (join)
import           Data.List (intersperse, nub)
import           Core.Types
import           Control.Monad
import           Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)

paramList words = zipWith (,) (makeParamNames (length words)) (map Text words)
placeholders n = "(" ++ (join $ intersperse ", " (makeParamNames n)) ++ ")"

makeParamNames n = makeNames n ((:)':')
  where
    makeNames n f = take n $ zipWith (++) (repeat $ f "word") (map show [1..])

sqldb path = openConnection path

lastRowId :: DbHandle -> IO (Int64)
lastRowId h = do lastId <- getLastRowID h
                 return $ (fromIntegral(lastId) :: Int64)

dumpNotes h = execStatement h "select * from notes;"
              :: IO (Either String [[Row String]])

dumpTags h = execStatement h "select * from tags;"
              :: IO (Either String [[Row String]])

dumpTagNotes h = execStatement h "select * from notes_tags;"
              :: IO (Either String [[Row String]])

intToInt64 i = fromIntegral (i) :: Int64
strToInt64 s = intToInt64 (read s)

dumpnotes = do
    c <- dbConnection
    liftIO $ dumpNotes c

dumptags = do
    c <- dbConnection
    liftIO $ dumpTags c

dumptagnotes = do
    c <- dbConnection
    liftIO $ dumpTagNotes c

closeconnection = do
    c <- dbConnection
    liftIO $ closeConnection c

