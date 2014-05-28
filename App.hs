module App where

import           Database.SQLite
import           GHC.Int (Int64)
import           Control.Monad (join)
import           Data.List (intersperse, nub)
import           Core
import           Control.Monad
import           Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)

main :: IO ()
main = runInDbCtx $ do
         createdb
         savenote "cake"    "this note will contain a cake recipe"   ["cake", "recipe", "dessert"]
         savenote "myths"   "some say the number three is lucky"     ["number", "essay"]
         savenote "eggs"    "the secret to perfect soft-boiled eggs" ["recipe", "eggs", "entree"]
         savenote "mousse"  "steps for a delicious chocolate mousse" ["mousse", "recipe", "dessert"]
         savenote "chicken" "cordon-bleu chicken, deep fried..."     ["recipe", "entree", "chicken"]

         liftIO $ putStrLn "\nNotes Table:"
         dumpnotes >>= liftIO . putStrLn . makeTable

         liftIO $ putStrLn "\nTags Table:"
         dumptags >>= liftIO . putStrLn . makeTable

         liftIO $ putStrLn "\nJoin Table:"
         dumptagnotes >>= liftIO . putStrLn . makeTable

         let tag = "dessert"
         liftIO $ putStrLn $ "\nTag search: (" ++ tag ++ ")"
         rows <- searchtags [tag]
         liftIO $ putStrLn $ makeTable rows

         liftIO $ putStrLn "\nFull-text search:"
         rows <- searchft ["delicious"]
         liftIO $ putStrLn $ makeTable rows

         liftIO $ putStrLn "\nLet's edit note 1 and its tags\nformat: (deletedTags,keptTags,addedTags)"
         editnote 1 "cake?" "The cake is a lie, it's a trap" ["cake","not-recipe!"] >>= liftIO . putStrLn . show

         liftIO $ putStrLn "\nJoin Table:"
         dumptagnotes >>= liftIO . putStrLn . makeTable

         liftIO $ putStrLn "\nTags Table:"
         dumptags >>= liftIO . putStrLn . makeTable

         closeconnection

makeTable (Right x) = join . intersperse "\n" . map (join . intersperse "\t|\t") . map (map snd) $ head x
makeTable (Left  x) = []