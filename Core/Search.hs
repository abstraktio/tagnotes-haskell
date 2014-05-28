module Core.Search (searchft, searchtags) where


import           Database.SQLite
import           GHC.Int (Int64)
import           Control.Monad (join)
import           Data.List (intersperse, nub)
import           Core.Types
import           Core.Tags
import           Core.Helpers
import           Control.Monad
import           Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)


searchft words = do
    c <- dbConnection
    liftIO $ searchFT c words

searchtags tags = do
    c <- dbConnection
    liftIO $ searchTags c tags

-- | Search full-text by keyword
searchFT :: DbHandle -> [String] -> IO (Either String [[Row String]])
searchFT h words = do
  execParamStatement h query [(":words",Text (ftwords words))]
    where
      ftwords :: [String] -> String
      ftwords = join . map wrapQuote
      wrap :: String -> String -> String
      wrap w t = w ++ t ++ w
      wrapQuote = wrap "\""
      query = "SELECT distinct n.id, n.title FROM notes n LEFT JOIN notes_tags nt ON nt.note_id = n.id LEFT JOIN tags t ON nt.tag_id = t.id JOIN fulltext ft ON ft.docid = n.id WHERE ft.fulltext MATCH :words;"

-- | search by tag (the search is an AND search on all tags)
searchTags :: DbHandle -> [Tag] -> IO (Either String [[Row String]])
searchTags h _tags = do
  let uniqueTags = nub _tags
  let lenTags = length uniqueTags
  execParamStatement h (query lenTags) (paramList uniqueTags)
    where
      query n = "SELECT n.id, n.title FROM notes_tags nt JOIN tags t ON nt.tag_id = t.id JOIN notes n ON nt.note_id = n.id WHERE t.name IN " ++ placeholders n ++ " GROUP BY nt.note_id HAVING COUNT(DISTINCT nt.tag_id) = " ++ show n
