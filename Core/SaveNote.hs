module Core.SaveNote (savenote, editnote, runInDbCtx) where


import           Database.SQLite
import           GHC.Int (Int64)
import           Control.Monad (join)
import           Data.List (intersperse, nub)
import           Core.CreateDB
import           Core.Types
import           Core.Helpers
import           Core.Tags
import           Control.Monad
import           Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)

savenote title text tags = do
    c <- dbConnection
    liftIO $ saveNote c title text tags

editnote id title text tags = do
    c <- dbConnection
    liftIO $ editNote c id title text tags

-- done
{- | To save a note is to insert it and save its tags.
To insert a note is to save it in the notes table and in the fulltext table.
To save tags is to maybe create them (if they don't already exist) and then to maybe save them to the join table (if that relationship doesn't already exist).
-}
saveNote :: DbHandle -> Title -> Text -> [Tag] -> IO ()
saveNote h title text tags = do
  insertNote h title text
  lastId <- lastRowId h
  saveTags h lastId tags

-- done
-- | inserts a note into the notes table and the fulltext table.
insertNote :: DbHandle -> Title -> Text -> IO (Maybe String)
insertNote h title text = do
  insertNote' h title text
  lastId <- lastRowId h
  insertNoteFT h (show lastId) title text
    where
      insertNote' h title text = insertRow h "notes" [("title",title),("text",text)]
      insertNoteFT h noteId title text = insertRow h "fulltext" [("docid",noteId),("title",title),("text",text)]

-- done
{- | To edit a note is to update it (both in the notes and fulltext tables); then ensure all desired tags have IDs (create tag otherwise); then ensure only the desired tags are associated to the note
-}
editNote :: DbHandle -> Int64 -> Title -> Text -> [String] -> IO ([Int64], [Int64], [Int64])
editNote h id title text tags = do
  updateNote h id title text
  mapM_ (maybeMakeTagId h) tags
  tagIds <- getTagIds h tags
  saveTagsEnsuringOnly h id tagIds

-- done
-- | updates a note in the notes table and the fulltext table.
updateNote :: DbHandle -> Int64 -> Title -> Text -> IO (Either String [[Row String]])
updateNote h id title text = do
  updateNote' h id title text
  updateNoteFT h id title text
    where
      updateNote' h id title text =
        execParamStatement h
        "UPDATE notes SET title = :title, text = :text WHERE id = :id;"
        [(":title",Text title),(":text",Text text),(":id",Int id)]
        :: IO (Either String [[Row String]])
      updateNoteFT h id title text =
        execParamStatement h
        "UPDATE fulltext SET title = :title, text = :text WHERE docid = :id;"
        [(":title",Text title),(":text",Text text),(":id",Int id)]
        :: IO (Either String [[Row String]])
