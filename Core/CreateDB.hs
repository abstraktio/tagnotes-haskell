module Core.CreateDB (createdb) where


import           Database.SQLite
import           GHC.Int (Int64)
import           Control.Monad (join)
import           Data.List (intersperse, nub)
import           Core.Types
import           Control.Monad
import           Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)

createTagsTable = "CREATE TABLE IF NOT EXISTS tags (\
                  \id   INTEGER PRIMARY KEY,\
                  \name TEXT NOT NULL UNIQUE);"

createNotesTable = "CREATE TABLE IF NOT EXISTS notes (\
                   \id     INTEGER PRIMARY KEY,\
                   \title  TEXT NOT NULL DEFAULT '',\
                   \text   TEXT NOT NULL DEFAULT '',\
                   \created_on DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,\
                   \modified_on DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP)"

createJoinTable = "CREATE TABLE IF NOT EXISTS notes_tags (\
                   \note_id INTEGER NOT NULL REFERENCES notes(id),\
                   \tag_id  INTEGER NOT NULL REFERENCES tags(id),\
                   \PRIMARY KEY (note_id, tag_id));"

createNoteIndex = "CREATE INDEX notes_tags_note_id_idx ON notes_tags (note_id ASC);"

createTagIndex = "CREATE INDEX notes_tags_tag_id_idx ON notes_tags (tag_id ASC);"

createFts = "CREATE VIRTUAL TABLE fulltext USING fts4(title,text);"

creationCmds = [createTagsTable
               ,createNotesTable
               ,createJoinTable
               ,createNoteIndex
               ,createTagIndex
               ,createFts]

createDb :: DbHandle -> IO ()
createDb h = mapM_ (execStatement_ h) creationCmds

createdb = do
    c <- dbConnection
    liftIO $ createDb c