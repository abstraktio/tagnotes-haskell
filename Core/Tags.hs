module Core.Tags where


import           Database.SQLite
import           GHC.Int (Int64)
import           Control.Monad (join)
import           Data.List (intersperse, nub)
import           Core.Types
import           Core.Helpers

-- done
-- | this ensures that only the specified tags will remain associated to the note, by deleting tags that were previously associated but are not on the tags list (the parameter here) anymore, and by adding tags that weren't previously associated but now show on the tags list.
saveTagsEnsuringOnly h noteId tags = do
  (deleted, kept, added) <- calculateTagDiffForNote h noteId tags
  mapM_ (\tagId -> deleteTagNoteJoin h tagId noteId) deleted
  mapM_ (\tagId -> maybeMakeTagNoteJoin h tagId noteId) added
  return (deleted, kept, added)


-- done
calculateTagDiffForNote :: DbHandle -> Int64 -> [Int64] -> IO ([Int64],[Int64],[Int64])
calculateTagDiffForNote h noteId desiredTags = do
  currentTags <- getAllTagIdsForNote h noteId
                 :: IO (Either String [[Row String]])
                 -- currentTags ::: Right [[[("tag_id","1")],[("tag_id","2")]]]
  let c = map strToInt64 (normalize currentTags)
  return $ (deletedTags c, keptTags c, addedTags c)
    where
      deletedTags c = filter (not . (flip elem desiredTags)) c
      keptTags    c = filter        (flip elem c)  desiredTags
      addedTags   c = filter (not . (flip elem c)) desiredTags

normalize c = either (const []) (\a -> map (snd.head) (head $ a)) c

-- done
deleteTagNoteJoin h tagId noteId = do
  execParamStatement_ h query params
    where
      query = "DELETE FROM notes_tags WHERE tag_id = :tagId AND note_id = :noteId;"
      params = [(":tagId", Int tagId), (":noteId", Int noteId)]

-- done
-- | get all tagIds associated to a noteId
getAllTagIdsForNote :: DbHandle -> Int64 -> IO (Either String [[Row String]])
getAllTagIdsForNote h noteId = do
  execParamStatement h query params
    where
      query = "SELECT tag_id FROM notes_tags WHERE note_id = :noteId;"
      params = [(":noteId", Int noteId)]

-- done
-- | conditionally creates tags and join table entries.
-- in other words, saveTags ensures the tags and their join entries exist (by creating them if they don't already exist).
saveTags h noteId tags = do
  tagIds <- mapM (maybeMakeTagId h) tags
  mapM_ (\tagId -> maybeMakeTagNoteJoin h tagId noteId) (map read tagIds)

-- done
-- | creates a join table entry if it doesn't exist already, otherwise does no action.
maybeMakeTagNoteJoin h tagId noteId = do
  joinExistence <- doesTagNoteJoinExist h tagId noteId
  if joinExistence
  then return ()
  else do { makeTagNoteJoin h tagId noteId
          ; return () }

-- done
-- | returns True if the join table entry exists, False otherwise.
doesTagNoteJoinExist :: DbHandle -> Int64 -> Int64 -> IO Bool
doesTagNoteJoinExist h tagId noteId = do
  result <- execParamStatement h query params
            :: IO (Either String [[Row String]])
  case result of
    Left  x     -> return False
    Right [[]]  -> return False
    Right [[x]] -> case lookup "count" x of
                          Nothing  -> return False
                          Just "0" -> return False
                          Just _   -> return True
    where
      query = "SELECT count(*) as count FROM notes_tags WHERE tag_id = :tagId AND note_id = :noteId LIMIT 1;"
      params = [(":tagId", Int tagId), (":noteId", Int noteId)]

-- done
-- | creates a join table entry unconditionally.
makeTagNoteJoin h tagId noteId = do
  execParamStatement_ h query params
    where
      query = "INSERT INTO notes_tags (tag_id, note_id) VALUES(:tagId,:noteId);"
      params = [(":tagId", Int tagId), (":noteId", Int noteId)]

-- done
-- | creates a tag if it doesn't exist; return its id regardless.
maybeMakeTagId h tag = do
  id <- getTagId h tag
  case id of
    Just id -> return id
    Nothing -> do
               makeTag h tag
               lastId <- lastRowId h
               return . show $ lastId

-- done
-- | looks up a tag id by its name. returns Nothing if it doesn't exist; otherwise returns Just its id.
getTagId h tag = do
  result <- execParamStatement h "SELECT id FROM tags WHERE name = :name LIMIT 1;" [(":name", Text tag)]
            :: IO (Either String [[Row String]])
  case result of
    Left  x     -> return Nothing                         -- if error, return IO Nothing
    Right [[]]  -> return Nothing                         -- if no results, return IO Nothing
    Right [[x]] -> case (lookup "id" x) of                -- if results, get id from Row
                     Nothing -> return Nothing            -- if no id, return IO Nothing
                     Just id -> return $ Just id          -- if id, return IO (Just id)


{--
getTagId h tag = do
  rows <- execParamStatement h query params
          :: IO (Either String [[Row String]])
  let c = map strToInt64 (normalize rows)
  return c
    where
      query = "SELECT id FROM tags WHERE name = :name ;"
      params = [(":name", Text tag)]

--}


-- done
-- | creates a tag unconditionally.
makeTag :: DbHandle -> Tag -> IO (Maybe String)
makeTag h tag = execParamStatement_ h query params
  where
    query = "INSERT INTO tags (name) VALUES (:name);"
    params = [(":name", Text tag)]

getTagIds h tags = do
  rows <- execParamStatement h query (paramList tags)
          :: IO (Either String [[Row String]])
  let c = map strToInt64 (normalize rows)
  return c
    where
      query = "SELECT id FROM tags WHERE name IN " ++ placeholders (length tags)

