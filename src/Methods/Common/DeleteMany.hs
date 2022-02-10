{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}





module Methods.Common.DeleteMany where
          
import           Types
import Methods.Common
import           Data.Text                      ( pack,Text )
import           Control.Monad.Catch            ( MonadCatch)
import           Data.List                      ( intercalate )
import  Conf (Config(..),extractConn)


data Handle m = Handle 
  { hConf              :: Config,
    selectNums          :: Table -> [Param] -> Where -> [Text] -> m [Id],
    deleteFromDb       :: Table -> String -> [Text] -> m ()
    }

makeH :: Config -> Handle IO
makeH conf = let conn = extractConn conf in
  Handle 
    conf 
    (selectOnly' conn)
    (deleteFromDb' conn) 

  
deleteAllAboutPost :: (MonadCatch m) => Handle m  -> PostId -> m ()
deleteAllAboutPost h postId = do
  let postIdTxt = pack . show $ postId
  deletePostsPicsTags h [postId]
  deleteFromDb h "comments" "post_id=?" [postIdTxt]
  draftsIds <- selectNums h "drafts" ["draft_id"] "post_id=?" [postIdTxt]  
  deleteAllAboutDrafts h draftsIds
  deleteFromDb h "posts" "post_id=?" [postIdTxt]

deletePostsPicsTags :: (MonadCatch m) => Handle m  -> [PostId] -> m ()
deletePostsPicsTags _ [] = return ()
deletePostsPicsTags h postsIds = do
  let values = fmap (pack . show) postsIds
  let where' = intercalate " OR " . fmap (const "post_id=?") $ postsIds
  deleteFromDb h "postspics" where' values
  deleteFromDb h "poststags" where' values

deleteAllAboutDrafts :: (MonadCatch m) => Handle m  -> [DraftId] -> m ()
deleteAllAboutDrafts _ [] = return ()
deleteAllAboutDrafts h draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteDraftsPicsTags h draftsIds
  deleteFromDb h "drafts" where' values

deleteDraftsPicsTags :: (MonadCatch m) => Handle m  -> [DraftId] -> m ()
deleteDraftsPicsTags _ [] = return ()
deleteDraftsPicsTags h draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteFromDb h "draftspics" where' values
  deleteFromDb h "draftstags" where' values
