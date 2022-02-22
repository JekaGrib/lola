{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Common.DeleteMany where

import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Data.List (intercalate)
import Methods.Common
import Types

data Handle m = Handle
  { hConf :: Config,
    selectNums :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Id],
    deleteFromDb :: Table -> Where -> [DbValue] -> m ()
  }

makeH :: Config -> Handle IO
makeH conf =
  let conn = extractConn conf
   in Handle
        conf
        (selectOnly' conn)
        (deleteFromDb' conn)

deleteAllAboutPost :: (MonadCatch m) => Handle m -> PostId -> m ()
deleteAllAboutPost h postId = do
  deletePostsPicsTags h [postId]
  deleteFromDb h "comments" "post_id=?" [Id postId]
  draftsIds <- selectNums h "drafts" ["draft_id"] "post_id=?" [Id postId]
  deleteAllAboutDrafts h draftsIds
  deleteFromDb h "posts" "post_id=?" [Id postId]

deletePostsPicsTags :: (MonadCatch m) => Handle m -> [PostId] -> m ()
deletePostsPicsTags _ [] = return ()
deletePostsPicsTags h postsIds = do
  let values = fmap Id postsIds
  let where' = intercalate " OR " . fmap (const "post_id=?") $ postsIds
  deleteFromDb h "postspics" where' values
  deleteFromDb h "poststags" where' values

deleteAllAboutDrafts :: (MonadCatch m) => Handle m -> [DraftId] -> m ()
deleteAllAboutDrafts _ [] = return ()
deleteAllAboutDrafts h draftsIds = do
  let values = fmap Id draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteDraftsPicsTags h draftsIds
  deleteFromDb h "drafts" where' values

deleteDraftsPicsTags :: (MonadCatch m) => Handle m -> [DraftId] -> m ()
deleteDraftsPicsTags _ [] = return ()
deleteDraftsPicsTags h draftsIds = do
  let values = fmap Id draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteFromDb h "draftspics" where' values
  deleteFromDb h "draftstags" where' values
