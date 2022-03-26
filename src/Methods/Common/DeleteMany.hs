{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Common.DeleteMany where

import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Psql.Methods.Common.DeleteMany
import Types

data Handle m = Handle
  { hConf :: Config,
    selectDraftsForPost :: PostId -> m [DraftId],
    deleteDbPicsForPost :: PostId -> m (),
    deleteDbTagsForPost :: PostId -> m (),
    deleteDbCommsForPost :: PostId -> m (),
    deleteDbPost :: PostId -> m (),
    deleteDbPicsForDrafts :: [DraftId] -> m (),
    deleteDbTagsForDrafts :: [DraftId] -> m (),
    deleteDbDrafts :: [DraftId] -> m ()
  }

makeH :: Config -> Handle IO
makeH conf =
  let conn = extractConn conf
   in Handle
        conf
        (selectDraftsForPost' conn)
        (deleteDbPicsForPost' conn)
        (deleteDbTagsForPost' conn)
        (deleteDbCommsForPost' conn)
        (deleteDbPost' conn)
        (deleteDbPicsForDrafts' conn)
        (deleteDbTagsForDrafts' conn)
        (deleteDbDrafts' conn)

deleteAllAboutPost :: (MonadCatch m) => Handle m -> PostId -> m ()
deleteAllAboutPost h@Handle {..} postId = do
  deletePicsTagsForPost h postId
  deleteDbCommsForPost postId
  draftsIds <- selectDraftsForPost postId
  deleteAllAboutDrafts h draftsIds
  deleteDbPost postId

deletePicsTagsForPost :: (MonadCatch m) => Handle m -> PostId -> m ()
deletePicsTagsForPost Handle {..} postId = do
  deleteDbPicsForPost postId
  deleteDbTagsForPost postId

deleteAllAboutDrafts :: (MonadCatch m) => Handle m -> [DraftId] -> m ()
deleteAllAboutDrafts _ [] = return ()
deleteAllAboutDrafts h@Handle {..} draftsIds = do
  deletePicsTagsForDrafts h draftsIds
  deleteDbDrafts draftsIds

deletePicsTagsForDrafts :: (MonadCatch m) => Handle m -> [DraftId] -> m ()
deletePicsTagsForDrafts _ [] = return ()
deletePicsTagsForDrafts Handle {..} draftsIds = do
  deleteDbPicsForDrafts draftsIds
  deleteDbTagsForDrafts draftsIds
