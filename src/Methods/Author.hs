{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Author where

import Api.Response (AuthorResponse (..), OkResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutDrafts)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Methods.Common.Selecty (Author (..))
import Oops
import ParseQueryStr (CreateAuthor (..), DeleteAuthor (..), GetAuthor (..), UpdateAuthor (..))
import Types

data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , selectDraftsForAuthor :: AuthorId -> m [DraftId]
  , selectAuthorsForUser  :: UserId -> m [AuthorId]
  , selectAuthors :: AuthorId -> m [Author]
  , updateDbAuthor :: UserId -> AuthorInfo -> AuthorId -> m ()
  , updateDbAuthorForPosts :: AuthorId -> AuthorId -> m ()
  , deleteDbAuthor :: AuthorId -> m ()
  , isExistAuthor :: AuthorId -> m Bool
  , isExistUser :: UserId -> m Bool
  , isUserAuthor :: UserId -> m Bool
  , insertReturnAuthor :: UserId -> AuthorInfo -> m Id
  , withTransactionDB :: forall a. m a -> m a
  , hDelMany :: Methods.Common.DeleteMany.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectDraftsForAuthor' conn)
        (selectAuthorsForUser' conn)
        (selectAuthors' conn)
        (updateAuthorForPosts' conn)
        (deleteAuthor' conn)
        (isExistAuthor' conn)
        (isExistUser' conn)
        (isUserAuthor' conn)
        (insertReturnAuthor' conn)
        (withTransaction conn)
        (Methods.Common.DeleteMany.makeH conf)

selectDraftsForAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  selectOnly' conn (Select ["draft_id"] "drafts" wh)
selectAuthorsForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  selectOnly' conn (Select ["author_id"] "authors" wh)
selectAuthors' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  select' conn (Select ["author_id", "author_info", "user_id"] "authors" wh)
updateDbAuthor' conn usId auInfo auId = do
  let set1 = SetPair "user_id=?" (Id usId)
  let set2 = SetPair "author_info=?" (Txt auInfo)
  let wh = WherePair "author_id=?" (Id auId)
  updateInDb' conn (Update "posts" [set1,set2] wh)
updateDbAuthorForPosts' conn auIdNew auId = do
  let set = SetPair "author_id=?" (Id auIdNew)
  let wh = WherePair "author_id=?" (Id auId)
  updateInDb' conn (Update "posts" [set] wh)
deleteDbAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  deleteFromDb' conn (Delete "authors" wh)
isExistAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  isExistInDb' conn (Exists "authors" wh)
isExistUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  isExistInDb' conn (Exists "users" wh)
isUserAuthor' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  isExistInDb' conn (Exists "authors" wh)
insertReturnAuthor' conn usId auInfo = do
  let insPair1 = InsertPair "user_id"     (Id  usId)
  let insPair2 = InsertPair "author_info" (Txt  auInfo)
  let insPairs = [insPair1,insPair2]
  insertReturn' conn (InsertRet "authors" insPairs "author_id")



createAuthor :: (MonadCatch m) => Handle m -> CreateAuthor -> ExceptT ReqError m ResponseInfo
createAuthor h@Handle{..} (CreateAuthor usIdParam auInfoParam) = do
  let logpair = ("user_id", usIdParam)
  catchExistE hLog logpair $ isExistUser usIdParam
  isNotAlreadyAuthor h usIdParam
  auId <- catchInsRetE hLog $ insertReturnAuthor usIdParam auInfoParam
  lift $ logDebug hLog $ "DB return author_id" ++ show auId
  lift $ logInfo hLog $ "Author_id: " ++ show auId ++ " created"
  okHelper $ AuthorResponse {author_id = auId, auth_user_id = usIdParam, author_info = auInfoParam}

getAuthor :: (MonadCatch m) => Handle m -> GetAuthor -> ExceptT ReqError m ResponseInfo
getAuthor Handle{..} (GetAuthor auIdParam) = do
  let logpair = ("author_id", auIdParam)
  Author auId auInfo usId <- checkOneSelIfExistE hLog logpair $ selectAuthors auIdParam
  lift $ logInfo hLog $ "Author_id: " ++ show auId ++ " sending in response."
  okHelper $ AuthorResponse {author_id = auId, auth_user_id = usId, author_info = auInfo}

updateAuthor :: (MonadCatch m) => Handle m -> UpdateAuthor -> ExceptT ReqError m ResponseInfo
updateAuthor h@Handle{..} (UpdateAuthor auIdParam usIdParam auInfoParam) = do
  let logpair1 = ("user_id", usIdParam)
  catchExistE hLog logpair1 $ isExistUser usIdParam
  let logpair2 = ("author_id", auIdParam)
  catchExistE hLog logpair2 $ isExistAuthor auIdParam
  isntUserOtherAuthor h usIdParam auIdParam
  catchUpdE hLog $ updateDbAuthor usIdParam auInfoParam auIdParam
  lift $ logInfo hLog $ "Author_id: " ++ show auIdParam ++ " updated."
  okHelper $ AuthorResponse {author_id = auIdParam, auth_user_id = usIdParam, author_info = auInfoParam}

deleteAuthor :: (MonadCatch m) => Handle m -> DeleteAuthor -> ExceptT ReqError m ResponseInfo
deleteAuthor Handle{..} (DeleteAuthor auIdParam) = do
  let logpair = ("author_id", auIdParam)
  catchExistE hLog logpair $ isExistAuthor auIdParam
  let updatePos = updateDbAuthorForPosts (cDefAuthId hConf) auIdParam
  draftsIds <- catchSelE hLog $ selectDraftsForAuthor auIdParam
  let deleteDr = deleteAllAboutDrafts hDelMany draftsIds
  let deleteAu = deleteDbAuthor auIdParam
  withTransactionDBE h (updatePos >> deleteDr >> deleteAu)
  lift $ logInfo hLog $ "Author_id: " ++ show auIdParam ++ " deleted."
  okHelper $ OkResponse {ok = True}

isntUserOtherAuthor :: (MonadCatch m) => Handle m -> UserId -> AuthorId -> ExceptT ReqError m ()
isntUserOtherAuthor Handle{..} usIdParam auIdParam = do
  maybeAuId <- catchMaybeOneSelE hLog $ selectAuthorsForUser usIdParam
  case maybeAuId of
    Just auId ->
      if auId == auIdParam
        then return ()
        else throwE $ SimpleError $ "user_id: " ++ show usIdParam ++ " is already author"
    Nothing -> return ()


isNotAlreadyAuthor :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ()
isNotAlreadyAuthor Handle{..} usId = do
  lift $ logDebug hLog  $ "Checking is user already in authors in DB"
  isExist <- catchDbErr $ lift $ isUserAuthor usId
  when isExist $
      throwE $ SimpleError $ "User is already author. User_id: " ++ show usId


withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactE (hLog h) . withTransactionDB h
