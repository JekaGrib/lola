{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

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
import Api.Request.QueryStr (CreateAuthor (..), UpdateAuthor (..),checkQStr)
import Types
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth,tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE,UncheckedExId(..))
import Methods.Common.ToQuery
import Network.HTTP.Types (StdMethod(..),QueryText)
import TryRead (tryReadResourseId)
import Api.Request.EndPoint


data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , selectDraftsForAuthor :: AuthorId -> m [DraftId]
  , selectAuthorsForUser  :: UserId -> m [AuthorId]
  , selectAuthors :: AuthorId -> m [Author]
  , updateDbAuthor :: UserId -> AuthorInfo -> AuthorId -> m ()
  , updateDbAuthorForPosts :: AuthorId -> AuthorId -> m ()
  , deleteDbAuthor :: AuthorId -> m ()
  , isUserAuthor :: UserId -> m Bool
  , insertReturnAuthor :: UserId -> AuthorInfo -> m Id
  , withTransactionDB :: forall a. m a -> m a
  , hDelMany :: Methods.Common.DeleteMany.Handle m
  , hAuth :: Methods.Common.Auth.Handle m
  , hExist :: Methods.Common.Exist.Handle m
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
        (updateDbAuthor' conn)
        (updateDbAuthorForPosts' conn)
        (deleteDbAuthor' conn)
        (isUserAuthor' conn)
        (insertReturnAuthor' conn)
        (withTransaction conn)
        (Methods.Common.DeleteMany.makeH conf)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)

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
  updateInDb' conn (Update "authors" [set1,set2] wh)
updateDbAuthorForPosts' conn auIdNew auId = do
  let set = SetPair "author_id=?" (Id auIdNew)
  let wh = WherePair "author_id=?" (Id auId)
  updateInDb' conn (Update "posts" [set] wh)
deleteDbAuthor' conn auId = do
  let wh = WherePair "author_id=?" (Id auId)
  deleteFromDb' conn (Delete "authors" wh)
isUserAuthor' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  isExistInDb' conn (Exists "authors" wh)
insertReturnAuthor' conn usId auInfo = do
  let insPair1 = InsertPair "user_id"     (Id  usId)
  let insPair2 = InsertPair "author_info" (Txt  auInfo)
  let insPairs = [insPair1,insPair2]
  insertReturn' conn (InsertRet "authors" insPairs "author_id")

workWithAuthors :: (MonadCatch m) => Handle m -> QueryText -> AppMethod -> ExceptT ReqError m ResponseInfo
workWithAuthors h@Handle{..} qStr meth  = 
  case meth of
    ToPost -> do
      lift $ logInfo hLog "Create author command"
      tokenAdminAuth hAuth  qStr
      checkQStr hExist qStr >>= createAuthor h
    ToGet auId -> do
      lift $ logInfo hLog "Get author command"
      tokenAdminAuth hAuth  qStr
      isExistResourseE hExist (AuthorId auId)
      getAuthor h auId
    ToPut auId -> do
      lift $ logInfo hLog "Update author command"
      tokenAdminAuth hAuth qStr
      isExistResourseE hExist (AuthorId auId)
      checkQStr hExist qStr >>= updateAuthor h auId
    ToDelete auId -> do
      lift $ logInfo hLog "Delete author command"
      tokenAdminAuth hAuth qStr
      isExistResourseE hExist (AuthorId auId)
      deleteAuthor h auId
    _ -> throwE $ ResourseNotExistError $ "Wrong method for authors resourse: "  ++ show meth


createAuthor :: (MonadCatch m) => Handle m -> CreateAuthor -> ExceptT ReqError m ResponseInfo
createAuthor h@Handle{..} (CreateAuthor usIdParam auInfoParam) = do
  isNotAlreadyAuthor h usIdParam
  auId <- catchInsRetE hLog $ insertReturnAuthor usIdParam auInfoParam
  lift $ logDebug hLog $ "DB return author_id" ++ show auId
  lift $ logInfo hLog $ "Author_id: " ++ show auId ++ " created"
  okHelper $ AuthorResponse {author_id = auId, auth_user_id = usIdParam, author_info = auInfoParam}

getAuthor :: (MonadCatch m) => Handle m -> AuthorId -> ExceptT ReqError m ResponseInfo
getAuthor Handle{..} authId = do
  Author auId auInfo usId <- catchOneSelE hLog $ selectAuthors authId
  lift $ logInfo hLog $ "Author_id: " ++ show auId ++ " sending in response."
  okHelper $ AuthorResponse {author_id = auId, auth_user_id = usId, author_info = auInfo}

updateAuthor :: (MonadCatch m) => Handle m -> AuthorId -> UpdateAuthor -> ExceptT ReqError m ResponseInfo
updateAuthor h@Handle{..} auId (UpdateAuthor usIdParam auInfoParam) = do
  isntUserOtherAuthor h usIdParam auId
  catchUpdE hLog $ updateDbAuthor usIdParam auInfoParam auId
  lift $ logInfo hLog $ "Author_id: " ++ show auId ++ " updated."
  okHelper $ AuthorResponse {author_id = auId, auth_user_id = usIdParam, author_info = auInfoParam}

deleteAuthor :: (MonadCatch m) => Handle m -> AuthorId -> ExceptT ReqError m ResponseInfo
deleteAuthor h@Handle{..} auId = do
  let updatePos = updateDbAuthorForPosts (cDefAuthId hConf) auId
  draftsIds <- catchSelE hLog $ selectDraftsForAuthor auId
  let deleteDr = deleteAllAboutDrafts hDelMany draftsIds
  let deleteAu = deleteDbAuthor auId
  withTransactionDBE h (updatePos >> deleteDr >> deleteAu)
  lift $ logInfo hLog $ "Author_id: " ++ show auId ++ " deleted."
  okHelper $ OkResponse {ok = True}

isntUserOtherAuthor :: (MonadCatch m) => Handle m -> UserId -> AuthorId -> ExceptT ReqError m ()
isntUserOtherAuthor Handle{..} usId auId = do
  maybeAuId <- catchMaybeOneSelE hLog $ selectAuthorsForUser usId
  case maybeAuId of
    Just usAuId ->
      if usAuId == auId
        then return ()
        else throwE $ BadReqError $ "User_id: " ++ show usId ++ " is already other author"
    Nothing -> return ()


isNotAlreadyAuthor :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ()
isNotAlreadyAuthor Handle{..} usId = do
  lift $ logDebug hLog  $ "Checking is user already in authors in DB"
  isEx <- catchDbErr $ lift $ isUserAuthor usId
  when isEx $
      throwE $ BadReqError $ "User is already author."



withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactE (hLog h) . withTransactionDB h
