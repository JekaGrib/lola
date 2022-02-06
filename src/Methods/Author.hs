--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}




module Methods.Author where
          
import           Api.Response
import           Logger
import           Types
import           Oops
import           Methods.Handle
import Methods.Handle.Select (Author(..))
import ParseQueryStr (CreateAuthor(..),GetAuthor(..),UpdateAuthor(..),DeleteAuthor(..))
import Conf (Config(..))
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Data.Text                      ( pack, unpack, Text )
import           Database.PostgreSQL.Simple (query, withTransaction, execute, executeMany,Connection,Only(..),Binary(Binary))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( catch, throwM, MonadCatch)
import Methods.Post (deleteAllAboutDrafts)


createAuthor :: (MonadCatch m) => MethodsHandle m -> CreateAuthor -> ExceptT ReqError m ResponseInfo
createAuthor h (CreateAuthor usIdNum auInfoParam) = do
  let usIdParam = numToTxt usIdNum
  isExistInDbE h  "users" "user_id"  "user_id=?" [usIdParam] 
  ifExistInDbThrowE h "authors" "user_id" "user_id=?" [usIdParam] 
  auId <- insertReturnE h "authors" "author_id" ["user_id","author_info"] [usIdParam,auInfoParam]
  lift $ logDebug (hLog h) $ "DB return author_id" ++ show auId
  lift $ logInfo (hLog h) $ "Author_id: " ++ show auId ++ " created"
  okHelper $ AuthorResponse {author_id = auId, auth_user_id = usIdNum, author_info = auInfoParam}
  
getAuthor :: (MonadCatch m) => MethodsHandle m -> GetAuthor -> ExceptT ReqError m ResponseInfo 
getAuthor h (GetAuthor auIdNum) = do
  let auIdParam = numToTxt auIdNum
  Author auId auInfo usId <- selectOneIfExistE h "authors" ["author_id","author_info","user_id"] "author_id=?" auIdParam
  okHelper $ AuthorResponse {author_id = auIdNum, auth_user_id = usId, author_info = auInfo}
  
updateAuthor :: (MonadCatch m) => MethodsHandle m -> UpdateAuthor -> ExceptT ReqError m ResponseInfo 
updateAuthor h (UpdateAuthor auIdNum usIdNum auInfoParam) = do
  let usIdParam = numToTxt usIdNum
  let auIdParam = numToTxt auIdNum
  isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
  isExistInDbE h "authors" "author_id" "author_id=?" [auIdParam] 
  isntUserOtherAuthor h usIdNum auIdNum
  updateInDbE h "authors" "author_info=?,user_id=?" "author_id=?" [auInfoParam,usIdParam,auIdParam]
  okHelper $ AuthorResponse {author_id = auIdNum, auth_user_id = usIdNum, author_info = auInfoParam}

deleteAuthor :: (MonadCatch m) => MethodsHandle m -> DeleteAuthor -> ExceptT ReqError m ResponseInfo 
deleteAuthor h (DeleteAuthor auIdNum) = do
  let auIdParam = numToTxt auIdNum
  isExistInDbE h "authors" "author_id" "author_id=?" [auIdParam] 
  let updatePos = updateInDb h "posts" "author_id=?" "author_id=?" [pack . show $ (cDefAuthId $ hConf h),auIdParam]
  onlyDraftsIds <- selectListFromDbE h "drafts" ["draft_id"] "author_id=?" [auIdParam]
  let draftsIds = fmap fromOnly onlyDraftsIds
  let deleteDr = deleteAllAboutDrafts h  draftsIds
  let deleteAu = deleteFromDb h "authors" "author_id=?" [auIdParam]
  withTransactionDBE h (updatePos >> deleteDr >> deleteAu)
  okHelper $ OkResponse {ok = True}

isntUserOtherAuthor :: (MonadCatch m) => MethodsHandle m -> UserId -> AuthorId -> ExceptT ReqError m ()
isntUserOtherAuthor h usIdNum auIdNum = do
  let usIdParam = pack . show $ usIdNum
  maybeAuId <- selectMaybeOneE h "authors" ["author_id"] "user_id=?" [usIdParam]
  case maybeAuId of
    Just (Only auId) -> if auId == auIdNum 
      then return ()
      else throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is already author"
    Nothing -> return ()