{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text (Text, pack, unpack)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutDrafts)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Methods.Common.Select (Author (..))
import Oops
import ParseQueryStr (CreateAuthor (..), DeleteAuthor (..), GetAuthor (..), UpdateAuthor (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectNums :: Table -> [DbSelectParamKey] -> Where -> [DbParamValue] -> m [Id],
    selectAuthors :: Table -> [DbSelectParamKey] -> Where -> [DbParamValue] -> m [Author],
    updateInDb :: Table -> ToUpdate -> Where -> [DbParamValue] -> m (),
    deleteFromDb :: Table -> Where -> [DbParamValue] -> m (),
    isExistInDb :: Table -> Where -> DbParamValue -> m Bool,
    insertReturn :: Table -> DbReturnParamKey -> [DbInsertParamKey] -> [DbParamValue] -> m Integer,
    withTransactionDB :: forall a. m a -> m a,
    hDelMany :: Methods.Common.DeleteMany.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectOnly' conn)
        (select' conn)
        (updateInDb' conn)
        (deleteFromDb' conn)
        (isExistInDb' conn)
        (insertReturn' conn)
        (withTransaction conn)
        (Methods.Common.DeleteMany.makeH conf)

createAuthor :: (MonadCatch m) => Handle m -> CreateAuthor -> ExceptT ReqError m ResponseInfo
createAuthor h (CreateAuthor usIdNum auInfoParam) = do
  let usIdParam = numToTxt usIdNum
  isExistInDbE h "users" "user_id=?" usIdParam
  isNotAlreadyAuthor h usIdNum
  auId <- insertReturnE h "authors" "author_id" ["user_id", "author_info"] [usIdParam, auInfoParam]
  lift $ logDebug (hLog h) $ "DB return author_id" ++ show auId
  lift $ logInfo (hLog h) $ "Author_id: " ++ show auId ++ " created"
  okHelper $ AuthorResponse {author_id = auId, auth_user_id = usIdNum, author_info = auInfoParam}

getAuthor :: (MonadCatch m) => Handle m -> GetAuthor -> ExceptT ReqError m ResponseInfo
getAuthor h (GetAuthor auIdNum) = do
  let auIdParam = numToTxt auIdNum
  Author auId auInfo usId <- checkOneIfExistE (hLog h) (selectAuthors h) "authors" ["author_id", "author_info", "user_id"] "author_id=?" auIdParam
  lift $ logInfo (hLog h) $ "Author_id: " ++ show auId ++ " sending in response."
  okHelper $ AuthorResponse {author_id = auId, auth_user_id = usId, author_info = auInfo}

updateAuthor :: (MonadCatch m) => Handle m -> UpdateAuthor -> ExceptT ReqError m ResponseInfo
updateAuthor h (UpdateAuthor auIdNum usIdNum auInfoParam) = do
  let usIdParam = numToTxt usIdNum
  let auIdParam = numToTxt auIdNum
  isExistInDbE h "users"  "user_id=?" usIdParam
  isExistInDbE h "authors"  "author_id=?" auIdParam
  isntUserOtherAuthor h usIdNum auIdNum
  updateInDbE h "authors" "author_info=?,user_id=?" "author_id=?" [auInfoParam, usIdParam, auIdParam]
  lift $ logInfo (hLog h) $ "Author_id: " ++ show auIdNum ++ " updated."
  okHelper $ AuthorResponse {author_id = auIdNum, auth_user_id = usIdNum, author_info = auInfoParam}

deleteAuthor :: (MonadCatch m) => Handle m -> DeleteAuthor -> ExceptT ReqError m ResponseInfo
deleteAuthor h (DeleteAuthor auIdNum) = do
  let auIdParam = numToTxt auIdNum
  isExistInDbE h "authors" "author_id=?" auIdParam
  let updatePos = updateInDb h "posts" "author_id=?" "author_id=?" [pack . show $ cDefAuthId (hConf h), auIdParam]
  draftsIds <- checkListE (hLog h) $ selectNums h "drafts" ["draft_id"] "author_id=?" [auIdParam]
  let deleteDr = deleteAllAboutDrafts (hDelMany h) draftsIds
  let deleteAu = deleteFromDb h "authors" "author_id=?" [auIdParam]
  withTransactionDBE h (updatePos >> deleteDr >> deleteAu)
  lift $ logInfo (hLog h) $ "Author_id: " ++ show auIdNum ++ " deleted."
  okHelper $ OkResponse {ok = True}

isntUserOtherAuthor :: (MonadCatch m) => Handle m -> UserId -> AuthorId -> ExceptT ReqError m ()
isntUserOtherAuthor h usIdNum auIdNum = do
  let usIdParam = pack . show $ usIdNum
  maybeAuId <- checkMaybeOneE (hLog h) $ selectNums h "authors" ["author_id"] "user_id=?" [usIdParam]
  case maybeAuId of
    Just auId ->
      if auId == auIdNum
        then return ()
        else throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is already author"
    Nothing -> return ()


isNotAlreadyAuthor :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ()
isNotAlreadyAuthor h usIdNum = do
  lift $ logDebug (hLog h) $ "Checking is user already in authors in DB"
  isExist <- catchDbErr $ lift $ isExistInDb h "authors" "user_id=?" (numToTxt usIdNum)
  when isExist $
      throwE $ SimpleError $ "User is already author. User_id: " ++ show usIdNum

isExistInDbE :: (MonadCatch m) => Handle m -> Table -> Where -> DbParamValue -> ExceptT ReqError m ()
isExistInDbE h = checkIsExistE (hLog h) (isExistInDb h)

updateInDbE :: (MonadCatch m) => Handle m -> Table -> Set -> Where -> [Text] -> ExceptT ReqError m ()
updateInDbE h t s w values = checkUpdE (hLog h) $ updateInDb h t s w values

insertReturnE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> [Text] -> ExceptT ReqError m Integer
insertReturnE h = checkInsRetE (hLog h) (insertReturn h)

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h
