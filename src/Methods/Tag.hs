{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Tag where

import Api.Response (OkResponse (..), TagResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Common
import Oops
import ParseQueryStr (CreateTag (..), DeleteTag (..), UpdateTag (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectTxts :: Table -> [DbSelectParamKey] -> Where -> [DbParamValue] -> m [Text],
    updateInDb :: Table -> ToUpdate -> Where -> [DbParamValue] -> m (),
    deleteFromDb :: Table -> Where -> [DbParamValue] -> m (),
    isExistInDb :: Table -> Where -> DbParamValue -> m Bool,
    insertReturn :: Table -> DbReturnParamKey -> [DbInsertParamKey] -> [DbParamValue] -> m Integer,
    withTransactionDB :: forall a. m a -> m a
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectOnly' conn)
        (updateInDb' conn)
        (deleteFromDb' conn)
        (isExistInDb' conn)
        (insertReturn' conn)
        (withTransaction conn)

createTag :: (Monad m, MonadCatch m) => Handle m -> CreateTag -> ExceptT ReqError m ResponseInfo
createTag h (CreateTag tagNameParam) = do
  tagId <- insertReturnE h "tags" "tag_id" ["tag_name"] [tagNameParam]
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagId ++ " created"
  okHelper $ TagResponse tagId tagNameParam

getTag :: (Monad m, MonadCatch m) => Handle m -> TagId -> ExceptT ReqError m ResponseInfo
getTag h tagIdNum = do
  tagName <- checkOneIfExistE (hLog h) (selectTxts h) "tags" ["tag_name"] "tag_id=?" (numToTxt tagIdNum)
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " sending in response"
  okHelper $ TagResponse tagIdNum tagName

updateTag :: (Monad m, MonadCatch m) => Handle m -> UpdateTag -> ExceptT ReqError m ResponseInfo
updateTag h (UpdateTag tagIdNum tagNameParam) = do
  let tagIdParam = numToTxt tagIdNum
  isExistInDbE h "tags" "tag_id=?" tagIdParam
  updateInDbE h "tags" "tag_name=?" "tag_id=?" [tagNameParam, tagIdParam]
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " updated"
  okHelper $ TagResponse tagIdNum tagNameParam

deleteTag :: (Monad m, MonadCatch m) => Handle m -> DeleteTag -> ExceptT ReqError m ResponseInfo
deleteTag h (DeleteTag tagIdNum) = do
  let tagIdParam = numToTxt tagIdNum
  isExistInDbE h "tags" "tag_id=?" tagIdParam
  let deleteDrTg = deleteFromDb h "draftstags" "tag_id=?" [tagIdParam]
  let deletePosTg = deleteFromDb h "poststags" "tag_id=?" [tagIdParam]
  let deleteTg = deleteFromDb h "tags" "tag_id=?" [tagIdParam]
  withTransactionDBE h (deleteDrTg >> deletePosTg >> deleteTg)
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " deleted"
  okHelper $ OkResponse {ok = True}

updateInDbE :: (MonadCatch m) => Handle m -> Table -> Set -> Where -> [Text] -> ExceptT ReqError m ()
updateInDbE h t s w values = checkUpdE (hLog h) $ updateInDb h t s w values

isExistInDbE :: (MonadCatch m) => Handle m -> Table -> Where -> DbParamValue -> ExceptT ReqError m ()
isExistInDbE h = checkIsExistE (hLog h) (isExistInDb h)

insertReturnE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> [Text] -> ExceptT ReqError m Integer
insertReturnE h = checkInsRetE (hLog h) (insertReturn h)

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h
