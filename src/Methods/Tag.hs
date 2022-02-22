


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
    selectTxts :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Text],
    updateInDb :: Table -> ToUpdate -> Where -> [DbValue] -> m (),
    deleteFromDb :: Table -> Where -> [DbValue] -> m (),
    isExistInDb :: Table -> Where -> DbValue -> m Bool,
    insertReturn :: Table -> DbReturnParamKey -> [DbInsertParamKey] -> [DbValue] -> m Integer,
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
  tagId <- insertReturnE h "tags" "tag_id" ["tag_name"] [Txt tagNameParam]
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagId ++ " created"
  okHelper $ TagResponse tagId tagNameParam

getTag :: (Monad m, MonadCatch m) => Handle m -> TagId -> ExceptT ReqError m ResponseInfo
getTag h tagIdNum = do
  tagName <- checkOneIfExistE (hLog h) (selectTxts h) "tags" ["tag_name"] "tag_id=?" (Num tagIdNum)
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " sending in response"
  okHelper $ TagResponse tagIdNum tagName

updateTag :: (Monad m, MonadCatch m) => Handle m -> UpdateTag -> ExceptT ReqError m ResponseInfo
updateTag h (UpdateTag tagIdNum tagNameParam) = do
  isExistInDbE h "tags" "tag_id=?" (Num tagIdNum)
  updateInDbE h "tags" "tag_name=?" "tag_id=?" [Txt tagNameParam,Num tagIdNum]
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " updated"
  okHelper $ TagResponse tagIdNum tagNameParam

deleteTag :: (Monad m, MonadCatch m) => Handle m -> DeleteTag -> ExceptT ReqError m ResponseInfo
deleteTag h (DeleteTag tagIdNum) = do
  isExistInDbE h "tags" "tag_id=?" (Num tagIdNum)
  let deleteDrTg = deleteFromDb h "draftstags" "tag_id=?" [Num tagIdNum]
  let deletePosTg = deleteFromDb h "poststags" "tag_id=?" [Num tagIdNum]
  let deleteTg = deleteFromDb h "tags" "tag_id=?" [Num tagIdNum]
  withTransactionDBE h (deleteDrTg >> deletePosTg >> deleteTg)
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " deleted"
  okHelper $ OkResponse {ok = True}

updateInDbE :: (MonadCatch m) => Handle m -> Table -> Set -> Where -> [DbValue] -> ExceptT ReqError m ()
updateInDbE h t s w values = checkUpdE (hLog h) $ updateInDb h t s w values

isExistInDbE :: (MonadCatch m) => Handle m -> Table -> Where -> DbValue -> ExceptT ReqError m ()
isExistInDbE h = checkIsExistE (hLog h) (isExistInDb h)

insertReturnE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> [DbValue] -> ExceptT ReqError m Integer
insertReturnE h = checkInsRetE (hLog h) (insertReturn h)

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h
