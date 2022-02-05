--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}




module Methods.Tag where
          
import           Api.Response
import           Logger
import           Types
import           Oops
import           Methods.Handle
import ParseQueryStr 
import Conf (Config(..))
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Data.Text                      ( pack, unpack, Text )
import           Database.PostgreSQL.Simple (query, withTransaction, execute, executeMany,Connection,Only(..),Binary(Binary))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( catch, throwM, MonadCatch)


createTag :: (Monad m,MonadCatch m) => MethodsHandle m -> CreateTag -> ExceptT ReqError m ResponseInfo
createTag h (CreateTag tagNameParam) = do
  tagId <-  insertReturnE h "tags" "tag_id" ["tag_name"] [tagNameParam] 
  okHelper $ TagResponse tagId tagNameParam
  
getTag :: (Monad m,MonadCatch m) => MethodsHandle m -> TagId -> ExceptT ReqError m ResponseInfo 
getTag h tagIdNum = do
  Only tagName <- selectOneIfExistE h "tags" ["tag_name"] "tag_id=?" (numToTxt tagIdNum)
  okHelper $ TagResponse tagIdNum tagName
  
updateTag :: (Monad m,MonadCatch m) => MethodsHandle m -> UpdateTag -> ExceptT ReqError m ResponseInfo 
updateTag h (UpdateTag tagIdNum tagNameParam) = do
  let tagIdParam = numToTxt tagIdNum
  isExistInDbE h "tags" "tag_id" "tag_id=?" [tagIdParam] 
  updateInDbE h "tags" "tag_name=?" "tag_id=?" [tagNameParam,tagIdParam]
  okHelper $ TagResponse tagIdNum tagNameParam

deleteTag :: (Monad m,MonadCatch m) => MethodsHandle m -> DeleteTag -> ExceptT ReqError m ResponseInfo 
deleteTag h (DeleteTag tagIdNum) = do
  let tagIdParam = numToTxt tagIdNum
  isExistInDbE h "tags" "tag_id" "tag_id=?" [tagIdParam]
  let deleteDrTg = deleteFromDb h "draftstags" "tag_id=?" [tagIdParam] 
  let deletePosTg = deleteFromDb h "poststags" "tag_id=?" [tagIdParam] 
  let deleteTg = deleteFromDb h "tags" "tag_id=?" [tagIdParam]
  withTransactionDBE h (deleteDrTg >> deletePosTg >> deleteTg)
  okHelper $ OkResponse {ok = True}

