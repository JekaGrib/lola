{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module CheckJsonReq (checkDraftReqJson) where
          
import           Api.Request (DraftRequest)
import           Oops (ReqError(..))
import           Data.Aeson (Object,Value(..),decode)
import           Data.Text                      (  unpack, Text )
import           Control.Monad.Trans.Except (ExceptT,throwE)
import qualified Data.ByteString.Lazy           as BSL
import           Control.Monad.Catch            ( MonadCatch)
import           Data.HashMap.Strict            ( toList )
import qualified Data.Vector                    as V
import ParseQueryStr (checkLength,checkSecretLength)

checkDraftReqJson :: (MonadCatch m) => BSL.ByteString -> ExceptT ReqError m DraftRequest
checkDraftReqJson json =  
  case (decode json :: Maybe DraftRequest) of
    Just body -> return body
    Nothing   -> case (decode json :: Maybe Object) of
      Just obj -> do
        checkSecretLengTxt obj 50 "token"
        checkTxt obj 50 "draft_name"
        checkTxt obj 10000 "draft_text"
        mapM_ (checkNum obj) ["draft_category_id","draft_main_pic_id"]
        mapM_ (checkNumArr obj) ["draft_tags_ids","draft_pics_ids"]
        throwE $ SimpleError  "Can`t parse request body"
      Nothing -> throwE $ SimpleError  "Invalid request body"

checkNum :: (MonadCatch m) => Object -> Text -> ExceptT ReqError m ()
checkNum obj paramKey = do
  val <- isExistInObj obj paramKey
  checkNumVal val

checkNumArr :: (MonadCatch m) => Object -> Text -> ExceptT ReqError m ()
checkNumArr obj paramKey = do
  val <- isExistInObj obj paramKey
  checkNumArrVal val

checkTxt :: (MonadCatch m) => Object -> Int ->  Text -> ExceptT ReqError m ()
checkTxt obj leng paramKey = do
  val <- isExistInObj obj paramKey
  txt <- checkTxtVal val
  _ <- checkLength leng paramKey txt
  return ()


checkSecretLengTxt ::  (MonadCatch m) => Object -> Int ->  Text -> ExceptT ReqError m ()
checkSecretLengTxt obj leng paramKey = do
  val <- isExistInObj obj paramKey
  txt <- checkTxtVal val
  _ <- checkSecretLength leng paramKey txt
  return ()


isExistInObj :: (MonadCatch m) => Object -> Text -> ExceptT ReqError m Value
isExistInObj obj paramKey = 
  case lookup paramKey . toList $ obj of
    Just val -> return val
    Nothing -> throwE $ SimpleError $ "Can`t find parameter: " ++ unpack paramKey

checkNumVal :: (MonadCatch m) => Value -> ExceptT ReqError m ()
checkNumVal val = 
  case val of
    Number _ -> return ()
    _ -> throwE $ SimpleError $ "Can`t parse parameter value: " ++ show val ++ ". It should be number"

checkTxtVal :: (MonadCatch m) => Value -> ExceptT ReqError m Text
checkTxtVal val = 
  case val of
    String txt -> return txt
    _ -> throwE $ SimpleError $ "Can`t parse parameter value: " ++ show val ++ ". It should be text"


checkNumArrVal :: (MonadCatch m) => Value -> ExceptT ReqError m ()
checkNumArrVal values = 
  case values of
    Array arr -> case V.toList arr of
      [] -> return ()
      (Number _ : _) -> return ()
      _ -> throwE $ SimpleError $ "Can`t parse parameter values: " ++ show values ++ ". It should be number array"
    _ -> throwE $ SimpleError $ "Can`t parse parameter values: " ++ show values ++ ". It should be number array"