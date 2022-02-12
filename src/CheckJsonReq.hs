{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module CheckJsonReq (checkDraftReqJson,pullTokenDraftReqJson) where
          
import           Api.Request (DraftRequest(..))
import           Oops (ReqError(..),hideTokenErr)
import           Data.Aeson (Object,Value(..),decode)
import           Data.Text                      (  unpack, Text )
import           Control.Monad.Trans.Except (ExceptT,throwE)
import qualified Data.ByteString.Lazy           as BSL
import           Control.Monad.Catch            ( MonadCatch)
import           Data.HashMap.Strict            ( toList )
import qualified Data.Vector                    as V
import ParseQueryStr (checkLength)
import TryRead (checkBigInt)




checkDraftReqJson :: (MonadCatch m) => BSL.ByteString -> ExceptT ReqError m DraftRequest
checkDraftReqJson json =  
  case (decode json :: Maybe DraftRequest) of
    Just body@(DraftRequest token nameParam catIdParam txtParam picId picsIds tagsIds) -> do
      checkTokenLength 100 token
      _ <- checkLength 50 "draft_name" nameParam
      _ <- checkLength 10000 "draft_text" txtParam
      _ <- checkBigInt "draft_category_id" catIdParam
      _ <- checkBigInt "draft_main_pic_id" picId
      mapM_ (checkBigInt "draft_tags_ids") tagsIds
      mapM_ (checkBigInt "draft_pics_ids") picsIds
      return body
    Nothing -> whyBadDraftReq json

whyBadDraftReq :: (MonadCatch m) => BSL.ByteString -> ExceptT ReqError m a
whyBadDraftReq json =  
  case (decode json :: Maybe Object) of
      Just obj -> do
        mapM_ (checkTxt obj) ["token","draft_name","draft_text"]
        mapM_ (checkNum obj) ["draft_category_id","draft_main_pic_id"]
        mapM_ (checkNumArr obj) ["draft_tags_ids","draft_pics_ids"]
        throwE $ SimpleError  "Can`t parse request body"
      Nothing -> throwE $ SimpleError  "Invalid request body"

pullTokenDraftReqJson :: (MonadCatch m) => BSL.ByteString -> ExceptT ReqError m Text
pullTokenDraftReqJson json = 
  case (decode json :: Maybe DraftRequest) of
    Just (DraftRequest token _ _ _ _ _ _) -> do
      checkTokenLength 100 token
      return token
    Nothing -> pullTokenJson json

pullTokenJson :: (MonadCatch m) => BSL.ByteString -> ExceptT ReqError m Text
pullTokenJson json = 
  case (decode json :: Maybe Object) of
    Just obj -> do
      token <- hideTokenErr $ checkTxt obj "token"
      checkTokenLength 100 token
      return token
    Nothing -> throwE $ SecretTokenError "Can`t parse token from request body"

checkNum :: (MonadCatch m) => Object -> Text -> ExceptT ReqError m ()
checkNum obj paramKey = do
  val <- isExistInObj obj paramKey
  checkNumVal val

checkNumArr :: (MonadCatch m) => Object -> Text -> ExceptT ReqError m ()
checkNumArr obj paramKey = do
  val <- isExistInObj obj paramKey
  checkNumArrVal val

checkTxt :: (MonadCatch m) => Object -> Text -> ExceptT ReqError m Text
checkTxt obj paramKey = do
  val <- isExistInObj obj paramKey
  checkTxtVal val



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

checkTokenLength :: (Monad m) => Int -> Text -> ExceptT ReqError m ()
checkTokenLength leng txt = do
  if (length . unpack $ txt) > leng
    then throwE $ SecretTokenError $ "Token too long. Maximum length should be: " ++ show leng
    else return ()