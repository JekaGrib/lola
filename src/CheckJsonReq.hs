{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module CheckJsonReq (checkDraftReqJson, pullTokenDraftReqJson) where

import Api.Request (DraftRequest (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson (Object, Value (..), decode)
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict (toList)
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Oops (ReqError (..), hideTokenErr)
import ParseQueryStr (checkLength)
import TryRead (checkBigInt)
import Types

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
      mapM_ (checkTxt obj) ["token", "draft_name", "draft_text"]
      mapM_ (checkNum obj) ["draft_category_id", "draft_main_pic_id"]
      mapM_ (checkNumArr obj) ["draft_tags_ids", "draft_pics_ids"]
      throwE $ SimpleError "Invalid request body"
    Nothing -> throwE $ SimpleError "Invalid request body"

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
    Nothing -> throwE $ SimpleError "Invalid request body"

checkNum :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m ()
checkNum obj paramKey = do
  val <- isExistInObj obj paramKey
  checkNumVal paramKey val

checkNumArr :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m ()
checkNumArr obj paramKey = do
  val <- isExistInObj obj paramKey
  checkNumArrVal paramKey val

checkTxt :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m Text
checkTxt obj paramKey = do
  val <- isExistInObj obj paramKey
  checkTxtVal paramKey val

isExistInObj :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m Value
isExistInObj obj paramKey =
  case lookup paramKey . toList $ obj of
    Just val -> return val
    Nothing -> throwE $ SimpleError $ "Can`t find parameter: " ++ unpack paramKey

checkNumVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m ()
checkNumVal paramKey val =
  case val of
    Number _ -> return ()
    _ -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be number"

checkTxtVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m Text
checkTxtVal paramKey val =
  case val of
    String txt -> return txt
    _ -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be text"

checkNumArrVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m ()
checkNumArrVal paramKey values =
  case values of
    Array arr -> case V.toList arr of
      [] -> return ()
      (Number _ : _) -> return ()
      _ -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be number array. Example: [1,5,8]"
    _ -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be number array. Example: [1,5,8]"

checkTokenLength :: (Monad m) => Int -> Text -> ExceptT ReqError m ()
checkTokenLength leng txt = case splitAt leng (unpack txt) of
  (_, []) -> return ()
  _ -> throwE $ SecretTokenError $ "Token too long. Maximum length should be: " ++ show leng
