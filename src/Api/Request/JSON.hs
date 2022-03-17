{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Api.Request.JSON where

import Data.Aeson (Object, Value (..), decodeStrict,(.:), FromJSON (parseJSON), withObject)
import Types
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.HashMap.Strict (toList)
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Oops (ReqError (..))
import Api.Request.QueryStr (checkLength)
import Data.Scientific (Scientific,toBoundedInteger)
import Methods.Common.Exist (Handle, CheckExist(..))
import Methods.Common.Exist.UncheckedExId (UncheckedExId(..))
import Data.ByteString (ByteString)

data DraftRequest = DraftRequest
  { draft_name :: Text,
    draft_cat_id :: CategoryId,
    draft_textDR :: Text,
    draft_main_pic_id :: PictureId,
    draft_pics_ids :: [PictureId],
    draft_tags_ids :: [TagId]
  }
  deriving (Eq, Show)

instance FromJSON DraftRequest where
  parseJSON = withObject "DraftRequest" $ \v ->
    DraftRequest
      <$> v .: "draft_name"
      <*> v .: "draft_category_id"
      <*> v .: "draft_text"
      <*> v .: "draft_main_pic_id"
      <*> v .: "draft_pics_ids"
      <*> v .: "draft_tags_ids"


checkDraftReqJson :: (MonadCatch m) => Handle m -> ByteString -> ExceptT ReqError m DraftRequest
checkDraftReqJson h json =
  case (decodeStrict json :: Maybe DraftRequest) of
    Just body@(DraftRequest name catId txt picId picsIds tagsIds) -> do
      _ <- checkLength 50 "draft_name" name
      _ <- checkLength 10000 "draft_text" txt
      _ <- checkNatural "draft_category_id" catId      
      _ <- checkNatural "draft_main_pic_id" picId
      mapM_ (checkNatural "draft_tags_ids") tagsIds
      mapM_ (checkNatural "draft_pics_ids") picsIds
      checkExist h body
      return body
    Nothing -> whyBadDraftReq json

instance CheckExist DraftRequest where 
  checkExist h (DraftRequest _ catId _ picId picsIds tagsIds) = do
    checkExist h (CategoryId catId)
    checkExist h (PictureId  picId)
    checkExist h (fmap PictureId picsIds)
    checkExist h (fmap TagId   tagsIds)



whyBadDraftReq :: (MonadCatch m) => ByteString -> ExceptT ReqError m a
whyBadDraftReq json =
  case (decodeStrict json :: Maybe Object) of
    Just obj -> do
      mapM_ (checkTxt obj) [ "draft_name", "draft_text"]
      mapM_ (checkId obj) ["draft_category_id", "draft_main_pic_id"]
      mapM_ (checkIdArr obj) ["draft_tags_ids", "draft_pics_ids"]
      throwE $ BadReqError "Invalid request body"
    Nothing -> throwE $ BadReqError "Invalid request body"

checkId :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m ()
checkId obj paramKey = do
  val <- isExistInObj obj paramKey 
  _ <- checkNumVal paramKey val >>= checkScientific paramKey >>= checkNatural paramKey
  return ()

checkIdArr :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m ()
checkIdArr obj paramKey = do
  val <- isExistInObj obj paramKey
  checkNumArrVal paramKey val >>= mapM (checkScientific paramKey) >>= mapM_ (checkNatural paramKey)


checkTxt :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m Text
checkTxt obj paramKey = do
  val <- isExistInObj obj paramKey
  checkTxtVal paramKey val

isExistInObj :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m Value
isExistInObj obj paramKey =
  case lookup paramKey . toList $ obj of
    Just val -> return val
    Nothing -> throwE $ BadReqError $ "Can`t find parameter: " ++ unpack paramKey


checkScientific :: (MonadCatch m) => JsonParamKey -> Scientific -> ExceptT ReqError m Id
checkScientific paramKey scien = 
  case toBoundedInteger scien of
    Just i -> return i
    _ -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be whole number"


checkNumVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m Scientific
checkNumVal paramKey val =
  case val of
    Number scien -> return scien
    _ -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be number"

checkTxtVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m Text
checkTxtVal paramKey val =
  case val of
    String txt -> return txt
    _ -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be text"

checkNumArrVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m [Scientific]
checkNumArrVal paramKey values =
  case values of
    Array arr -> checkNumListVal paramKey (V.toList arr) 
    _ -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be number array. Example: [1,5,8]"

checkNumListVal ::  (MonadCatch m) => JsonParamKey -> [Value] -> ExceptT ReqError m [Scientific]
checkNumListVal paramKey xs = case xs of
  [] -> return []
  (Number scien : ys) -> do
    scienS <- checkNumListVal paramKey ys
    return $ scien : scienS
  _ -> throwE $ BadReqError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be number array. Example: [1,5,8]"


checkTokenLength :: (Monad m) => Int -> Text -> ExceptT ReqError m ()
checkTokenLength leng txt = case splitAt leng (unpack txt) of
  (_, []) -> return ()
  _ -> throwE $ SecretTokenError $ "Token too long. Maximum length should be: " ++ show leng

checkNatural :: (Monad m) => QueryParamKey -> Id -> ExceptT ReqError m Id
checkNatural paramKey num
  | num <= 0 = throwE $ BadReqError $ "Parameter: " ++ unpack paramKey ++ " . Id should be greater then 0"
  | otherwise = return num