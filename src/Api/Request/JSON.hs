{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Api.Request where

import Data.Aeson ((.:), FromJSON (parseJSON), withObject)
import Types
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
import TryRead (checkBigIntId)
import Types
import Data.Scientific (Scientific,floatingOrInteger)

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


checkDraftReqJson :: (MonadCatch m) => Handle m -> BSL.ByteString -> ExceptT ReqError m DraftRequest
checkDraftReqJson h json =
  case (decode json :: Maybe DraftRequest) of
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
    checkExist h (fmap PictureId picId)
    checkExist h (fmap TagId   tagsIds)



whyBadDraftReq :: (MonadCatch m) => BSL.ByteString -> ExceptT ReqError m a
whyBadDraftReq json =
  case (decode json :: Maybe Object) of
    Just obj -> do
      mapM_ (checkTxt obj) [ "draft_name", "draft_text"]
      mapM_ (checkId obj) ["draft_category_id", "draft_main_pic_id"]
      mapM_ (checkIdArr obj) ["draft_tags_ids", "draft_pics_ids"]
      throwE $ SimpleError "Invalid request body"
    Nothing -> throwE $ SimpleError "Invalid request body"

checkId :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m ()
checkId obj paramKey = do
  val <- isExistInObj obj paramKey 
  checkNumVal paramKey val >>= checkScientific paramKey >>= checkBigIntId paramKey
  return ()

checkIdArr :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m ()
checkIdArr obj paramKey = do
  val <- isExistInObj obj paramKey
  checkNumArrVal paramKey val >>= mapM (checkScientific paramKey) >>= mapM (checkBigIntId paramKey)
  return ()


checkTxt :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m Text
checkTxt obj paramKey = do
  val <- isExistInObj obj paramKey
  checkTxtVal paramKey val

isExistInObj :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m Value
isExistInObj obj paramKey =
  case lookup paramKey . toList $ obj of
    Just val -> return val
    Nothing -> throwE $ SimpleError $ "Can`t find parameter: " ++ unpack paramKey


checkScientific :: (MonadCatch m) => JsonParamKey -> Scientific -> ExceptT ReqError m Integer
checkScientific paramKey scien =
  case floatingOrInteger scien of
    Right i -> return i
    _ -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be whole number"

checkNumVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m Scientific
checkNumVal paramKey val =
  case val of
    Number scien -> return scien
    _ -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be number"

checkTxtVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m Text
checkTxtVal paramKey val =
  case val of
    String txt -> return txt
    _ -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be text"

checkNumArrVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m [Scientific]
checkNumArrVal paramKey values =
  case values of
    Array arr -> checkNumListVal paramKey (V.toList arr) 
    _ -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be number array. Example: [1,5,8]"

checkNumListVal ::  (MonadCatch m) => JsonParamKey -> [Value] -> ExceptT ReqError m [Scientific]
checkNumListVal paramKey xs = case xs of
  [] -> return []
  ((Number scien) : ys) -> do
    scienS <- checkNumListVal paramKey ys
    return $ scien : scienS
  _ -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack paramKey ++ ". It should be number array. Example: [1,5,8]"


checkTokenLength :: (Monad m) => Int -> Text -> ExceptT ReqError m ()
checkTokenLength leng txt = case splitAt leng (unpack txt) of
  (_, []) -> return ()
  _ -> throwE $ SecretTokenError $ "Token too long. Maximum length should be: " ++ show leng

checkNatural :: (Monad m) => QueryParamKey -> Id -> ExceptT ReqError m Id
checkNatural paramKey num
  | num <= 0 = throwE $ SimpleError $ "Parameter: " ++ unpack paramKey ++ " . Id should be greater then 0"
  | otherwise = return num