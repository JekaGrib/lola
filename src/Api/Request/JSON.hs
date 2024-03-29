{-# LANGUAGE DeriveGeneric #-}

module Api.Request.JSON where

import Api.AesonOption (optionsSnakeCase)
import Api.Request.QueryStr (checkLength)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    Value (..),
    decodeStrict,
    genericParseJSON,
  )
import Data.ByteString (ByteString)
import Data.HashMap.Strict (toList)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Error (ReqError (..))
import GHC.Generics (Generic)
import Methods.Common.Exist (CheckExist (..), Handle)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Types
import Prelude hiding (length)

data DraftRequest = DraftRequest
  { draftName :: Text,
    draftCategoryId :: CategoryId,
    draftText :: Text,
    draftMainPicId :: PictureId,
    draftPicsIds :: [PictureId],
    draftTagsIds :: [TagId]
  }
  deriving (Eq, Show, Generic)

instance FromJSON DraftRequest where
  parseJSON = genericParseJSON optionsSnakeCase

checkDraftReqJson ::
  (MonadCatch m) =>
  Handle m ->
  ByteString ->
  ExceptT ReqError m DraftRequest
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
    checkExist h (PictureId picId)
    checkExist h (fmap PictureId picsIds)
    checkExist h (fmap TagId tagsIds)

whyBadDraftReq :: (MonadCatch m) => ByteString -> ExceptT ReqError m a
whyBadDraftReq json =
  case (decodeStrict json :: Maybe Object) of
    Just obj -> do
      mapM_ (checkTxt obj) ["draft_name", "draft_text"]
      mapM_ (checkId obj) ["draft_category_id", "draft_main_pic_id"]
      mapM_ (checkIdArray obj) ["draft_tags_ids", "draft_pics_ids"]
      throwE $ BadReqError "Invalid request body"
    Nothing -> throwE $ BadReqError "Invalid request body"

checkId :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m ()
checkId obj paramKey = do
  val <- isExistInObj obj paramKey
  _ <- checkNumVal paramKey val >>= checkScientific paramKey >>= checkNatural paramKey
  return ()

checkIdArray :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m ()
checkIdArray obj paramKey = do
  val <- isExistInObj obj paramKey
  checkNumArrayVal paramKey val
    >>= mapM (checkScientific paramKey)
    >>= mapM_ (checkNatural paramKey)

checkTxt :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m Text
checkTxt obj paramKey = do
  val <- isExistInObj obj paramKey
  checkTxtVal paramKey val

isExistInObj :: (MonadCatch m) => Object -> JsonParamKey -> ExceptT ReqError m Value
isExistInObj obj paramKey =
  case lookup paramKey . toList $ obj of
    Just val -> return val
    Nothing -> throwE $ BadReqError $ "Can`t find parameter: " ++ unpack paramKey

checkScientific ::
  (MonadCatch m) =>
  JsonParamKey ->
  Scientific ->
  ExceptT ReqError m Id
checkScientific paramKey scien =
  case toBoundedInteger scien of
    Just i -> return i
    _ ->
      throwE $ BadReqError $
        "Can`t parse parameter: " ++ unpack paramKey
          ++ ". It should be whole number"

checkNumVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m Scientific
checkNumVal paramKey val =
  case val of
    Number scien -> return scien
    _ ->
      throwE $ BadReqError $
        "Can`t parse parameter: " ++ unpack paramKey
          ++ ". It should be number"

checkTxtVal :: (MonadCatch m) => JsonParamKey -> Value -> ExceptT ReqError m Text
checkTxtVal paramKey val =
  case val of
    String txt -> return txt
    _ ->
      throwE $ BadReqError $
        "Can`t parse parameter: " ++ unpack paramKey
          ++ ". It should be text"

checkNumArrayVal ::
  (MonadCatch m) =>
  JsonParamKey ->
  Value ->
  ExceptT ReqError m [Scientific]
checkNumArrayVal paramKey values =
  case values of
    Array arr -> checkNumListVal paramKey (V.toList arr)
    _ ->
      throwE $ BadReqError $
        "Can`t parse parameter: " ++ unpack paramKey
          ++ ". It should be number array. Example: [1,5,8]"

checkNumListVal ::
  (MonadCatch m) =>
  JsonParamKey ->
  [Value] ->
  ExceptT ReqError m [Scientific]
checkNumListVal paramKey xs = case xs of
  [] -> return []
  (Number scien : ys) -> do
    scienS <- checkNumListVal paramKey ys
    return $ scien : scienS
  _ ->
    throwE $ BadReqError $
      "Can`t parse parameter: " ++ unpack paramKey
        ++ ". It should be number array. Example: [1,5,8]"

checkTokenLength :: (Monad m) => Int -> Text -> ExceptT ReqError m ()
checkTokenLength length txt = case splitAt length (unpack txt) of
  (_, []) -> return ()
  _ ->
    throwE $ SecretTokenError $
      "Token too long. Maximum length should be: " ++ show length

checkNatural :: (Monad m) => QueryParamKey -> Id -> ExceptT ReqError m Id
checkNatural paramKey num
  | num <= 0 =
    throwE $ BadReqError $
      "Parameter: " ++ unpack paramKey
        ++ " . Id should be greater then 0"
  | otherwise = return num
