{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module CheckJsonReq (checkDraftReqJson) where
          
import           Api.Request (DraftRequest)
import           Oops
import           Data.Aeson (Object,Value(..),decode)
import           Data.Text                      (  unpack, Text )
import           Control.Monad.Trans.Except (ExceptT,throwE)
import qualified Data.ByteString.Lazy           as BSL
import           Control.Monad.Catch            ( MonadCatch)
import           Data.HashMap.Strict            ( toList )
import qualified Data.Vector                    as V

checkDraftReqJson :: (MonadCatch m) => BSL.ByteString -> ExceptT ReqError m DraftRequest
checkDraftReqJson json =  
  case (decode json :: Maybe DraftRequest) of
    Just body -> return body
    Nothing   -> case (decode json :: Maybe Object) of
      Just obj -> do
        catIdVal <- isExistInObj obj "draft_category_id"
        picIdVal <- isExistInObj obj "draft_main_pic_id"
        tokenVal <- isExistInObj obj "token"
        nameVal <- isExistInObj obj "draft_name"
        txtVal <- isExistInObj obj "draft_text"
        tagsIdsVal <- isExistInObj obj "draft_tags_ids"
        picsIdsVal <- isExistInObj obj "draft_pics_ids"
        mapM_ checkNumVal [catIdVal,picIdVal]
        mapM_ checkStrVal [tokenVal,nameVal,txtVal]
        mapM_ checkNumArrVal [tagsIdsVal,picsIdsVal]
        throwE $ SimpleError  "Can`t parse request body"
      Nothing -> throwE $ SimpleError  "Invalid request body"


isExistInObj :: (MonadCatch m) => Object -> Text -> ExceptT ReqError m Value
isExistInObj obj param = 
  case lookup param . toList $ obj of
    Just val -> return val
    Nothing -> throwE $ SimpleError $ "Can`t find parameter: " ++ unpack param

checkNumVal :: (MonadCatch m) => Value -> ExceptT ReqError m ()
checkNumVal val = 
  case val of
    Number _ -> return ()
    _ -> throwE $ SimpleError $ "Can`t parse parameter value: " ++ show val ++ ". It should be number"

checkStrVal :: (MonadCatch m) => Value -> ExceptT ReqError m ()
checkStrVal val = 
  case val of
    String _ -> return ()
    _ -> throwE $ SimpleError $ "Can`t parse parameter value: " ++ show val ++ ". It should be text"

checkNumArrVal :: (MonadCatch m) => Value -> ExceptT ReqError m ()
checkNumArrVal values = 
  case values of
    Array arr -> case V.toList arr of
      [] -> return ()
      (Number _ : _) -> return ()
      _ -> throwE $ SimpleError $ "Can`t parse parameter values: " ++ show values ++ ". It should be number array"
    _ -> throwE $ SimpleError $ "Can`t parse parameter values: " ++ show values ++ ". It should be number array"