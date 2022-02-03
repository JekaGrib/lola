{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module CheckJsonReq where
          
import           Api.Request (DraftRequest)
import           Oops
import           Data.Aeson (Object,Value(..),decode)
import           Data.Text                      (  unpack, Text )
import           Control.Monad.Trans.Except (ExceptT,throwE)
import qualified Data.ByteString.Lazy           as BSL
import           Control.Monad.Catch            ( MonadCatch)
import           Data.HashMap.Strict            ( toList )
import qualified Data.Vector                    as V

checkDraftReqJson :: (Monad m, MonadCatch m,MonadFail m) => BSL.ByteString -> ExceptT ReqError m DraftRequest
checkDraftReqJson json = do 
  case (decode json :: Maybe DraftRequest) of
    Just body -> return body
    Nothing   -> case (decode json :: Maybe Object) of
      Just obj -> do
        let numParams = ["draft_category_id","draft_main_pic_id"]
        let textParams = ["token","draft_name","draft_text"]
        let arrayParams = ["draft_tags_ids","draft_pics_ids"]
        let params = numParams ++ textParams ++ arrayParams
        [catIdVal,picIdVal,tokenVal,nameVal,txtVal,tagsIdsVal,picsIdsVal] <- mapM (isExistInObj obj) params
        _ <- mapM checkNumVal [catIdVal,picIdVal]
        _ <- mapM checkStrVal [tokenVal,nameVal,txtVal]
        _ <- mapM checkNumArrVal [tagsIdsVal,picsIdsVal]
        throwE $ SimpleError $ "Can`t parse request body"
      Nothing -> throwE $ SimpleError $ "Invalid request body"


isExistInObj :: (Monad m, MonadCatch m) => Object -> Text -> ExceptT ReqError m Value
isExistInObj obj param = do
  case lookup param . toList $ obj of
    Just val -> return val
    Nothing -> throwE $ SimpleError $ "Can`t find parameter: " ++ unpack param

checkNumVal :: (Monad m, MonadCatch m) => Value -> ExceptT ReqError m ()
checkNumVal val = do
  case val of
    Number _ -> return ()
    _ -> throwE $ SimpleError $ "Can`t parse parameter value: " ++ show val ++ ". It should be number"

checkStrVal :: (Monad m, MonadCatch m) => Value -> ExceptT ReqError m ()
checkStrVal val = do
  case val of
    String _ -> return ()
    _ -> throwE $ SimpleError $ "Can`t parse parameter value: " ++ show val ++ ". It should be text"

checkNumArrVal :: (Monad m, MonadCatch m) => Value -> ExceptT ReqError m ()
checkNumArrVal values = do
  case values of
    Array arr -> case V.toList arr of
      [] -> return ()
      ((Number _) : _) -> return ()
      _ -> throwE $ SimpleError $ "Can`t parse parameter values: " ++ show values ++ ". It should be number array"
    _ -> throwE $ SimpleError $ "Can`t parse parameter values: " ++ show values ++ ". It should be number array"