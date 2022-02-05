--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}




module Methods.Picture where
          
import           Api.Response
import           Logger
import           Types
import           Oops
import           Methods.Handle
import ParseQueryStr (BrowsePicture(..))
import Conf (Config(..))
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Data.Text                      ( pack, unpack, Text )
import           Database.PostgreSQL.Simple (query, withTransaction, execute, executeMany,Connection,Only(..),Binary(Binary))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( catch, throwM, MonadCatch)
import qualified Data.ByteString.Lazy           as BSL
import           Data.ByteString.Builder        ( lazyByteString)
import qualified Network.HTTP.Simple            as HT
import           Codec.Picture                  ( decodeImage )
import           Data.String                    ( fromString )
import           Network.HTTP.Types             ( status200 )


sendPicture :: (MonadCatch m) => MethodsHandle m -> PictureId -> ExceptT ReqError m ResponseInfo
sendPicture h picIdNum = do
  Only (Binary bs) <- selectOneIfExistE h "pics" ["pic"] "pic_id=?" (numToTxt picIdNum) 
  let lbs = BSL.fromStrict bs
  lift $ logInfo (hLog h) $ "Sending picture"
  return $ ResponseInfo 
    status200 
    [("Content-Type", "image/jpeg")] 
    (lazyByteString $ lbs)

browsePicture :: (MonadCatch m) => MethodsHandle m -> BrowsePicture -> ExceptT ReqError m ResponseInfo
browsePicture h (BrowsePicture picUrlParam) = do
  lbs <- checkPicUrlGetPic h picUrlParam
  let sbs = BSL.toStrict lbs
  picId <- insertByteaInDbE h "pics" "pic_id" ["pic"] sbs
  okHelper $ inPicIdUrl picId 

checkPicUrlGetPic :: (MonadCatch m) => MethodsHandle m  -> Text -> ExceptT ReqError m BSL.ByteString
checkPicUrlGetPic h url = do
  res <- (lift $ (httpAction h) . fromString . unpack $ url) `catch` ( (\e -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url ++ ". " ++ (show (e :: HT.HttpException))) )
  let lbs = HT.getResponseBody res
  let sbs = BSL.toStrict lbs
  case decodeImage sbs of
    Right _ -> return lbs
    Left _  -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url