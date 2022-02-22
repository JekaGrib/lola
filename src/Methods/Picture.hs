{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Picture where

import Codec.Picture (decodeImage)
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import Data.ByteString.Builder (lazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.String (fromString)
import Data.Text (Text, unpack)
import Logger
import Methods.Common
import qualified Network.HTTP.Simple as HT
import Network.HTTP.Types (status200)
import Oops
import ParseQueryStr (BrowsePicture (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectBS :: Table -> [Param] -> Where -> [DbValue] -> m [ByteString],
    insertByteaInDb :: Table -> String -> [String] -> ByteString -> m Integer,
    httpAction :: HT.Request -> m (HT.Response BSL.ByteString)
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectBS' conn)
        (insertByteaInDb' conn)
        HT.httpLBS

sendPicture :: (MonadCatch m) => Handle m -> PictureId -> ExceptT ReqError m ResponseInfo
sendPicture h picIdNum = do
  bs <- checkOneIfExistE (hLog h) (selectBS h) "pics" ["pic"] "pic_id=?" (Num picIdNum)
  let lbs = BSL.fromStrict bs
  lift $ logInfo (hLog h) $ "Pic_id: " ++ show picIdNum ++ " sending in response"
  return $
    ResponseInfo
      status200
      [("Content-Type", "image/jpeg")]
      (lazyByteString lbs)

browsePicture :: (MonadCatch m) => Handle m -> BrowsePicture -> ExceptT ReqError m ResponseInfo
browsePicture h (BrowsePicture picUrlParam) = do
  lbs <- checkPicUrlGetPic h picUrlParam
  let sbs = BSL.toStrict lbs
  picId <- insertByteaInDbE h "pics" "pic_id" ["pic"] sbs
  lift $ logInfo (hLog h) $ "Picture_id: " ++ show picId ++ " uploaded"
  okHelper $ inPicIdUrl (hConf h) picId

checkPicUrlGetPic :: (MonadCatch m) => Handle m -> Text -> ExceptT ReqError m BSL.ByteString
checkPicUrlGetPic h url = do
  res <- lift (httpAction h . fromString . unpack $ url) `catch` (\e -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url ++ ". " ++ show (e :: HT.HttpException))
  let lbs = HT.getResponseBody res
  let sbs = BSL.toStrict lbs
  case decodeImage sbs of
    Right _ -> return lbs
    Left _ -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url

insertByteaInDbE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> ByteString -> ExceptT ReqError m Integer
insertByteaInDbE h = checkInsRetE (hLog h) (insertByteaInDb h)
