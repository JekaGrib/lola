{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
    selectPicBS :: PicId -> m [ByteString],
    insertRetPicBS :: ByteString -> m Id,
    goToUrl :: HT.Request -> m (HT.Response BSL.ByteString)
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectPicBS' conn)
        (insertRetPicBS' conn)
        HT.httpLBS

selectPicBS' conn picId =
  let wh = WherePair "pic_id=?" (Id picId)
  selectBS' conn (Select ["pic"] "pics" wh)
insertRetPicBS conn sbs =
  let insPair = InsertPair "pic" (BS (Binary sbs))
  insertReturn' conn (InsertRet "pics" [insPair] "pic_id")

sendPicture :: (MonadCatch m) => Handle m -> PictureId -> ExceptT ReqError m ResponseInfo
sendPicture Handle{..} picIdNum = do
  let logpair = ("pic_id", picIdNum)
  bs <- checkOneSelIfExistE hLog logpair $ selectPicBS picIdNum
  let lbs = BSL.fromStrict bs
  lift $ logInfo hLog $ "Pic_id: " ++ show picIdNum ++ " sending in response"
  return $
    ResponseInfo
      status200
      [("Content-Type", "image/jpeg")]
      (lazyByteString lbs)

browsePicture :: (MonadCatch m) => Handle m -> BrowsePicture -> ExceptT ReqError m ResponseInfo
browsePicture h@Handle{..} (BrowsePicture picUrlParam) = do
  lbs <- checkPicUrlGetPic h picUrlParam
  let sbs = BSL.toStrict lbs
  picId <- catchInsRetE hLog $ insertRetPicBS sbs
  lift $ logInfo hLog $ "Picture_id: " ++ show picId ++ " uploaded"
  okHelper $ inPicIdUrl hConf picId

checkPicUrlGetPic :: (MonadCatch m) => Handle m -> Text -> ExceptT ReqError m BSL.ByteString
checkPicUrlGetPic Handle{..} url = do
  res <- lift (goToUrl . fromString . unpack $ url) `catch` (\e -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url ++ ". " ++ show (e :: HT.HttpException))
  let lbs = HT.getResponseBody res
  let sbs = BSL.toStrict lbs
  case decodeImage sbs of
    Right _ -> return lbs
    Left _ -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url
