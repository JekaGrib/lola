module Methods.Picture where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (LoadPicture (..), checkQStr)
import Codec.Picture (decodeImage)
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.String (fromString)
import Data.Text (Text, unpack)
import Error (ReqError (..))
import Logger
import Methods.Common
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourceE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import qualified Network.HTTP.Simple as HT
import Network.HTTP.Types (QueryText, status200)
import Psql.Methods.Picture
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectPicBS :: PictureId -> m [ByteString],
    insertRetPicBS :: ByteString -> m PictureId,
    goToUrl :: Text -> m BSL.ByteString,
    hAuth :: Methods.Common.Auth.Handle m,
    hExist :: Methods.Common.Exist.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectPicBS' conn)
        (insertRetPicBS' conn)
        goToUrl'
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)

workWithPics ::
  (MonadCatch m) =>
  Handle m ->
  QueryText ->
  AppMethod ->
  ExceptT ReqError m ResponseInfo
workWithPics h@Handle {..} qStr meth =
  case meth of
    ToPost -> do
      lift $ logInfo hLog "Load picture command"
      _ <- tokenUserAuth hAuth qStr
      checkQStr hExist qStr >>= loadPicture h
    ToGet picId -> do
      lift $ logInfo hLog "Get picture command"
      isExistResourceE hExist (PictureId picId)
      sendPicture h picId
    _ ->
      throwE $ ResourceNotExistError $
        "Wrong method for pictures resource: " ++ show meth

sendPicture :: (MonadCatch m) => Handle m -> PictureId -> ExceptT ReqError m ResponseInfo
sendPicture Handle {..} picIdNum = do
  bs <- catchOneSelectE hLog $ selectPicBS picIdNum
  let lbs = BSL.fromStrict bs
  lift $ logInfo hLog $ "Pic_id: " ++ show picIdNum ++ " sending in response"
  return $
    ResponseInfo
      status200
      [("Content-Type", "image/jpeg")]
      lbs

loadPicture ::
  (MonadCatch m) =>
  Handle m ->
  LoadPicture ->
  ExceptT ReqError m ResponseInfo
loadPicture h@Handle {..} (LoadPicture picUrlParam) = do
  lbs <- checkPicUrlGetPic h picUrlParam
  let sbs = BSL.toStrict lbs
  picId <- catchInsertReturnE hLog $ insertRetPicBS sbs
  lift $ logInfo hLog $ "Picture_id: " ++ show picId ++ " uploaded"
  ok201Helper hConf "picture" picId

checkPicUrlGetPic :: (MonadCatch m) => Handle m -> Text -> ExceptT ReqError m BSL.ByteString
checkPicUrlGetPic Handle {..} url = do
  lbs <-
    lift (goToUrl url)
      `catch` ( \e ->
                  throwE $ BadReqError $
                    "Invalid picture url:" ++ unpack url
                      ++ ". "
                      ++ show (e :: HT.HttpException)
              )
  let sbs = BSL.toStrict lbs
  case decodeImage sbs of
    Right _ -> return lbs
    Left _ -> throwE $ BadReqError $ "Invalid picture url:" ++ unpack url

goToUrl' :: Text -> IO BSL.ByteString
goToUrl' url = do
  res <- HT.httpLBS . fromString . unpack $ url
  return (HT.getResponseBody res)
