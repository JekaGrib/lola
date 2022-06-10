{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module App where

import Api.Request.EndPoint (AppMethod (..), EndPoint (..), parseEndPoint)
import Api.Response (OkInfoResponse (..))
import Conf (Config (..), reConnectDB)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.List (genericLength)
import Data.Text (Text, pack, unpack)
import Logger
import qualified Methods (Handle, makeH)
import Methods (hAdm, hAu, hCat, hCom, hDr, hPic, hPost, hTag, hUs)
import Methods.Admin (workWithAdmin)
import Methods.Author (workWithAuthors)
import Methods.Category (workWithCats)
import Methods.Comment (workWithComms)
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Draft (workWithDrafts)
import Methods.Picture (workWithPics)
import Methods.Post (workWithPosts)
import Methods.Tag (workWithTags)
import Methods.User (workWithLogIn, workWithUsers)
import Network.HTTP.Types (QueryText, StdMethod, parseMethod, queryToQueryText, status400, status401, status403, status404, status413, status414, status500, status501)
import Network.Wai (Request, RequestBodyLength (..), Response, ResponseReceived, getRequestBodyChunk, pathInfo, queryString, requestBodyLength, requestMethod, responseLBS)
import Oops (ReqError (..), logOnErr)

data Handle m = Handle
  { hLog :: LogHandle m,
    hMeth :: Methods.Handle m,
    getBody :: Request -> m ByteString
  }

application :: Config -> LogHandle IO -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application config handleLog req send = do
  newConfig <- reConnectDB config
  let methH = Methods.makeH newConfig handleLog
  let h = Handle handleLog methH getRequestBodyChunk
  logDebug (hLog h) "Connect to DB"
  respE <- runExceptT $ logOnErr (hLog h) $ chooseRespEx h req
  let resInfo = fromE respE
  logDebug (hLog h) $ "Output response: " ++ show resInfo
  send (responseFromInfo resInfo)

responseFromInfo :: ResponseInfo -> Response
responseFromInfo (ResponseInfo s h b) = responseLBS s h b


fromE :: Either ReqError ResponseInfo -> ResponseInfo
fromE respE = case respE of
  Right a -> a
  Left (BadReqError str) ->
    ResponseInfo
      status400
      [jsonHeader]
      (encode $ OkInfoResponse {ok7 = False, info7 = pack str})
  Left (SecretLogInError _) ->
    ResponseInfo
      status401
      [jsonHeader]
      (encode $ OkInfoResponse {ok7 = False, info7 = "INVALID password or user_id"})
  Left (SecretTokenError _) ->
    ResponseInfo
      status401
      [jsonHeader]
      (encode $ OkInfoResponse {ok7 = False, info7 = "INVALID token"})
  Left (NotImplementedError _) ->
    ResponseInfo
      status501
      [jsonHeader]
      (encode $ OkInfoResponse {ok7 = False, info7 = "Unknown method"})
  Left (ForbiddenError str) ->
    ResponseInfo
      status403
      [jsonHeader]
      (encode $ OkInfoResponse {ok7 = False, info7 = pack str})
  Left (ReqBodyTooLargeError _) -> ResponseInfo status413 [textHeader] "Status 413 Request Body Too Large"
  Left (UriTooLongError _) -> ResponseInfo status414 [textHeader] "Status 414 Request-URI Too Long"
  Left (ResourseNotExistError _) -> ResponseInfo status404 [textHeader] "Status 404 Not Found"
  Left (SecretError _) -> ResponseInfo status404 [textHeader] "Status 404 Not Found"
  Left (DatabaseError _) -> ResponseInfo status500 [textHeader] "Internal server error"

chooseRespEx :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m ResponseInfo
chooseRespEx h@Handle {..} req = do
  lift $ logDebug hLog "Incoming request"
  stdMeth <- pullStdMethod req
  let path = pathInfo req
  checkPathLength path
  let qStr = queryToQueryText $ queryString req
  checkLengthQStr qStr
  lift $ logDebug hLog $ "Request not too long: " ++ show req
  endPoint <- parseEndPoint stdMeth path
  case endPoint of
    AdminEP -> workWithAdmin (hAdm hMeth) qStr
    LogInEP -> workWithLogIn (hUs hMeth) qStr
    UserEP meth -> workWithUsers (hUs hMeth) qStr meth
    AuthorEP meth -> workWithAuthors (hAu hMeth) qStr meth
    CatEP meth -> workWithCats (hCat hMeth) qStr meth
    CommentEP meth -> workWithComms (hCom hMeth) qStr meth
    TagEP meth -> workWithTags (hTag hMeth) qStr meth
    DraftEP ToPost -> do
      body <- pullReqBody h req
      workWithDrafts (hDr hMeth) qStr ToPost body
    DraftEP (ToPut iD) -> do
      body <- pullReqBody h req
      workWithDrafts (hDr hMeth) qStr (ToPut iD) body
    DraftEP meth -> workWithDrafts (hDr hMeth) qStr meth ""
    PostEP meth -> workWithPosts (hPost hMeth) qStr meth
    PictureEP meth -> workWithPics (hPic hMeth) qStr meth

pullStdMethod :: (MonadCatch m) => Request -> ExceptT ReqError m StdMethod
pullStdMethod req = do
  let eithReqMeth = parseMethod . requestMethod $ req
  case eithReqMeth of
    Right meth -> return meth
    Left x -> throwE $ NotImplementedError $ "Wrong method: " ++ show x

checkPathLength :: (MonadCatch m) => [Text] -> ExceptT ReqError m ()
checkPathLength path =
  case drop 3 path of
    [] -> mapM_ checkPathTextLength path
    _ -> throwE $ UriTooLongError "Request path too long"

checkPathTextLength :: (Monad m) => Text -> ExceptT ReqError m ()
checkPathTextLength txt = case splitAt 20 (unpack txt) of
  (_, []) -> return ()
  (x, _) -> throwE $ UriTooLongError $ "Request path part too long: " ++ show x ++ "..."

checkLengthQStr :: (MonadCatch m) => QueryText -> ExceptT ReqError m ()
checkLengthQStr qStr =
  case drop 12 qStr of
    [] -> when ((sum . map lengthQText $ qStr) > 600) $ throwE $ UriTooLongError "Query string too long."
    _ -> throwE $ UriTooLongError "Query string too long"

lengthQText :: (Text, Maybe Text) -> Integer
lengthQText (txtKey, maybeTxt) =
  case maybeTxt of
    Just txt -> genericLength (unpack txtKey) + genericLength (unpack txt)
    Nothing -> genericLength (unpack txtKey)

pullReqBody :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m ByteString
pullReqBody h req = do
  checkLengthReqBody req
  lift $ getBody h req

checkLengthReqBody :: (MonadCatch m) => Request -> ExceptT ReqError m ()
checkLengthReqBody req =
  case requestBodyLength req of
    ChunkedBody -> throwE $ ReqBodyTooLargeError "Chunked request body"
    _ -> return ()


