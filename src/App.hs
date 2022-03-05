{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module App where

import Api.Request (DraftRequest (..))
import Api.Response (OkInfoResponse (..))
import CheckJsonReq (checkDraftReqJson, pullTokenDraftReqJson)
import Conf (Config (..), reConnectDB)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson (encode)
import Data.ByteString.Builder (lazyByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (pack)
import Logger
import qualified Methods (Handle, makeH)
import Methods (hAdm, hAu, hAuth, hCat, hCom, hDr, hPic, hPost, hTag, hUs)
import Methods.Admin (createAdmin)
import Methods.Auth (checkUserTokenParam, logIn, tokenAdminAuth, tokenUserAuth)
import Methods.Author (createAuthor, deleteAuthor, getAuthor, updateAuthor)
import Methods.Category (createCategory, createSubCategory, deleteCategory, getCategory, updateCategory)
import Methods.Comment (createComment, deleteComment, getComments, updateComment)
import Methods.Common (ResponseInfo (..))
import Methods.Draft (createNewDraft, createPostsDraft, deleteDraft, getDraft, getDrafts, publishDraft, updateDraft)
import Methods.Picture (loadPicture, sendPicture)
import Methods.Post (deletePost, getPost, getPosts)
import Methods.Tag (createTag, deleteTag, getTag, updateTag)
import Methods.User (createUser, deleteUser, getUser)
import Network.HTTP.Types (status200,status500, status404)
import Network.Wai (Request, Response, ResponseReceived, pathInfo, responseBuilder, strictRequestBody)
import Oops (ReqError (..), hideErr, hideLogInErr, logOnErr)
import ParseQueryStr (ParseQueryStr, parseQueryStr)
import TryRead (tryReadId, tryReadPage)
import Types

data Handle m = Handle
  { hLog :: LogHandle m
  , hMeth :: Methods.Handle m
  , getBody :: Request -> m BSL.ByteString
  }

application :: Config -> LogHandle IO -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application config handleLog req send = do
  newConfig <- reConnectDB config
  let methH = Methods.makeH newConfig handleLog
  let h = Handle handleLog methH strictRequestBody
  logDebug (hLog h) "Connect to DB"
  respE <- runExceptT $ logOnErr (hLog h) $ chooseRespEx h req
  let resInfo = fromE respE
  logDebug (hLog h) $ "Output response: " ++ (show . toLazyByteString . resBuilder $ resInfo)
  send (responseBuilderFromInfo resInfo)

responseBuilderFromInfo :: ResponseInfo -> Response
responseBuilderFromInfo (ResponseInfo s h b) = responseBuilder s h b

fromE :: Either ReqError ResponseInfo -> ResponseInfo
fromE respE = case respE of
  Right a -> a
  Left (SimpleError str) ->
    ResponseInfo
      status200
      [("Content-Type", "application/json; charset=utf-8")]
      (lazyByteString . encode $ OkInfoResponse {ok7 = False, info7 = pack str})
  Left (SecretLogInError _) ->
    ResponseInfo
      status200
      [("Content-Type", "application/json; charset=utf-8")]
      (lazyByteString . encode $ OkInfoResponse {ok7 = False, info7 = "INVALID password or user_id"})
  Left (SecretTokenError _) ->
    ResponseInfo
      status200
      [("Content-Type", "application/json; charset=utf-8")]
      (lazyByteString . encode $ OkInfoResponse {ok7 = False, info7 = "INVALID token"})
  Left (SecretError _) -> ResponseInfo status404 [] "Status 404 Not Found"
  Left (DatabaseError _) -> ResponseInfo status500 [] "Internal server error"

data ReqInfo = ReqInfo StdMethod [Text] QueryText (Maybe ByteString)

chooseRespEx :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m ResponseInfo
chooseRespEx Handle{..} req = do
  lift $ logDebug hLog $ "Incoming request: " ++ show req
  meth <- pullMethod req
  let path = pathInfo req
  checkPathLength path
  let qStr = queryToQueryText $ queryString req
  checkLengthQStr qStr
  let reqInfo = ReqInfo meth path qStr Nothing
  case path of
    ["users","admin"] -> workWithAdmin (hAdm hMeth) reqInfo
    ("users":_) -> workWithUsers (hUs hMeth) reqInfo
    ("authors":_) -> workWithAuthors (hAu hMeth) reqInfo
    ("categories":_) -> workWithCats (hCat hMeth) reqInfo
    ("tags":_) -> workWithTags (hTag hMeth) reqInfo
    ("drafts":_) -> do
      body <- pullReqBody h req
      workWithDrafts (hDr hMeth) (ReqInfo meth path qStr (Just body))
    
    (POST,["drafts"]) -> do
      lift $ logInfo (hLog h) "Create new draft command"
      (usId, _) <- tokenUserAuth (hAuth methH) req
      body <- getBodyAndCheck h req
      createNewDraft (hDr methH) usId body
    (POST,["drafts",draftIdTxt,"posts"]) -> do
      lift $ logInfo (hLog h) "Publish draft command"
      (usId, _) <- tokenUserAuth (hAuth methH) req
      draftId <- tryReadId "draft_id" draftIdTxt
      publishDraft (hDr methH) usId
    (GET,["drafts",draftIdTxt]) -> do
      lift $ logInfo (hLog h) "Get draft command"
      (usId, _) <- tokenUserAuth (hAuth methH) req
      draftId <- tryReadId "draft_id" draftIdTxt
      getDraft (hDr methH) usId draftId
    (GET,["drafts"]) -> do
      lift $ logInfo (hLog h) "Get drafts command"
      (usId, _) <- tokenUserAuth (hAuth methH) req
      preParseQueryStr h req $ getDrafts (hDr methH) usId
    (PUT,["drafts",draftIdTxt]) -> do
      lift $ logInfo (hLog h) "Update draft command"
      (usId, _) <- tokenUserAuth (hAuth methH) req
      draftId <- tryReadId "draft_id" draftIdTxt
      body <- getBodyAndCheck h req
      updateDraft (hDr methH) usIdN draftId body
    (DELETE,["drafts",draftIdTxt]) -> do
      lift $ logInfo (hLog h) "Delete draft command"
      (usId, _) <- tokenUserAuth (hAuth methH) req
      draftId <- tryReadId "draft_id" draftIdTxt
      deleteDraft (hDr methH) usId draftId    
    (POST,["posts",postIdTxt,"drafts"]) -> do
      lift $ logInfo (hLog h) "Create post`s draft command"
      (usId, _) <- tokenUserAuth (hAuth methH) req
      postId <- tryReadId "post_id" postIdTxt
      createPostsDraft (hDr methH) usId postId
    (GET,["posts",postIdTxt]) -> do
      lift $ logInfo (hLog h) "Get post command"
      postId <- tryReadId "post_id" postIdTxt
      getPost (hPost methH) postId
    (GET,["posts"]) -> do
      lift $ logInfo (hLog h) "Get posts command"
      preParseQueryStr h req $ getPosts (hPost methH) 
    (DELETE,["posts",postIdTxt]) -> do
      lift $ logInfo (hLog h) "Delete post command"
      tokenAdminAuth (hAuth methH) req
      postId <- tryReadId "post_id" postIdTxt
      deletePost (hPost methH) postId
    
    
    ("comments":_) -> workWithComms (hCom hMeth) reqInfo
    ("pictures":_) -> workWithPics (hPic hMeth) reqInfo
    xs -> throwE $ ResourseNotExistError $ "Unknown path : " ++ show xs


pullMethod req = do
  let eithReqMeth = parseMethod . requestMethod $ req
  case eithReqMeth of
    Right meth -> return meth
    Left x -> throwE $ NotImplementedError $ "Wrong method: " ++ show x

checkPathLength path =
  case drop 3 path of
    [] -> mapM_ checkPathTextLength path
    _ -> throwE $ UriTooLongError "Request path too long"

checkPathTextLength :: (Monad m) => Text -> ExceptT ReqError m ()
checkPathTextLength txt = case splitAt 20 (unpack txt) of
  (_, []) -> return ()
  (x,_) -> throwE $ UriTooLongError $ "Request path part too long: " ++ unpack x ++ "..."

 
checkLengthQStr qStr = 
  case drop 12 qStr of
    [] -> when ((sum . map lengthQText $ qStr) > 600) $ throwE $ UriTooLongError "Query string too long"
    _ -> throwE $ UriTooLongError "Query string too long"


lengthQText (txtKey,maybeTxt) = 
  case maybeTxt of
    Just txt -> genericLength txtKey + genericLength txt
    Nothing  -> genericLength txtKey 

pullReqBody :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m ByteString
pullReqBody h req = do
  checkLengthReqBody req
  lift $ getBody h req

checkLengthReqBody req =
  case requestBodyLength req of
    ChunkedBody -> throwE $ ReqBodyTooLargeError "Chunked request body"
    _ -> return ()

parseQueryStrAndLog :: (MonadCatch m, ParseQueryStr a) => Handle m -> Request -> ExceptT ReqError m a
parseQueryStrAndLog h req = do
  queStr <- parseQueryStr req
  lift $ logInfo (hLog h) $ "Query string parsed to: " ++ show queStr
  return queStr

preParseQueryStr :: (MonadCatch m, ParseQueryStr a) => Handle m -> Request -> (a -> ExceptT ReqError m ResponseInfo) -> ExceptT ReqError m ResponseInfo
preParseQueryStr h req foo = do
  queStr <- parseQueryStrAndLog h req
  foo queStr

getBodyAndCheck :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m DraftRequest
getBodyAndCheck h req = do
  json <- lift $ getBody h req
  body <- checkDraftReqJson json

getBodyAndCheckUserToken :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m (UserId, DraftRequest)
getBodyAndCheckUserToken h req = do
  json <- lift $ getBody h req
  tokenParam <- pullTokenDraftReqJson json
  (usIdNum, _) <- checkUserTokenParam (hAuth . hMeth $ h) tokenParam
  body <- checkDraftReqJson json
  return (usIdNum, body)

checkReqLength :: (Monad m) => Request -> ExceptT ReqError m ()
checkReqLength req = case splitAt 20 $ queryString req of
  (_, []) -> return ()
  _ -> throwE $ SimpleError "There is should be less then 20 query string parameters"