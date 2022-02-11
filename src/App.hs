{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module App where
          
import           Api.Response (OkInfoResponse(..))
import           Api.Request (DraftRequest(..))
import           Logger
import           Types
import           Oops (ReqError(..),logOnErr,hideLogInErr,hideErr)
import           Methods.Admin (createAdmin)
import           Methods.Auth (logIn,tokenUserAuth,tokenAdminAuth,checkUserTokenParam)
import           Methods.Author (createAuthor,getAuthor,updateAuthor,deleteAuthor)
import           Methods.Category (createCategory,createSubCategory,getCategory,updateCategory,deleteCategory)
import           Methods.Comment (createComment,getComments,updateComment,deleteComment)
import           Methods.Draft (createNewDraft,createPostsDraft,getDraft,getDrafts,updateDraft,deleteDraft,publishDraft)
import           Methods.Picture (sendPicture,browsePicture)
import           Methods.Post (getPost,getPosts,deletePost)
import           Methods.Tag (createTag,getTag,updateTag,deleteTag)
import           Methods.User (createUser,getUser,deleteUser)
import           Methods.Common (ResponseInfo(..))
import qualified Methods (Handle,makeH)
import Methods (hAdm,hAuth,hAu,hCat,hCom,hDr,hPic,hPost,hTag,hUs)
import           Conf (Config(..),reConnectDB)
import ParseQueryStr  (parseQueryStr,ParseQueryStr)
import TryRead (tryReadNum)
import CheckJsonReq (checkDraftReqJson,pullTokenDraftReqJson)
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Network.HTTP.Types             ( status200, status404 )
import           Data.Aeson (encode)
import           Data.Text                      ( pack)
import           Control.Monad.Trans.Except (ExceptT,throwE,runExceptT)
import           Control.Monad.Trans            ( lift )
import qualified Data.ByteString.Lazy           as BSL
import           Control.Monad.Catch            ( MonadCatch)
import           Data.ByteString.Builder        ( lazyByteString, toLazyByteString )

data Handle m = Handle 
  { hLog              :: LogHandle m ,
    hMeth          :: Methods.Handle m,
    getBody           :: Request -> m BSL.ByteString
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
  send ( responseBuilderFromInfo resInfo )

responseBuilderFromInfo :: ResponseInfo -> Response
responseBuilderFromInfo (ResponseInfo s h b) = responseBuilder s h b

fromE :: Either ReqError ResponseInfo -> ResponseInfo
fromE respE = case respE of
  Right a                  -> a
  Left (SimpleError str)   -> ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")]
                                (lazyByteString . encode $ OkInfoResponse {ok7 = False, info7 = pack str})
  Left (SecretLogInError _) -> ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")]
                                (lazyByteString . encode $ OkInfoResponse {ok7 = False, info7 = "INVALID password or user_id"})
  Left (SecretTokenError _) -> ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")]
                                (lazyByteString . encode $ OkInfoResponse {ok7 = False, info7 = "INVALID token"})
  Left (SecretError _)   -> ResponseInfo status404 [] "Status 404 Not Found"
  Left (DatabaseError _) -> ResponseInfo status200 [] "Internal server error"
  

chooseRespEx :: (MonadCatch m) => Handle m  -> Request -> ExceptT ReqError m ResponseInfo
chooseRespEx h req = do
  let methH = hMeth h
  lift $ logDebug (hLog h) $ "Incoming request: " ++ show req
  case pathInfo req of
    ["logIn"] -> hideLogInErr $ do
      lift $ logInfo (hLog h) "Login command"
      preParseQueryStr h req $ logIn (hAuth methH) 
    ["createUser"] -> do
      lift $ logInfo (hLog h) "Create user command" 
      preParseQueryStr h req $ createUser (hUs methH) 
    ["getUser", usId] -> do
      lift $ logInfo (hLog h) "Get user command"
      usIdNum <- tryReadNum usId
      lift $ logInfo (hLog h) $ "User_id parameter parsed:" ++ show usIdNum
      getUser (hUs methH) usIdNum
    ["deleteUser"] -> do
      lift $ logInfo (hLog h) "Delete user command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ deleteUser (hUs methH) 
    ["createAdmin"]        -> hideErr $ do
      lift $ logInfo (hLog h) "Create admin command"
      preParseQueryStr h req $ createAdmin (hAdm methH) 
    ["createAuthor"]        -> do
      lift $ logInfo (hLog h) "Create author command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ createAuthor (hAu methH) 
    ["getAuthor"]        -> do
      lift $ logInfo (hLog h) "Get author command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ getAuthor (hAu methH) 
    ["updateAuthor"]        -> do
      lift $ logInfo (hLog h) "Update author command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ updateAuthor (hAu methH)  
    ["deleteAuthor"]   -> do
      lift $ logInfo (hLog h) "Delete author command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ deleteAuthor (hAu methH) 
    ["createCategory"]        -> do
      lift $ logInfo (hLog h) "Create category command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ createCategory (hCat methH)  
    ["createSubCategory"]        -> do
      lift $ logInfo (hLog h) "Create sub category command"
      tokenAdminAuth (hAuth methH)  req 
      preParseQueryStr h req $ createSubCategory (hCat methH) 
    ["getCategory", catId] -> do
      lift $ logInfo (hLog h) "Get category command"
      catIdNum <- tryReadNum catId
      getCategory (hCat methH) catIdNum 
    ["updateCategory"] -> do
      lift $ logInfo (hLog h) "Update category command"
      tokenAdminAuth (hAuth methH)  req    
      preParseQueryStr h req $ updateCategory (hCat methH) 
    ["deleteCategory"] -> do
      lift $ logInfo (hLog h) "Delete category command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ deleteCategory (hCat methH) 
    ["createTag"]  -> do
      lift $ logInfo (hLog h) "Create tag command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ createTag (hTag methH)   
    ["getTag",tagId]  -> do
      lift $ logInfo (hLog h) "Get tag command"
      tagIdNum <- tryReadNum tagId
      getTag (hTag methH) tagIdNum
    ["updateTag"]        -> do
      lift $ logInfo (hLog h) "Update tag command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ updateTag (hTag methH)
    ["deleteTag"]        -> do
      lift $ logInfo (hLog h) "Delete tag command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ deleteTag (hTag methH)
    ["createNewDraft"]  -> do
      lift $ logInfo (hLog h) "Create new draft command"
      (usIdNum,body) <- getBodyAndCheckUserToken h req 
      createNewDraft (hDr methH) usIdNum body
    ["createPostsDraft"]  -> do
      lift $ logInfo (hLog h) "Create post`s draft command"
      (usIdNum,_) <- tokenUserAuth (hAuth methH) req
      preParseQueryStr h req $ createPostsDraft (hDr methH) usIdNum
    ["getDraft"]  -> do
      lift $ logInfo (hLog h) "Get draft command"
      (usIdNum,_) <- tokenUserAuth (hAuth methH) req 
      preParseQueryStr h req $ getDraft (hDr methH) usIdNum
    ["getDrafts"]  -> do
      lift $ logInfo (hLog h) "Get drafts command"
      (usIdNum,_) <- tokenUserAuth (hAuth methH) req  
      preParseQueryStr h req $ getDrafts (hDr methH) usIdNum    
    ["updateDraft",draftId]  -> do
      lift $ logInfo (hLog h) "Update draft command"
      draftIdNum <- tryReadNum draftId
      (usIdNum,body) <- getBodyAndCheckUserToken h req 
      updateDraft (hDr methH) usIdNum draftIdNum body
    ["deleteDraft"]  -> do
      lift $ logInfo (hLog h) "Delete draft command"
      (usIdNum,_) <- tokenUserAuth (hAuth methH) req 
      preParseQueryStr h req $ deleteDraft (hDr methH) usIdNum    
    ["publishDraft"]  -> do
      lift $ logInfo (hLog h) "Publish draft command"
      (usIdNum,_) <- tokenUserAuth (hAuth methH) req 
      preParseQueryStr h req $ publishDraft (hDr methH) usIdNum    
    ["getPost",postId]  -> do
      lift $ logInfo (hLog h) "Get post command"
      postIdNum <- tryReadNum postId
      getPost (hPost methH) postIdNum
    ["getPosts", page] -> do
      lift $ logInfo (hLog h) "Get posts command"
      pageNum <- tryReadNum page
      getPosts (hPost methH) req pageNum
    ["deletePost"]  -> do
      lift $ logInfo (hLog h) "Delete post command"
      tokenAdminAuth (hAuth methH)  req
      preParseQueryStr h req $ deletePost (hPost methH)  
    ["createComment"]  -> do
      lift $ logInfo (hLog h) "Create comment command"
      (usIdNum,_) <- tokenUserAuth (hAuth methH) req
      preParseQueryStr h req $ createComment (hCom methH) usIdNum        
    ["getComments"] -> do
      lift $ logInfo (hLog h) "Get comments command"
      preParseQueryStr h req $ getComments (hCom methH)              
    ["updateComment"]  -> do
      lift $ logInfo (hLog h) "Update comment command"
      (usIdNum,_) <- tokenUserAuth (hAuth methH) req 
      preParseQueryStr h req $ updateComment (hCom methH) usIdNum             
    ["deleteComment"]  -> do
      lift $ logInfo (hLog h) "Delete comment command"
      (usIdNum,accessMode) <- tokenUserAuth (hAuth methH) req 
      preParseQueryStr h req $ deleteComment (hCom methH) usIdNum accessMode                 
    ["browsePicture"] -> do
      lift $ logInfo (hLog h) "browsePicture command"
      _ <- tokenUserAuth (hAuth methH) req 
      preParseQueryStr h req $ browsePicture (hPic methH)                  
    ["picture",picId]  -> do
      lift $ logInfo (hLog h) "Picture command"
      picIdNum <- tryReadNum picId
      sendPicture (hPic methH) picIdNum
    _ -> throwE $ SecretError "Unknown response" 
       

parseQueryStrAndLog :: (MonadCatch m, ParseQueryStr a) => Handle m  -> Request -> ExceptT ReqError m a
parseQueryStrAndLog h req = do
  queStr <- parseQueryStr req
  lift $ logInfo (hLog h) $ "Query string parsed to: " ++ show queStr
  return queStr

preParseQueryStr :: (MonadCatch m, ParseQueryStr a) => Handle m  -> Request -> (a -> ExceptT ReqError m ResponseInfo) -> ExceptT ReqError m ResponseInfo
preParseQueryStr h req foo = do
  queStr <- parseQueryStrAndLog h req
  foo queStr

getBodyAndCheckUserToken :: (MonadCatch m) => Handle m -> Request -> ExceptT ReqError m (UserId,DraftRequest)
getBodyAndCheckUserToken h req = do
  json <- lift $ getBody h req
  tokenParam <- pullTokenDraftReqJson json
  (usIdNum,_) <- checkUserTokenParam (hAuth . hMeth $ h) tokenParam
  body <- checkDraftReqJson json
  return (usIdNum,body)

