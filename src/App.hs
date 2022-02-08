{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module App where
          
import           Api.Response (OkInfoResponse(..))
import           Api.Request (DraftRequest(..))
import ConnectDB  (ConnDB(..))
import           Logger
import           Types
import           Oops (ReqError(..),logOnErr)
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
import           Methods.Handle (MethodsHandle, ResponseInfo(..),makeMethodsH)
import           Conf (Config(..))
import ParseQueryStr  (parseQueryStr,ParseQueryStr)
import TryRead (tryReadNum)
import CheckJsonReq (checkDraftReqJson)
import ConnectDB  (tryConnect)
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
    hMeth             :: MethodsHandle m,
    getBody           :: Request -> m BSL.ByteString
    }



application :: Config -> LogHandle IO -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application config handleLog req send = do
  let ConnDB _ connDBInfo = cConnDB config
  connDB <- tryConnect connDBInfo
  let newConfig = config {cConnDB = connDB}
  let methH = makeMethodsH newConfig handleLog 
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
  Left (SecretError _)   -> ResponseInfo status404 [] "Status 404 Not Found"
  Left (DatabaseError _) -> ResponseInfo status200 [] "Internal server error"


chooseRespEx :: (MonadCatch m) => Handle m  -> Request -> ExceptT ReqError m ResponseInfo
chooseRespEx h req = do
  lift $ logDebug (hLog h) $ "Incoming request: " ++ show req
  case pathInfo req of
    ["logIn"] -> do
      lift $ logInfo (hLog h) "Login command"
      preParseQueryStr h req $ logIn (hMeth h) 
    ["createUser"] -> do
      lift $ logInfo (hLog h) "Create user command" 
      preParseQueryStr h req $ createUser (hMeth h) 
    ["getUser", usId] -> do
      lift $ logInfo (hLog h) "Get user command"
      usIdNum <- tryReadNum usId
      lift $ logInfo (hLog h) $ "User_id parameter parsed:" ++ show usIdNum
      getUser (hMeth h) usIdNum
    ["deleteUser"] -> do
      lift $ logInfo (hLog h) "Delete user command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ deleteUser (hMeth h) 
    ["createAdmin"]        -> do
      lift $ logInfo (hLog h) "Create admin command"
      preParseQueryStr h req $ createAdmin (hMeth h) 
    ["createAuthor"]        -> do
      lift $ logInfo (hLog h) "Create author command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ createAuthor (hMeth h) 
    ["getAuthor"]        -> do
      lift $ logInfo (hLog h) "Get author command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ getAuthor (hMeth h) 
    ["updateAuthor"]        -> do
      lift $ logInfo (hLog h) "Update author command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ updateAuthor (hMeth h) 
    ["deleteAuthor"]   -> do
      lift $ logInfo (hLog h) "Delete author command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ deleteAuthor (hMeth h) 
    ["createCategory"]        -> do
      lift $ logInfo (hLog h) "Create category command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ createCategory (hMeth h) 
    ["createSubCategory"]        -> do
      lift $ logInfo (hLog h) "Create sub category command"
      tokenAdminAuth (hMeth h)  req 
      preParseQueryStr h req $ createSubCategory (hMeth h) 
    ["getCategory", catId] -> do
      lift $ logInfo (hLog h) "Get category command"
      catIdNum <- tryReadNum catId
      getCategory (hMeth h) catIdNum 
    ["updateCategory"] -> do
      lift $ logInfo (hLog h) "Update category command"
      tokenAdminAuth (hMeth h)  req    
      preParseQueryStr h req $ updateCategory (hMeth h) 
    ["deleteCategory"] -> do
      lift $ logInfo (hLog h) "Delete category command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ deleteCategory (hMeth h) 
    ["createTag"]  -> do
      lift $ logInfo (hLog h) "Create tag command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ createTag (hMeth h)   
    ["getTag",tagId]  -> do
      lift $ logInfo (hLog h) "Get tag command"
      tagIdNum <- tryReadNum tagId
      getTag (hMeth h) tagIdNum
    ["updateTag"]        -> do
      lift $ logInfo (hLog h) "Update tag command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ updateTag (hMeth h)
    ["deleteTag"]        -> do
      lift $ logInfo (hLog h) "Delete tag command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ deleteTag (hMeth h)
    ["createNewDraft"]  -> do
      lift $ logInfo (hLog h) "Create new draft command"
      (usIdNum,body) <- getBodyAndCheckUserToken h req 
      createNewDraft (hMeth h) usIdNum body
    ["createPostsDraft"]  -> do
      lift $ logInfo (hLog h) "Create post`s draft command"
      (usIdNum,_) <- tokenUserAuth (hMeth h) req
      preParseQueryStr h req $ createPostsDraft (hMeth h) usIdNum
    ["getDraft"]  -> do
      lift $ logInfo (hLog h) "Get draft command"
      (usIdNum,_) <- tokenUserAuth (hMeth h) req 
      preParseQueryStr h req $ getDraft (hMeth h) usIdNum
    ["getDrafts"]  -> do
      lift $ logInfo (hLog h) "Get drafts command"
      (usIdNum,_) <- tokenUserAuth (hMeth h) req  
      preParseQueryStr h req $ getDrafts (hMeth h) usIdNum    
    ["updateDraft",draftId]  -> do
      lift $ logInfo (hLog h) "Update draft command"
      draftIdNum <- tryReadNum draftId
      (usIdNum,body) <- getBodyAndCheckUserToken h req 
      updateDraft (hMeth h) usIdNum draftIdNum body
    ["deleteDraft"]  -> do
      lift $ logInfo (hLog h) "Delete draft command"
      (usIdNum,_) <- tokenUserAuth (hMeth h) req 
      preParseQueryStr h req $ deleteDraft (hMeth h) usIdNum    
    ["publishDraft"]  -> do
      lift $ logInfo (hLog h) "Publish draft command"
      (usIdNum,_) <- tokenUserAuth (hMeth h) req 
      preParseQueryStr h req $ publishDraft (hMeth h) usIdNum    
    ["getPost",postId]  -> do
      lift $ logInfo (hLog h) "Get post command"
      postIdNum <- tryReadNum postId
      getPost (hMeth h) postIdNum
    ["getPosts", page] -> do
      lift $ logInfo (hLog h) "Get posts command"
      pageNum <- tryReadNum page
      getPosts (hMeth h) req pageNum
    ["deletePost"]  -> do
      lift $ logInfo (hLog h) "Delete post command"
      tokenAdminAuth (hMeth h)  req
      preParseQueryStr h req $ deletePost (hMeth h)   
    ["createComment"]  -> do
      lift $ logInfo (hLog h) "Create comment command"
      (usIdNum,_) <- tokenUserAuth (hMeth h) req
      preParseQueryStr h req $ createComment (hMeth h) usIdNum        
    ["getComments"] -> do
      lift $ logInfo (hLog h) "Get comments command"
      preParseQueryStr h req $ getComments (hMeth h)              
    ["updateComment"]  -> do
      lift $ logInfo (hLog h) "Update comment command"
      (usIdNum,_) <- tokenUserAuth (hMeth h) req 
      preParseQueryStr h req $ updateComment (hMeth h) usIdNum             
    ["deleteComment"]  -> do
      lift $ logInfo (hLog h) "Delete comment command"
      (usIdNum,accessMode) <- tokenUserAuth (hMeth h) req 
      preParseQueryStr h req $ deleteComment (hMeth h) usIdNum accessMode                 
    ["browsePicture"] -> do
      lift $ logInfo (hLog h) "browsePicture command"
      _ <- tokenUserAuth (hMeth h) req 
      preParseQueryStr h req $ browsePicture (hMeth h)                  
    ["picture",picId]  -> do
      lift $ logInfo (hLog h) "Picture command"
      picIdNum <- tryReadNum picId
      sendPicture (hMeth h) picIdNum
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
  body <- checkDraftReqJson json
  let tokenParam   = tokenDR    body
  (usIdNum,_) <- checkUserTokenParam (hMeth h) tokenParam
  return (usIdNum,body)

