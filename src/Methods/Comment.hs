--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}




module Methods.Comment where
          
import           Api.Response
import           Logger
import           Types
import           Oops
import           Methods.Handle
import Methods.Auth (AccessMode(..))
import ParseQueryStr 
import Conf (Config(..))
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Data.Text                      ( pack, unpack, Text )
import           Database.PostgreSQL.Simple (query, withTransaction, execute, executeMany,Connection,Only(..),Binary(Binary))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( catch, throwM, MonadCatch)


createComment :: (MonadCatch m) => MethodsHandle m -> UserId -> CreateComment -> ExceptT ReqError m ResponseInfo
createComment h usIdNum (CreateComment postIdNum txtParam) = do
  let postIdParam = numToTxt postIdNum
  isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
  commId <- insertReturnE h "comments" "comment_id" ["comment_text","post_id","user_id"] [txtParam,postIdParam,(pack . show $ usIdNum)] 
  okHelper $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postIdNum, user_id6 = usIdNum}

  
getComments :: (MonadCatch m) => MethodsHandle m -> GetComments -> ExceptT ReqError m ResponseInfo 
getComments h (GetComments postIdNum pageNum) = do
  let postIdParam = numToTxt postIdNum
  isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
  comms <- selectListLimitFromDbE h "comments" "comment_id DESC" pageNum (cCommLimit . hConf $ h) ["comment_id","user_id","comment_text"] "post_id=?" [postIdParam] [] []
  okHelper $ CommentsResponse {pageCR = pageNum, post_id9 = postIdNum, comments = fmap inCommResp comms}
  
updateComment :: (MonadCatch m) => MethodsHandle m -> UserId -> UpdateComment -> ExceptT ReqError m ResponseInfo 
updateComment h usIdNum (UpdateComment commIdNum txtParam) = do
  let commIdParam = numToTxt commIdNum 
  isCommAuthorIfExist h  commIdParam usIdNum
  updateInDbE h "comments" "comment_text=?" "comment_id=?" [txtParam,commIdParam]
  Only postId <- selectOneE h "comments" ["post_id"] "comment_id=?" [commIdParam] 
  okHelper $ CommentResponse {comment_id = commIdNum, comment_text = txtParam, post_id6 = postId, user_id6 = usIdNum}


deleteComment :: (MonadCatch m) => MethodsHandle m -> UserId -> AccessMode ->  DeleteComment -> ExceptT ReqError m ResponseInfo 
deleteComment h usIdNum accessMode (DeleteComment commIdNum) = do
  let commIdParam = numToTxt commIdNum
  isExistInDbE h "comments" "comment_id" "comment_id=?" [commIdParam] 
  case accessMode of
    AdminMode -> do
      deleteFromDbE h "comments" "comment_id=?" [commIdParam]
      okHelper $ OkResponse { ok = True }
    UserMode -> do
      Only postId <- selectOneE h "comments" ["post_id"] "comment_id=?" [commIdParam]  
      isCommOrPostAuthor h commIdNum postId usIdNum 
      deleteFromDbE h "comments" "comment_id=?" [commIdParam]
      okHelper $ OkResponse {ok = True}      

isCommOrPostAuthor :: (MonadCatch m) => MethodsHandle m  -> CommentId -> PostId -> UserId -> ExceptT ReqError m ()
isCommOrPostAuthor h commIdNum postId usIdNum = do
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  Only usPostId <- selectOneE h table ["user_id"] "post_id=?" [pack . show $ postId] 
  Only usComId <- selectOneE h "comments" ["user_id"] "comment_id=?" [pack . show $ commIdNum]
  case usPostId == usIdNum || usComId == usIdNum of
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ show commIdNum ++ "and not author of post_id: " ++ show postId

isCommAuthorIfExist :: (MonadCatch m) => MethodsHandle m  -> Text -> UserId -> ExceptT ReqError m ()
isCommAuthorIfExist h  commIdParam usIdNum = do
  Only usId <- selectOneIfExistE h "comments" ["user_id"] "comment_id=?" commIdParam  
  case usId == usIdNum of
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ unpack commIdParam