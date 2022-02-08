{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Methods.Comment where
          
import           Api.Response (CommentResponse(..),CommentsResponse(..),OkResponse(..))
import Methods.Handle.Select (Comment(comment_idC))
import           Logger
import           Types
import           Oops
import           Methods.Handle
import Methods.Auth (AccessMode(..))
import ParseQueryStr (CreateComment(..),GetComments(..),UpdateComment(..),DeleteComment(..))
import Conf (Config(..))
import           Data.Text                      ( pack, unpack, Text )
import           Database.PostgreSQL.Simple (Only(..))
import           Control.Monad.Trans.Except (ExceptT,throwE)
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            (MonadCatch)
import           Control.Monad (unless)



createComment :: (MonadCatch m) => MethodsHandle m -> UserId -> CreateComment -> ExceptT ReqError m ResponseInfo
createComment h usIdNum (CreateComment postIdNum txtParam) = do
  let postIdParam = numToTxt postIdNum
  isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
  commId <- insertReturnE h "comments" "comment_id" ["comment_text","post_id","user_id"] [txtParam,postIdParam,numToTxt usIdNum] 
  lift $ logInfo (hLog h) $ "Comment_id: " ++ show commId ++ " created"
  okHelper $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postIdNum, user_id6 = usIdNum}

  
getComments :: (MonadCatch m) => MethodsHandle m -> GetComments -> ExceptT ReqError m ResponseInfo 
getComments h (GetComments postIdNum pageNum) = do
  let postIdParam = numToTxt postIdNum
  isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
  comms <- checkListE h $ selectLimitComment h "comments" "comment_id DESC" pageNum (cCommLimit . hConf $ h) ["comment_id","user_id","comment_text"] "post_id=?" [postIdParam] [] []
  lift $ logInfo (hLog h) $ "Comments_id: " ++ show (fmap comment_idC comms) ++ " sending in response" 
  okHelper $ CommentsResponse {pageCR = pageNum, post_id9 = postIdNum, comments = fmap inCommResp comms}
  
updateComment :: (MonadCatch m) => MethodsHandle m -> UserId -> UpdateComment -> ExceptT ReqError m ResponseInfo 
updateComment h usIdNum (UpdateComment commIdNum txtParam) = do
  let commIdParam = numToTxt commIdNum 
  isCommAuthorIfExist h  commIdParam usIdNum
  updateInDbE h "comments" "comment_text=?" "comment_id=?" [txtParam,commIdParam]
  Only postId <- checkOneE h $ selectNum h "comments" ["post_id"] "comment_id=?" [commIdParam]
  lift $ logInfo (hLog h) $ "Comment_id: " ++ show commIdNum ++ " updated"
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
      Only postId <- checkOneE h $ selectNum h "comments" ["post_id"] "comment_id=?" [commIdParam]  
      isCommOrPostAuthor h commIdNum postId usIdNum 
      deleteFromDbE h "comments" "comment_id=?" [commIdParam]
      lift $ logInfo (hLog h) $ "Comment_id: " ++ show commIdNum ++ " deleted"
      okHelper $ OkResponse {ok = True}      

isCommOrPostAuthor :: (MonadCatch m) => MethodsHandle m  -> CommentId -> PostId -> UserId -> ExceptT ReqError m ()
isCommOrPostAuthor h commIdNum postId usIdNum = do
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  Only usPostId <- checkOneE h $ selectNum h table ["user_id"] "post_id=?" [pack . show $ postId] 
  Only usComId <- checkOneE h $ selectNum h "comments" ["user_id"] "comment_id=?" [pack . show $ commIdNum]
  unless (usPostId == usIdNum || usComId == usIdNum) $
    throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ show commIdNum ++ "and not author of post_id: " ++ show postId

isCommAuthorIfExist :: (MonadCatch m) => MethodsHandle m  -> Text -> UserId -> ExceptT ReqError m ()
isCommAuthorIfExist h  commIdParam usIdNum = do
  Only usId <- checkOneIfExistE h (selectNum h) "comments" ["user_id"] "comment_id=?" commIdParam  
  unless (usId == usIdNum) $
    throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ unpack commIdParam