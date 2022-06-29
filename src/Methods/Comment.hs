module Methods.Comment where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr
  ( CreateComment (..),
    GetComments (..),
    UpdateComment (..),
    checkQStr,
  )
import Api.Response (CommentResponse (..), CommentsResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Error (ReqError (..))
import Logger
import Methods.Common
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (AccessMode (..), tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourceE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Network.HTTP.Types (QueryText)
import Psql.Methods.Comment
import Psql.Selecty (Comment (..))
import Psql.ToQuery.SelectLimit (OrderBy (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectComment :: CommentId -> m [Comment],
    selectUsersForPost :: PostId -> m [UserId],
    selectUsersForComment :: CommentId -> m [UserId],
    selectPostsForComment :: CommentId -> m [PostId],
    selectLimCommentsForPost :: PostId -> OrderBy -> Page -> Limit -> m [Comment],
    updateDbComment :: CommentText -> CommentId -> m (),
    deleteDbComment :: CommentId -> m (),
    insertReturnComment :: CommentText -> PostId -> UserId -> m CommentId,
    hAuth :: Methods.Common.Auth.Handle m,
    hExist :: Methods.Common.Exist.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectComment' conn)
        (selectUsersForPost' conn)
        (selectUsersForComment' conn)
        (selectPostsForComment' conn)
        (selectLimCommentsForPost' conn)
        (updateDbComment' conn)
        (deleteDbComment' conn)
        (insertReturnComment' conn)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)

workWithComments ::
  (MonadCatch m) =>
  Handle m ->
  QueryText ->
  AppMethod ->
  ExceptT ReqError m ResponseInfo
workWithComments h@Handle {..} qStr meth =
  case meth of
    ToPost -> do
      lift $ logInfo hLog "Create comment command"
      (usId, _) <- tokenUserAuth hAuth qStr
      checkQStr hExist qStr >>= createComment h usId
    ToGetAll -> do
      lift $ logInfo hLog "Get comments command"
      checkQStr hExist qStr >>= getComments h
    ToGet commentId -> do
      lift $ logInfo hLog "Get comment command"
      isExistResourceE hExist (CommentId commentId)
      getComment h commentId
    ToPut commentId -> do
      lift $ logInfo hLog "Update comment command"
      (usId, _) <- tokenUserAuth hAuth qStr
      isExistResourceE hExist (CommentId commentId)
      checkQStr hExist qStr >>= updateComment h usId commentId
    ToDelete commentId -> do
      lift $ logInfo hLog "Delete comment command"
      (usId, accessMode) <- tokenUserAuth hAuth qStr
      isExistResourceE hExist (CommentId commentId)
      deleteComment h usId accessMode commentId
    _ ->
      throwE $ ResourceNotExistError $
        "Wrong method for comments resource: " ++ show meth

createComment ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  CreateComment ->
  ExceptT ReqError m ResponseInfo
createComment Handle {..} usIdNum (CreateComment postIdParam txtParam) = do
  commentId <- catchInsertReturnE hLog $ insertReturnComment txtParam postIdParam usIdNum
  lift $ logInfo hLog $ "Comment_id: " ++ show commentId ++ " created"
  ok201Helper hConf "comment" commentId

getComment :: (MonadCatch m) => Handle m -> CommentId -> ExceptT ReqError m ResponseInfo
getComment Handle {..} commentId = do
  Comment _ usId txt postId <- catchOneSelectE hLog $ selectComment commentId
  lift $ logInfo hLog $ "Comment_id: " ++ show commentId ++ " sending in response"
  okHelper $
    CommentResponse
      { commentIdCR = commentId,
        commentTextCR = txt,
        userIdCR = usId,
        postIdCR = postId
      }

getComments ::
  (MonadCatch m) =>
  Handle m ->
  GetComments ->
  ExceptT ReqError m ResponseInfo
getComments Handle {..} (GetComments postIdParam pageNum) = do
  let orderBy = ByCommentId DESC
  comments <-
    catchSelE hLog $ selectLimCommentsForPost postIdParam orderBy pageNum (cCommentLimit hConf)
  lift $ logInfo hLog $
    "Comments_id: " ++ show (fmap commentIdC comments)
      ++ " sending in response"
  okHelper $
    CommentsResponse
      { pageCSR = pageNum,
        postIdCSR = postIdParam,
        commentsCSR = fmap inCommentResp comments
      }

updateComment ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  CommentId ->
  UpdateComment ->
  ExceptT ReqError m ResponseInfo
updateComment h@Handle {..} usId commentId (UpdateComment txtParam) = do
  isCommentAuthor h commentId usId
  catchUpdE hLog $ updateDbComment txtParam commentId
  postId <- catchOneSelectE hLog $ selectPostsForComment commentId
  lift $ logInfo hLog $ "Comment_id: " ++ show commentId ++ " updated"
  okHelper $
    CommentResponse
      { commentIdCR = commentId,
        commentTextCR = txtParam,
        postIdCR = postId,
        userIdCR = usId
      }

deleteComment ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  AccessMode ->
  CommentId ->
  ExceptT ReqError m ResponseInfo
deleteComment h@Handle {..} usId accessMode commentId = do
  case accessMode of
    AdminMode -> return ()
    UserMode -> do
      postId <- catchOneSelectE hLog $ selectPostsForComment commentId
      isCommentOrPostAuthor h commentId postId usId
  deleteDbCommentE h commentId
  lift $ logInfo hLog $ "Comment_id: " ++ show commentId ++ " deleted"
  ok204Helper

isCommentOrPostAuthor ::
  (MonadCatch m) =>
  Handle m ->
  CommentId ->
  PostId ->
  UserId ->
  ExceptT ReqError m ()
isCommentOrPostAuthor Handle {..} commentId postId usId = do
  usPostId <- catchOneSelectE hLog $ selectUsersForPost postId
  usCommentId <- catchOneSelectE hLog $ selectUsersForComment commentId
  unless (usPostId == usId || usCommentId == usId)
    $ throwE
    $ ForbiddenError
    $ "user_id: " ++ show usId
      ++ " is not author of comment_id: "
      ++ show commentId
      ++ " and not author of post_id: "
      ++ show postId

isCommentAuthor ::
  (MonadCatch m) =>
  Handle m ->
  CommentId ->
  UserId ->
  ExceptT ReqError m ()
isCommentAuthor Handle {..} commentId usId = do
  usIdForComment <- catchOneSelectE hLog $ selectUsersForComment commentId
  unless (usIdForComment == usId)
    $ throwE
    $ ForbiddenError
    $ "user_id: " ++ show usId
      ++ " is not author of comment_id: "
      ++ show commentId

deleteDbCommentE :: (MonadCatch m) => Handle m -> CommentId -> ExceptT ReqError m ()
deleteDbCommentE Handle {..} commentId = do
  lift . logDebug hLog $ "Delete data from DB."
  catchDbErrE $ deleteDbComment commentId
  lift . logInfo hLog $ "Data deleted from DB"
