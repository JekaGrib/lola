module Methods.Comment where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateComment (..), GetComments (..), UpdateComment (..), checkQStr)
import Api.Response (CommentResponse (..), CommentsResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Logger
import Methods.Common
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (AccessMode (..), tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Network.HTTP.Types (QueryText)
import Error (ReqError (..))
import Psql.Methods.Comment
import Psql.Selecty (Comment (..))
import Psql.ToQuery.SelectLimit (OrderBy (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectComm :: CommentId -> m [Comment],
    selectUsersForPost :: PostId -> m [UserId],
    selectUsersForComm :: CommentId -> m [UserId],
    selectPostsForComm :: CommentId -> m [PostId],
    selectLimCommsForPost :: PostId -> OrderBy -> Page -> Limit -> m [Comment],
    updateDbComm :: CommentText -> CommentId -> m (),
    deleteDbComm :: CommentId -> m (),
    insertReturnComm :: CommentText -> PostId -> UserId -> m CommentId,
    hAuth :: Methods.Common.Auth.Handle m,
    hExist :: Methods.Common.Exist.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectComm' conn)
        (selectUsersForPost' conn)
        (selectUsersForComm' conn)
        (selectPostsForComm' conn)
        (selectLimCommsForPost' conn)
        (updateDbComm' conn)
        (deleteDbComm' conn)
        (insertReturnComm' conn)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)

workWithComms :: (MonadCatch m) => Handle m -> QueryText -> AppMethod -> ExceptT ReqError m ResponseInfo
workWithComms h@Handle {..} qStr meth =
  case meth of
    ToPost -> do
      lift $ logInfo hLog "Create comment command"
      (usId, _) <- tokenUserAuth hAuth qStr
      checkQStr hExist qStr >>= createComment h usId
    ToGetAll -> do
      lift $ logInfo hLog "Get comments command"
      checkQStr hExist qStr >>= getComments h
    ToGet commId -> do
      lift $ logInfo hLog "Get comment command"
      isExistResourseE hExist (CommentId commId)
      getComment h commId
    ToPut commId -> do
      lift $ logInfo hLog "Update comment command"
      (usId, _) <- tokenUserAuth hAuth qStr
      isExistResourseE hExist (CommentId commId)
      checkQStr hExist qStr >>= updateComment h usId commId
    ToDelete commId -> do
      lift $ logInfo hLog "Delete comment command"
      (usId, accessMode) <- tokenUserAuth hAuth qStr
      isExistResourseE hExist (CommentId commId)
      deleteComment h usId accessMode commId
    _ -> throwE $ ResourseNotExistError $ "Wrong method for comments resourse: " ++ show meth

createComment :: (MonadCatch m) => Handle m -> UserId -> CreateComment -> ExceptT ReqError m ResponseInfo
createComment Handle {..} usIdNum (CreateComment postIdParam txtParam) = do
  commId <- catchInsRetE hLog $ insertReturnComm txtParam postIdParam usIdNum
  lift $ logInfo hLog $ "Comment_id: " ++ show commId ++ " created"
  ok201Helper hConf "comment" commId

getComment :: (MonadCatch m) => Handle m -> CommentId -> ExceptT ReqError m ResponseInfo
getComment Handle {..} commId = do
  Comment _ usId txt postId <- catchOneSelE hLog $ selectComm commId
  lift $ logInfo hLog $ "Comment_id: " ++ show commId ++ " sending in response"
  okHelper $ CommentResponse {commentIdCR = commId, commentTextCR = txt, userIdCR = usId, postIdCR = postId}

getComments :: (MonadCatch m) => Handle m -> GetComments -> ExceptT ReqError m ResponseInfo
getComments Handle {..} (GetComments postIdParam pageNum) = do
  let orderBy = ByCommId DESC
  comms <- catchSelE hLog $ selectLimCommsForPost postIdParam orderBy pageNum (cCommLimit hConf)
  lift $ logInfo hLog $ "Comments_id: " ++ show (fmap comment_idC comms) ++ " sending in response"
  okHelper $ CommentsResponse {pageCSR = pageNum, postIdCSR = postIdParam, commentsCSR = fmap inCommResp comms}

updateComment :: (MonadCatch m) => Handle m -> UserId -> CommentId -> UpdateComment -> ExceptT ReqError m ResponseInfo
updateComment h@Handle {..} usId commId (UpdateComment txtParam) = do
  isCommAuthor h commId usId
  catchUpdE hLog $ updateDbComm txtParam commId
  postId <- catchOneSelE hLog $ selectPostsForComm commId
  lift $ logInfo hLog $ "Comment_id: " ++ show commId ++ " updated"
  okHelper $ CommentResponse {commentIdCR = commId, commentTextCR = txtParam, postIdCR = postId, userIdCR = usId}

deleteComment :: (MonadCatch m) => Handle m -> UserId -> AccessMode -> CommentId -> ExceptT ReqError m ResponseInfo
deleteComment h@Handle {..} usId accessMode commId = do
  case accessMode of
    AdminMode -> return ()
    UserMode -> do
      postId <- catchOneSelE hLog $ selectPostsForComm commId
      isCommOrPostAuthor h commId postId usId
  deleteDbCommE h commId
  lift $ logInfo hLog $ "Comment_id: " ++ show commId ++ " deleted"
  ok204Helper

isCommOrPostAuthor :: (MonadCatch m) => Handle m -> CommentId -> PostId -> UserId -> ExceptT ReqError m ()
isCommOrPostAuthor Handle {..} commId postId usId = do
  usPostId <- catchOneSelE hLog $ selectUsersForPost postId
  usComId <- catchOneSelE hLog $ selectUsersForComm commId
  unless (usPostId == usId || usComId == usId)
    $ throwE
    $ ForbiddenError
    $ "user_id: " ++ show usId ++ " is not author of comment_id: " ++ show commId ++ " and not author of post_id: " ++ show postId

isCommAuthor :: (MonadCatch m) => Handle m -> CommentId -> UserId -> ExceptT ReqError m ()
isCommAuthor Handle {..} commId usId = do
  usIdForComm <- catchOneSelE hLog $ selectUsersForComm commId
  unless (usIdForComm == usId)
    $ throwE
    $ ForbiddenError
    $ "user_id: " ++ show usId ++ " is not author of comment_id: " ++ show commId

deleteDbCommE :: (MonadCatch m) => Handle m -> CommentId -> ExceptT ReqError m ()
deleteDbCommE Handle {..} commId = do
  lift . logDebug hLog $ "Delete data from DB."
  catchDbErrE $ deleteDbComm commId
  lift . logInfo hLog $ "Data deleted from DB"
