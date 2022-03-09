{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Methods.Comment where

import Api.Response (CommentResponse (..), CommentsResponse (..), OkResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Logger
import Methods.Common.Auth (AccessMode (..))
import Methods.Common
import Methods.Common.Selecty (Comment (comment_idC))
import Oops
import Api.Request.QueryStr (CreateComment (..), GetComments (..), UpdateComment (..),checkQStr)
import Types
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth,tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE,UncheckedExId(..))
import Methods.Common.ToQuery
import Network.HTTP.Types (StdMethod(..))
import TryRead (tryReadResourseId)

data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , selectUsersForPost :: PostId -> m [UserId]
  , selectUsersForComm :: CommentId -> m [UserId]
  , selectPostsForComm :: CommentId -> m [PostId]
  , selectLimCommsForPost :: PostId -> OrderBy -> Page -> Limit -> m [Comment]
  , updateDbComm :: CommentText -> CommentId -> m ()
  , deleteDbComm :: CommentId -> m ()
  , insertReturnComm :: CommentText -> PostId -> UserId -> m CommentId
  , hAuth :: Methods.Common.Auth.Handle m
  , hExist :: Methods.Common.Exist.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectUsersForPost' conn)
        (selectUsersForComm' conn)
        (selectPostsForComm' conn)
        (selectLimCommsForPost' conn)
        (updateDbComm' conn)
        (deleteDbComm' conn)
        (insertReturnComm' conn)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)

selectUsersForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn (Select ["user_id"] "posts AS p JOIN authors AS a ON p.author_id=a.author_id" wh)
selectUsersForComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  selectOnly' conn (Select ["user_id"] "comments" wh)
selectPostsForComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  selectOnly' conn (Select ["post_id"] "comments" wh)
selectLimCommsForPost' conn postId orderBy page limit = do
  let wh = WherePair "post_id=?" (Id postId)
  selectLimit' conn $ 
    SelectLim 
      ["comment_id","user_id","comment_text"]
      "comments" wh [] orderBy page limit
updateDbComm' conn commTxt commId = do
  let set = SetPair "comment_text=?" (Txt commTxt)
  let wh = WherePair "comment_id=?" (Id commId)
  updateInDb' conn (Update "comments" [set] wh)
deleteDbComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  deleteFromDb' conn (Delete "comments" wh)
insertReturnComm' conn commTxt postId usId = do
  let insPair1 = InsertPair "comment_text" (Txt commTxt)
  let insPair2 = InsertPair "post_id" (Id postId)
  let insPair3 = InsertPair "user_id" (Id usId)
  let insPairs = [insPair1,insPair2,insPair3]
  insertReturn' conn (InsertRet "comments" insPairs "comment_id")

workWithComms :: (MonadCatch m) => Handle m -> ReqInfo -> ExceptT ReqError m ResponseInfo
workWithComms h@Handle{..} (ReqInfo meth path qStr _) = 
  case (meth,path) of
    (POST,["comments"]) -> do
      lift $ logInfo hLog "Create comment command"
      (usId, _) <- tokenUserAuth hAuth qStr
      checkQStr hExist qStr >>= createComment h usId
    (GET,["comments"]) -> do
      lift $ logInfo hLog "Get comments command"
      checkQStr hExist qStr >>= getComments h
    (PUT,["comments",commIdTxt]) -> do
      lift $ logInfo hLog "Update comment command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      commId <- checkCommResourse h commIdTxt
      checkQStr hExist qStr >>= updateComment h usId commId
    (DELETE,["comments",commIdTxt]) -> do
      lift $ logInfo hLog "Delete comment command"
      (usId, accessMode) <- tokenUserAuth hAuth  qStr
      commId <- checkCommResourse h commIdTxt
      deleteComment h usId accessMode commId
    (x,y) -> throwE $ ResourseNotExistError $ "Unknown method-path combination: " ++ show (x,y)
    
createComment :: (MonadCatch m) => Handle m -> UserId -> CreateComment -> ExceptT ReqError m ResponseInfo
createComment Handle{..} usIdNum (CreateComment postIdParam txtParam) = do
  commId <- catchInsRetE hLog $ insertReturnComm txtParam postIdParam usIdNum
  lift $ logInfo hLog $ "Comment_id: " ++ show commId ++ " created"
  okHelper $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postIdParam, user_id6 = usIdNum}

getComments :: (MonadCatch m) => Handle m -> GetComments -> ExceptT ReqError m ResponseInfo
getComments Handle{..} (GetComments postIdParam pageNum) = do
  let orderBy = ByCommId DESC
  comms <- catchSelE hLog $ selectLimCommsForPost postIdParam orderBy pageNum (cCommLimit hConf)  
  lift $ logInfo hLog $ "Comments_id: " ++ show (fmap comment_idC comms) ++ " sending in response"
  okHelper $ CommentsResponse {pageCR = pageNum, post_id9 = postIdParam, comments = fmap inCommResp comms}

updateComment :: (MonadCatch m) => Handle m -> UserId -> CommentId -> UpdateComment -> ExceptT ReqError m ResponseInfo
updateComment h@Handle{..} usId commId (UpdateComment txtParam) = do
  isCommAuthor h commId usId
  catchUpdE hLog $ updateDbComm txtParam commId
  postId <- catchOneSelE hLog $ selectPostsForComm commId
  lift $ logInfo hLog  $ "Comment_id: " ++ show commId ++ " updated"
  okHelper $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postId, user_id6 = usId}

deleteComment :: (MonadCatch m) => Handle m -> UserId -> AccessMode -> CommentId -> ExceptT ReqError m ResponseInfo
deleteComment h@Handle{..} usId accessMode commId = do
  case accessMode of
    AdminMode -> return ()
    UserMode -> do
      postId <- catchOneSelE hLog $ selectPostsForComm commId
      isCommOrPostAuthor h commId postId usId
  deleteDbCommE h commId
  lift $ logInfo hLog $ "Comment_id: " ++ show commId ++ " deleted"
  okHelper $ OkResponse {ok = True}    

isCommOrPostAuthor :: (MonadCatch m) => Handle m -> CommentId -> PostId -> UserId -> ExceptT ReqError m ()
isCommOrPostAuthor Handle{..} commId postId usId = do
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

checkCommResourse :: (MonadCatch m) => Handle m -> ResourseId -> ExceptT ReqError m CommentId
checkCommResourse Handle{..} commIdTxt = do
  iD <- tryReadResourseId "comment_id" commIdTxt
  isExistResourseE hExist (CommentId iD)
  return iD

deleteDbCommE :: (MonadCatch m) => Handle m -> CommentId -> ExceptT ReqError m ()
deleteDbCommE Handle {..} commId = do
  lift . logDebug hLog $ "Delete data from DB."
  catchDbErrE $ deleteDbComm commId
  lift . logInfo hLog $ "Data deleted from DB"


