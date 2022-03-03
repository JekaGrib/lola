{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Comment where

import Api.Response (CommentResponse (..), CommentsResponse (..), OkResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Logger
import Methods.Auth (AccessMode (..))
import Methods.Common
import Methods.Common.Selecty (Comment (comment_idC))
import Methods.Post.LimitArg (FilterArg, SortArg)
import Oops
import ParseQueryStr (CreateComment (..), DeleteComment (..), GetComments (..), UpdateComment (..))
import Types

data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , selectUsersForPost :: PostId -> m [UserId]
  , selectUsersForComm :: CommentId -> m [UserId]
  , selectPostsForComm :: CommentId -> m [PostId]
  , selectLimCommsForPost :: PostId -> OrderBy -> Page -> Limit -> m [Comment]
  , updateDbComm :: CommentText -> CommentId -> m ()
  , deleteDbComm :: CommentId -> m ()
  , isExistComm :: CommentId -> m Bool
  , isExistPost :: PostId -> m Bool
  , insertReturnComm :: CommentText -> PostId -> UserId -> m CommentId
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
        (isExistComm' conn)
        (isExistPost' conn)
        (insertReturnComm' conn)

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
      "comments" wh orderBy page limit
updateDbComm' conn commTxt commId = do
  let set = SetPair "comment_text=?" (Txt commTxt)
  let wh = WherePair "comment_id=?" (Id commId)
  updateInDb' conn (Update "comments" [set] wh)
deleteDbComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  deleteFromDb' conn (Delete "comments" wh)
isExistComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  isExistInDb' conn (Exists "comments" wh)
isExistPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  isExistInDb' conn (Exists "posts" wh)
insertReturnComm' conn commTxt postId usId = do
  let insPair1 = InsertPair "comment_text" (Txt commTxt)
  let insPair2 = InsertPair "post_id" (Id postId)
  let insPair3 = InsertPair "user_id" (Id usId)
  let insPairs = [insPair1,insPair2,insPair3]
  insertReturn' conn (InsertRet "comments" [insPair] "comment_id")

createComment :: (MonadCatch m) => Handle m -> UserId -> CreateComment -> ExceptT ReqError m ResponseInfo
createComment Handle{..} usIdNum (CreateComment postIdParam txtParam) = do
  let logpair = ("post_id", postIdParam)
  catchExistE hLog logpair $ isExistPost postIdParam
  commId <- catchInsRetE hLog $ insertReturnComm txtParam postIdParam usIdNum
  lift $ logInfo hLog $ "Comment_id: " ++ show commId ++ " created"
  okHelper $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postIdParam, user_id6 = usIdNum}

getComments :: (MonadCatch m) => Handle m -> GetComments -> ExceptT ReqError m ResponseInfo
getComments Handle{..} (GetComments postIdParam pageNum) = do
  let logpair = ("post_id", postIdParam)
  catchExistE hLog logpair $ isExistPost postIdParam
  let orderBy = ByCommId DESC
  comms <- catchSelE hLog $ selectLimCommsForPost' conn postIdParam orderBy pageNum (cCommLimit hConf)  
  lift $ logInfo hLog $ "Comments_id: " ++ show (fmap comment_idC comms) ++ " sending in response"
  okHelper $ CommentsResponse {pageCR = pageNum, post_id9 = postIdParam, comments = fmap inCommResp comms}

updateComment :: (MonadCatch m) => Handle m -> UserId -> UpdateComment -> ExceptT ReqError m ResponseInfo
updateComment h@Handle{..} usId commId (UpdateComment txtParam) = do
  isCommAuthorIfExist h commId usId
  catchUpdE hLog $ updateDbComm txtParam commId
  postId <- catchOneSelE hLog $ selectPostsForComm commId
  lift $ logInfo hLog  $ "Comment_id: " ++ show commId ++ " updated"
  okHelper $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postId, user_id6 = usIdNum}

deleteComment :: (MonadCatch m) => Handle m -> UserId -> AccessMode -> CommentId -> ExceptT ReqError m ResponseInfo
deleteComment h@Handle{..} usId accessMode commId = do
  let logpair = ("comment_id", commId)
  catchExistE hLog logpair $ isExistComm commId
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
    $ SimpleError
    $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ show commIdParam ++ " and not author of post_id: " ++ show postId

isCommAuthorIfExist :: (MonadCatch m) => Handle m -> CommentId -> UserId -> ExceptT ReqError m ()
isCommAuthorIfExist Handle {..} commId usId = do
  usIdForComm <- catchOneSelE hLog $ selectUsersForComm commId
  unless (usIdForComm == usId)
    $ throwE
    $ SimpleError
    $ "user_id: " ++ show usId ++ " is not author of comment_id: " ++ show commId


deleteDbCommE :: (MonadCatch m) => CommentId -> ExceptT ReqError m ()
deleteDbCommE Handle {..} commId = do
  lift . logDebug hLog $ "Delete data from DB."
  catchDbErrE $ deleteDbComm commId
  lift . logInfo hLog $ "Data deleted from DB"


