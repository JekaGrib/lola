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
        (selectLimCommsForPost' conn)
        (updateDbComm' conn)
        (deleteDbComm' conn)
        (isExistComm' conn)
        (isExistPost' conn)
        (insertReturnComm' conn)

selectUsersForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn (Select ["user_id"] "posts" wh)
selectUsersForComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  selectOnly' conn (Select ["user_id"] "comments" wh)
selectLimCommsForPost' conn postId orderBy page limit = do
  let wh = WherePair "post_id=?" (Id postId)
  selectLimit' conn (SelectLim "comments" wh orderBy page limit)
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
  commId <- catchInsRetE hLog $ insertReturnComm  txtParam postIdParam usIdNum
  lift $ logInfo hLog $ "Comment_id: " ++ show commId ++ " created"
  okHelper $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postIdParam, user_id6 = usIdNum}

getComments :: (MonadCatch m) => Handle m -> GetComments -> ExceptT ReqError m ResponseInfo
getComments Handle{..} (GetComments postIdParam pageNum) = do
  let logpair = ("post_id", postIdParam)
  catchExistE hLog logpair $ isExistPost postIdParam
  comms <- catchSelE hLog $ selectLimCommsForPost' conn postIdParam orderBy pageNum (cCommLimit hConf)  
  lift $ logInfo (hLog h) $ "Comments_id: " ++ show (fmap comment_idC comms) ++ " sending in response"
  okHelper $ CommentsResponse {pageCR = pageNum, post_id9 = postIdParam, comments = fmap inCommResp comms}

updateComment :: (MonadCatch m) => Handle m -> UserId -> UpdateComment -> ExceptT ReqError m ResponseInfo
updateComment h@Handle{..} usIdNum (UpdateComment commIdParam txtParam) = do
  isCommAuthorIfExist h commIdParam usIdNum
  updateInDbE h "comments" "comment_text=?" "comment_id=?" [Txt txtParam, Id commIdParam]
  postId <- checkOneE (hLog h) $ selectNums h "comments" ["post_id"] "comment_id=?" [Id commIdParam]
  lift $ logInfo (hLog h) $ "Comment_id: " ++ show commIdParam ++ " updated"
  okHelper $ CommentResponse {comment_id = commIdParam, comment_text = txtParam, post_id6 = postId, user_id6 = usIdNum}

deleteComment :: (MonadCatch m) => Handle m -> UserId -> AccessMode -> DeleteComment -> ExceptT ReqError m ResponseInfo
deleteComment h@Handle{..} usIdNum accessMode (DeleteComment commIdParam) = do
  isExistInDbE h "comments" "comment_id=?" (Id commIdParam)
  case accessMode of
    AdminMode -> do
      deleteFromDbE h "comments" "comment_id=?" [Id commIdParam]
      okHelper $ OkResponse {ok = True}
    UserMode -> do
      postId <- checkOneE hLog $ selectNums "comments" ["post_id"] "comment_id=?" [Id commIdParam]
      isCommOrPostAuthor h commIdParam postId usIdNum
      deleteFromDbE h "comments" "comment_id=?" [Id commIdParam]
      lift $ logInfo hLog $ "Comment_id: " ++ show commIdParam ++ " deleted"
      okHelper $ OkResponse {ok = True}

isCommOrPostAuthor :: (MonadCatch m) => Handle m -> CommentId -> PostId -> UserId -> ExceptT ReqError m ()
isCommOrPostAuthor Handle{..} commIdParam postId usIdNum = do
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  usPostId <- checkOneE hLog $ selectNums table ["user_id"] "post_id=?" [Id postId]
  usComId <- checkOneE hLog $ selectNums "comments" ["user_id"] "comment_id=?" [Id commIdParam]
  unless (usPostId == usIdNum || usComId == usIdNum)
    $ throwE
    $ SimpleError
    $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ show commIdParam ++ " and not author of post_id: " ++ show postId

isCommAuthorIfExist :: (MonadCatch m) => Handle m -> CommentId -> UserId -> ExceptT ReqError m ()
isCommAuthorIfExist Handle {..} commIdParam usIdNum = do
  usId <- checkOneIfExistE hLog selectNums "comments" ["user_id"] "comment_id=?" (Id commIdParam)
  unless (usId == usIdNum)
    $ throwE
    $ SimpleError
    $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ show commIdParam

insertReturnE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> [DbValue] -> ExceptT ReqError m Id
insertReturnE Handle {..} = checkInsRetE hLog insertReturn

deleteFromDbE :: (MonadCatch m) => Handle m -> Table -> Where -> [DbValue] -> ExceptT ReqError m ()
deleteFromDbE Handle {..} t w values = do
  lift . logDebug hLog $ "Delete data from DB."
  catchDbErrE $ deleteFromDb t w values
  lift . logInfo hLog $ "Data deleted from DB"

updateInDbE :: (MonadCatch m) => Handle m -> Table -> Set -> Where -> [DbValue] -> ExceptT ReqError m ()
updateInDbE Handle {..} t s w values = checkUpdE hLog $ updateInDb t s w values

isExistInDbE :: (MonadCatch m) => Handle m -> Table -> Where -> DbValue -> ExceptT ReqError m ()
isExistInDbE Handle {..} = checkIsExistE hLog isExistInDb
