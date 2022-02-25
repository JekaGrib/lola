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
  { hConf :: Config,
    hLog :: LogHandle m,
    selectNums :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Id],
    selectLimitComments :: Table -> OrderBy -> Page -> Limit -> [DbSelectParamKey] -> Where -> [DbValue] -> [FilterArg] -> [SortArg] -> m [Comment],
    updateInDb :: Table -> ToUpdate -> Where -> [DbValue] -> m (),
    deleteFromDb :: Table -> Where -> [DbValue] -> m (),
    isExistInDb :: Table -> Where -> DbValue -> m Bool,
    insertReturn :: Table -> DbReturnParamKey -> [DbInsertParamKey] -> [DbValue] -> m Id
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectOnly' conn)
        (selectLimit' conn)
        (updateInDb' conn)
        (deleteFromDb' conn)
        (isExistInDb' conn)
        (insertReturn' conn)

createComment :: (MonadCatch m) => Handle m -> UserId -> CreateComment -> ExceptT ReqError m ResponseInfo
createComment h usIdNum (CreateComment postIdParam txtParam) = do
  isExistInDbE h "posts"  "post_id=?" (Id postIdParam)
  commId <- insertReturnE h "comments" "comment_id" ["comment_text", "post_id", "user_id"] [Txt txtParam, Id postIdParam, Id usIdNum]
  lift $ logInfo (hLog h) $ "Comment_id: " ++ show commId ++ " created"
  okHelper $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postIdParam, user_id6 = usIdNum}

getComments :: (MonadCatch m) => Handle m -> GetComments -> ExceptT ReqError m ResponseInfo
getComments h (GetComments postIdParam pageNum) = do
  isExistInDbE h "posts" "post_id=?" (Id postIdParam)
  comms <- checkListE (hLog h) $ selectLimitComments h "comments" "comment_id DESC" pageNum (cCommLimit . hConf $ h) ["comment_id", "user_id", "comment_text"] "post_id=?" [Id postIdParam] [] []
  lift $ logInfo (hLog h) $ "Comments_id: " ++ show (fmap comment_idC comms) ++ " sending in response"
  okHelper $ CommentsResponse {pageCR = pageNum, post_id9 = postIdParam, comments = fmap inCommResp comms}

updateComment :: (MonadCatch m) => Handle m -> UserId -> UpdateComment -> ExceptT ReqError m ResponseInfo
updateComment h usIdNum (UpdateComment commIdParam txtParam) = do
  isCommAuthorIfExist h commIdParam usIdNum
  updateInDbE h "comments" "comment_text=?" "comment_id=?" [Txt txtParam, Id commIdParam]
  postId <- checkOneE (hLog h) $ selectNums h "comments" ["post_id"] "comment_id=?" [Id commIdParam]
  lift $ logInfo (hLog h) $ "Comment_id: " ++ show commIdParam ++ " updated"
  okHelper $ CommentResponse {comment_id = commIdParam, comment_text = txtParam, post_id6 = postId, user_id6 = usIdNum}

deleteComment :: (MonadCatch m) => Handle m -> UserId -> AccessMode -> DeleteComment -> ExceptT ReqError m ResponseInfo
deleteComment h@Handle {..} usIdNum accessMode (DeleteComment commIdParam) = do
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
isCommOrPostAuthor Handle {..} commIdParam postId usIdNum = do
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
