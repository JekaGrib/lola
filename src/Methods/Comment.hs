{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}





module Methods.Comment where
          
import           Api.Response (CommentResponse(..),CommentsResponse(..),OkResponse(..))
import Methods.Common.Select (Comment(comment_idC))
import           Logger
import           Types
import           Oops
import           Methods.Common
import Methods.Auth (AccessMode(..))
import ParseQueryStr (CreateComment(..),GetComments(..),UpdateComment(..),DeleteComment(..))
import           Data.Text                      ( pack, unpack, Text )
import           Control.Monad.Trans.Except (ExceptT,throwE)
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            (MonadCatch)
import           Control.Monad (unless)
import  Conf (Config(..),extractConn)
import Methods.Post.LimitArg (FilterArg, SortArg)

data Handle m = Handle 
  { hConf              :: Config,
    hLog               :: LogHandle m ,
    selectNums          :: Table -> [Param] -> Where -> [Text] -> m [Id],
    selectLimitComments :: Table -> String -> Page -> Limit -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> m [Comment],
    updateInDb         :: Table -> String -> String -> [Text] -> m (),
    deleteFromDb       :: Table -> String -> [Text] -> m (),
    isExistInDb        :: Table -> String -> String -> [Text] -> m Bool,
    insertReturn       :: Table -> String -> [String] -> [Text] -> m Integer
    }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH = let conn = extractConn conf in
  Handle 
    conf 
    logH 
    (selectOnly' conn) 
    (selectLimit' conn) 
    (updateInDb' conn) 
    (deleteFromDb' conn)
    (isExistInDb' conn) 
    (insertReturn' conn) 

createComment :: (MonadCatch m) => Handle m -> UserId -> CreateComment -> ExceptT ReqError m ResponseInfo
createComment h usIdNum (CreateComment postIdNum txtParam) = do
  let postIdParam = numToTxt postIdNum
  isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
  commId <- insertReturnE h "comments" "comment_id" ["comment_text","post_id","user_id"] [txtParam,postIdParam,numToTxt usIdNum] 
  lift $ logInfo (hLog h) $ "Comment_id: " ++ show commId ++ " created"
  okHelper $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postIdNum, user_id6 = usIdNum}

  
getComments :: (MonadCatch m) => Handle m -> GetComments -> ExceptT ReqError m ResponseInfo 
getComments h (GetComments postIdNum pageNum) = do
  let postIdParam = numToTxt postIdNum
  isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
  comms <- checkListE (hLog h) $ selectLimitComments h "comments" "comment_id DESC" pageNum (cCommLimit . hConf $ h) ["comment_id","user_id","comment_text"] "post_id=?" [postIdParam] [] []
  lift $ logInfo (hLog h) $ "Comments_id: " ++ show (fmap comment_idC comms) ++ " sending in response" 
  okHelper $ CommentsResponse {pageCR = pageNum, post_id9 = postIdNum, comments = fmap inCommResp comms}
  
updateComment :: (MonadCatch m) => Handle m -> UserId -> UpdateComment -> ExceptT ReqError m ResponseInfo 
updateComment h usIdNum (UpdateComment commIdNum txtParam) = do
  let commIdParam = numToTxt commIdNum 
  isCommAuthorIfExist h  commIdParam usIdNum
  updateInDbE h "comments" "comment_text=?" "comment_id=?" [txtParam,commIdParam]
  postId <- checkOneE (hLog h) $ selectNums h "comments" ["post_id"] "comment_id=?" [commIdParam]
  lift $ logInfo (hLog h) $ "Comment_id: " ++ show commIdNum ++ " updated"
  okHelper $ CommentResponse {comment_id = commIdNum, comment_text = txtParam, post_id6 = postId, user_id6 = usIdNum}


deleteComment :: (MonadCatch m) => Handle m -> UserId -> AccessMode ->  DeleteComment -> ExceptT ReqError m ResponseInfo 
deleteComment h@Handle{..} usIdNum accessMode (DeleteComment commIdNum) = do
  let commIdParam = numToTxt commIdNum
  isExistInDbE h "comments" "comment_id" "comment_id=?" [commIdParam] 
  case accessMode of
    AdminMode -> do
      deleteFromDbE h "comments" "comment_id=?" [commIdParam]
      okHelper $ OkResponse { ok = True }
    UserMode -> do
      postId <- checkOneE hLog $ selectNums "comments" ["post_id"] "comment_id=?" [commIdParam]  
      isCommOrPostAuthor h commIdNum postId usIdNum 
      deleteFromDbE h "comments" "comment_id=?" [commIdParam]
      lift $ logInfo hLog $ "Comment_id: " ++ show commIdNum ++ " deleted"
      okHelper $ OkResponse {ok = True}      

isCommOrPostAuthor :: (MonadCatch m) => Handle m  -> CommentId -> PostId -> UserId -> ExceptT ReqError m ()
isCommOrPostAuthor Handle{..} commIdNum postId usIdNum = do
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  usPostId <- checkOneE hLog $ selectNums table ["user_id"] "post_id=?" [pack . show $ postId] 
  usComId <- checkOneE hLog $ selectNums "comments" ["user_id"] "comment_id=?" [pack . show $ commIdNum]
  unless (usPostId == usIdNum || usComId == usIdNum) $
    throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ show commIdNum ++ " and not author of post_id: " ++ show postId

isCommAuthorIfExist :: (MonadCatch m) => Handle m  -> Text -> UserId -> ExceptT ReqError m ()
isCommAuthorIfExist Handle{..} commIdParam usIdNum = do
  usId <- checkOneIfExistE hLog selectNums "comments" ["user_id"] "comment_id=?" commIdParam  
  unless (usId == usIdNum) $
    throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ unpack commIdParam

insertReturnE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> [Text] -> ExceptT ReqError m Integer
insertReturnE Handle{..} = checkInsRetE hLog insertReturn 

deleteFromDbE :: (MonadCatch m) => Handle m -> Table -> Where -> [Text] -> ExceptT ReqError m ()
deleteFromDbE Handle{..} t w values = do
  lift . logDebug hLog $ "Delete data from DB."
  catchDbErrE $ deleteFromDb t w values
  lift . logInfo hLog $ "Data deleted from DB"

updateInDbE :: (MonadCatch m) => Handle m -> Table -> Set -> Where -> [Text] -> ExceptT ReqError m ()
updateInDbE Handle{..} t s w values = checkUpdE hLog $ updateInDb t s w values

isExistInDbE :: (MonadCatch m) => Handle m  -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
isExistInDbE Handle{..} = checkIsExistE hLog isExistInDb 