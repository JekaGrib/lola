{-# LANGUAGE RankNTypes #-}

module Methods.Post where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (GetPosts (..), checkQStr)
import Api.Response (AuthorResponse (..), PostResponse (..), PostsResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Database.PostgreSQL.Simple (withTransaction)
import Error (ReqError (..))
import Logger
import Methods.Common
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth, tokenUserAuth)
import Methods.Common.DeleteMany (deleteAllAboutPost)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Common.MakeCatResp (makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import qualified Methods.Draft (Handle, makeH)
import Methods.Draft (insertReturnAllDraft, isUserAuthorE_)
import Methods.Post.LimitArg (LimitArg (..), chooseArgs, isDateASC)
import Network.HTTP.Types (QueryText)
import Psql.Methods.Post
import Psql.Selecty (Post (..), PostInfo (..), Tag (..))
import Psql.ToQuery.SelectLimit (Filter, OrderBy (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectPosts :: PostId -> m [Post],
    selectLimPosts :: [Filter] -> OrderBy -> Page -> Limit -> m [Post],
    selectPicsForPost :: PostId -> m [PictureId],
    selectTagsForPost :: PostId -> m [Tag],
    selectUsersForPost :: PostId -> m [UserId],
    selectPostInfos :: PostId -> m [PostInfo],
    withTransactionDB :: forall a. m a -> m a,
    hCatResp :: Methods.Common.MakeCatResp.Handle m,
    hDelMany :: Methods.Common.DeleteMany.Handle m,
    hDr :: Methods.Draft.Handle m,
    hAuth :: Methods.Common.Auth.Handle m,
    hExist :: Methods.Common.Exist.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectPosts' conn)
        (selectLimPosts' conn)
        (selectPicsForPost' conn)
        (selectTagsForPost' conn)
        (selectUsersForPost' conn)
        (selectPostInfos' conn)
        (withTransaction conn)
        (Methods.Common.MakeCatResp.makeH conf logH)
        (Methods.Common.DeleteMany.makeH conf)
        (Methods.Draft.makeH conf logH)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)

workWithPosts :: (MonadCatch m) => Handle m -> QueryText -> AppMethod -> ExceptT ReqError m ResponseInfo
workWithPosts h@Handle {..} qStr meth =
  case meth of
    ToPostId postId -> do
      lift $ logInfo hLog "Create post`s draft command"
      (usId, _) <- tokenUserAuth hAuth qStr
      isExistResourseE hExist (PostId postId)
      createPostsDraft h usId postId
    ToGet postId -> do
      lift $ logInfo hLog "Get post command"
      isExistResourseE hExist (PostId postId)
      getPost h postId
    ToGetAll -> do
      lift $ logInfo hLog "Get posts command"
      checkQStr hExist qStr >>= getPosts h
    ToDelete postId -> do
      lift $ logInfo hLog "Delete post command"
      tokenAdminAuth hAuth qStr
      isExistResourseE hExist (PostId postId)
      deletePost h postId
    _ -> throwE $ ResourseNotExistError $ "Wrong method for posts resourse: " ++ show meth

getPost :: (MonadCatch m) => Handle m -> PostId -> ExceptT ReqError m ResponseInfo
getPost h@Handle {..} postId = do
  post <- catchOneSelectE hLog $ selectPosts postId
  resp <- makePostResponse h post
  lift $ logInfo hLog $ "Post_id: " ++ show postId ++ " sending in response"
  okHelper resp

getPosts :: (MonadCatch m) => Handle m -> GetPosts -> ExceptT ReqError m ResponseInfo
getPosts h@Handle {..} gP@(GetPosts page _ _) = do
  LimitArg filterArgs sortArgs <- chooseArgs gP
  let defOrderBy = if isDateASC sortArgs then [ByPostDate ASC, ByPostId ASC] else [ByPostDate DESC, ByPostId DESC]
  let orderBy = OrderList $ sortArgs ++ defOrderBy
  posts <- catchSelE hLog $ selectLimPosts filterArgs orderBy page (cPostsLimit hConf)
  resps <- mapM (makePostResponse h) posts
  lift $ logInfo hLog $ "Post_ids: " ++ show (fmap post_idP posts) ++ " sending in response"
  okHelper $ PostsResponse {pageP = page, postsP = resps}

createPostsDraft :: (MonadCatch m) => Handle m -> UserId -> PostId -> ExceptT ReqError m ResponseInfo
createPostsDraft h@Handle {..} usId postId = do
  isUserAuthorE_ hDr usId
  PostInfo auId _ postName postCatId postTxt mPicId <- catchOneSelectE hLog $ selectPostInfos postId
  isPostAuthor h postId usId
  picsIds <- catchSelE hLog $ selectPicsForPost postId
  tagS <- catchSelE hLog $ selectTagsForPost postId
  let tagsIds = fmap tag_idT tagS
  let insDr = InsertDraft (Just postId) auId postName postCatId postTxt mPicId
  draftId <- insertReturnAllDraft hDr picsIds tagsIds insDr
  lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " created for post_id: " ++ show postId
  ok201Helper hConf "draft" draftId

deletePost :: (MonadCatch m) => Handle m -> PostId -> ExceptT ReqError m ResponseInfo
deletePost h@Handle {..} postId = do
  withTransactionDBE h $ deleteAllAboutPost hDelMany postId
  lift $ logInfo hLog $ "Post_id: " ++ show postId ++ " deleted"
  ok204Helper

makePostResponse :: (MonadCatch m) => Handle m -> Post -> ExceptT ReqError m PostResponse
makePostResponse Handle {..} (Post pId auId auInfo usId pName pDate pCatId pText picId) = do
  picsIds <- catchSelE hLog $ selectPicsForPost pId
  tagS <- catchSelE hLog $ selectTagsForPost pId
  catResp <- makeCatResp hCatResp pCatId
  return $ PostResponse {postIdP = pId, authorP = AuthorResponse auId auInfo usId, postNameP = pName, postCreateDateP = pDate, postCategoryP = catResp, postTextP = pText, postMainPicIdP = picId, postMainPicUrlP = makeMyPicUrl hConf picId, postPicsP = fmap (inPicIdUrl hConf) picsIds, postTagsP = fmap inTagResp tagS}

isPostAuthor :: (MonadCatch m) => Handle m -> PostId -> UserId -> ExceptT ReqError m ()
isPostAuthor Handle {..} postId usId = do
  usPostId <- catchOneSelectE hLog $ selectUsersForPost postId
  unless (usPostId == usId)
    $ throwE
    $ ForbiddenError
    $ "user_id: " ++ show usId ++ " is not author of post_id: " ++ show postId

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactionE (hLog h) . withTransactionDB h
