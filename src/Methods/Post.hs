{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Post where

import Api.Response (AuthorResponse (..), OkResponse (..), PostResponse (..), PostsResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.List (zip4)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutPost)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Methods.Common.MakeCatResp (makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import Methods.Common.Selecty (Post (..), Tag)
import Methods.Post.LimitArg (FilterArg (..), LimitArg (..), SortArg (..), chooseArgs, isDateASC)
import Network.Wai (Request)
import Oops
import ParseQueryStr (DeletePost (..),GetPosts(..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectPosts :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Id],
    selectLimPosts :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Tag],
    selectPicsForPost :: Table -> OrderBy -> Page -> Limit -> [DbSelectParamKey] -> Where -> [DbValue] -> [FilterArg] -> [SortArg] -> m [Post],
    selectTagsForPost :: Table -> ToUpdate -> Where -> [DbValue] -> m (),
    selectPostInfos :: Table -> OrderBy -> Page -> Limit -> [DbSelectParamKey] -> Where -> [DbValue] -> [FilterArg] -> [SortArg] -> m [Draft],
    withTransactionDB :: forall a. m a -> m a,
    hCatResp :: Methods.Common.MakeCatResp.Handle m,
    hDelMany :: Methods.Common.DeleteMany.Handle m
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
        (selectPostInfos' conn)
        (withTransaction conn)
        (Methods.Common.MakeCatResp.makeH conf logH)
        (Methods.Common.DeleteMany.makeH conf)

selectPosts' conn postId = do
  let wh = WherePair "draft_id=?" (Id postId)
  select' conn $ 
    Select 
      ["posts.post_id", "posts.author_id", "author_info", "user_id", "post_name", "post_create_date", "post_category_id", "post_text", "post_main_pic_id"]
      "posts JOIN authors ON authors.author_id = posts.author_id "
      wh
selectLimPosts conn orderBy filterArgs page limit = do
  let wh = WhereAnd (fmap toWhere filterArgs)
  selectLimit' conn $ 
    SelectLim 
      ["posts.post_id", "posts.author_id", "author_info", "authors.user_id", "post_name", "post_create_date", "post_category_id", "post_text", "post_main_pic_id"]
      "posts JOIN authors ON authors.author_id = posts.author_id" 
      wh orderBy page limit
selectPicsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn (Select ["pic_id"] "postspics" wh)
selectTagsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  select' conn $ 
    Select 
      ["tags.tag_id", "tag_name"] 
      "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" 
      wh
selectPostInfos' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  select' conn $ 
    Select 
      ["a.author_id", "author_info", "post_name", "post_category_id", "post_text", "post_main_pic_id"]
      "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
      wh

getPost :: (MonadCatch m) => Handle m -> PostId -> ExceptT ReqError m ResponseInfo
getPost h postId = do
  post <- catchOneSelE hLog $ selectPosts postId
  resp <- makePostResponse h post
  lift $ logInfo hLog $ "Post_id: " ++ show pId ++ " sending in response"
  okHelper resp

getPosts :: (MonadCatch m) => Handle m -> GetPosts -> ExceptT ReqError m ResponseInfo
getPosts h gP@(GetPosts page _ _) = do
  LimitArg filterArgs sortArgs <- chooseArgs gP
  let defOrderBy = if isDateASC sortArgs then [ByPostDate ASC, ByPostId ASC] else [ByPostDate DESC, ByPostId DESC]
  let orderBy = OrderList $ sortArgs ++ defOrderBy
  posts <- selectLimPosts orderBy page (cPostsLimit hConf) filterArgs
  resps <- mapM makePostResponse h posts
  lift $ logInfo (hLog h) $ "Post_ids: " ++ show (fmap post_idP posts) ++ " sending in response"
  okHelper $ PostsResponse {page10 = page, posts10 = resps}

createPostsDraft :: (MonadCatch m) => Handle m -> UserId -> PostId -> ExceptT ReqError m ResponseInfo
createPostsDraft h@Handle{..} usId postId = do
  isUserAuthorE_ h usId
  PostInfo auId auInfo postName postCatId postTxt mPicId <- catchOneSelE hLog $ selectPostInfos postId
  isPostAuthor h postId usId
  picsIds <- catchSelE hLog $ selectPicsForPost postId
  tagS <- catchSelE hLog $ selectTagsForPost postId
  let tagsIds = fmap tag_idT tagS
  catResp <- makeCatResp hCatResp postCatId
  insDr = InsertDraft postId auId postName postCatId postTxt mPicId
  draftId <- insertReturnAllDraft hDr picsIds tagsIds insDr 
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " created for post_id: " ++ show postId
  okHelper $ DraftResponse {draft_id2 = draftId, post_id2 = PostIdExist postId, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = postName, draft_cat2 = catResp, draft_text2 = postTxt, draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl (hConf h) mPicId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap (inPicIdUrl (hConf h)) picsIds}

deletePost :: (MonadCatch m) => Handle m -> PostId -> ExceptT ReqError m ResponseInfo
deletePost h@Handle{..} postId = do
  withTransactionDBE h $ deleteAllAboutPost hDelMany postId
  lift $ logInfo (hLog h) $ "Post_id: " ++ show postId ++ " deleted"
  okHelper $ OkResponse {ok = True}

makePostResponse :: (MonadCatch m) => Handle m -> Post -> ExceptT ReqError m PostResponse
makePostResponse h (Post pId auId auInfo usId pName pDate pCatId pText picId) = do
  picsIds <- catchSelE hLog $ selectPicsForPost pId
  tagS <- catchSelE hLog $ selectTagsForPost pId
  catResp <- makeCatResp hCatResp pCatId
  return $ PostResponse {post_id = pId, author4 = AuthorResponse auId auInfo usId, post_name = pName, post_create_date = pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl (hConf h) picId, post_pics = fmap (inPicIdUrl (hConf h)) picsIds, post_tags = fmap inTagResp tagS}

isExistInDbE :: (MonadCatch m) => Handle m -> Table -> Where -> DbValue -> ExceptT ReqError m ()
isExistInDbE h = checkIsExistE (hLog h) (isExistInDb h)

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h
