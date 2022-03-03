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
    selectNums :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Id],
    selectTags :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Tag],
    selectPosts :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Post],
    selectLimitPosts :: Table -> OrderBy -> Page -> Limit -> [DbSelectParamKey] -> Where -> [DbValue] -> [FilterArg] -> [SortArg] -> m [Post],
    updateInDb :: Table -> ToUpdate -> Where -> [DbValue] -> m (),
    deleteFromDb :: Table -> Where -> [DbValue] -> m (),
    isExistInDb :: Table -> Where -> DbValue -> m Bool,
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
        (selectOnly' conn)
        (select' conn)
        (select' conn)
        (selectLimit' conn)
        (updateInDb' conn)
        (deleteFromDb' conn)
        (isExistInDb' conn)
        (withTransaction conn)
        (Methods.Common.MakeCatResp.makeH conf logH)
        (Methods.Common.DeleteMany.makeH conf)

getPost :: (MonadCatch m) => Handle m -> PostId -> ExceptT ReqError m ResponseInfo
getPost h postIdParam = do
  Post pId auId auInfo usId pName pDate pCatId pText picId <- checkOneIfExistE (hLog h) (selectPosts h) "posts JOIN authors ON authors.author_id = posts.author_id " ["posts.post_id", "posts.author_id", "author_info", "user_id", "post_name", "post_create_date", "post_category_id", "post_text", "post_main_pic_id"] "post_id=?" (Id postIdParam)
  picsIds <- checkListE (hLog h) $ selectNums h "postspics" ["pic_id"] "post_id=?" [Id postIdParam]
  tagS <- checkListE (hLog h) $ selectTags h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id", "tag_name"] "post_id=?" [Id postIdParam]
  catResp <- makeCatResp (hCatResp h) pCatId
  lift $ logInfo (hLog h) $ "Post_id: " ++ show pId ++ " sending in response"
  okHelper $ PostResponse {post_id = pId, author4 = AuthorResponse auId auInfo usId, post_name = pName, post_create_date = pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl (hConf h) picId, post_pics = fmap (inPicIdUrl (hConf h)) picsIds, post_tags = fmap inTagResp tagS}

getPosts :: (MonadCatch m) => Handle m -> Page -> GetPosts -> ExceptT ReqError m ResponseInfo
getPosts h pageNum gP = do
  LimitArg filterArgs sortArgs <- chooseArgs gP
  let extractParams = ["posts.post_id", "posts.author_id", "author_info", "authors.user_id", "post_name", "post_create_date", "post_category_id", "post_text", "post_main_pic_id"]
  let defTable = "posts JOIN authors ON authors.author_id = posts.author_id"
  let defOrderBy = if isDateASC sortArgs then [ByPostDate ASC, ByPostId ASC] else [ByPostDate DESC, ByPostId DESC]
  let orderBy = OrderList $ sortArgs ++ defOrderBy
  posts <- selectLimPosts orderBy filterArgs
  posts <- checkListE (hLog h) $ selectLimitPosts h defTable defOrderBy pageNum (cPostsLimit . hConf $ h) extractParams defWhere defValues filterArgs sortArgs
  let postIdsValues = fmap (Id . post_idP) posts
  let postCatsIds = fmap post_cat_idP posts
  manyCatResp <- mapM (makeCatResp (hCatResp h)) postCatsIds
  manyPostPicsIds <- mapM (checkListE (hLog h) . selectNums h "postspics" ["pic_id"] "post_id=?") $ fmap (: []) postIdsValues
  tagSMany <- mapM (checkListE (hLog h) . selectTags h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id", "tag_name"] "post_id=?") $ fmap (: []) postIdsValues
  let allParams = zip4 posts manyCatResp manyPostPicsIds tagSMany
  lift $ logInfo (hLog h) $ "Post_ids: " ++ show (fmap post_idP posts) ++ " sending in response"
  okHelper $ PostsResponse {page10 = pageNum, posts10 = fmap (\(Post pId auId auInfo usId pName pDate _ pText picId, catResp, pics, tagS) -> PostResponse {post_id = pId, author4 = AuthorResponse auId auInfo usId, post_name = pName, post_create_date = pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl (hConf h) picId, post_pics = fmap (inPicIdUrl (hConf h)) pics, post_tags = fmap inTagResp tagS}) allParams}

deletePost :: (MonadCatch m) => Handle m -> DeletePost -> ExceptT ReqError m ResponseInfo
deletePost h (DeletePost postIdParam) = do
  isExistInDbE h "posts"  "post_id=?" (Id postIdParam)
  withTransactionDBE h $ deleteAllAboutPost (hDelMany h) postIdParam
  lift $ logInfo (hLog h) $ "Post_id: " ++ show postIdParam ++ " deleted"
  okHelper $ OkResponse {ok = True}

isExistInDbE :: (MonadCatch m) => Handle m -> Table -> Where -> DbValue -> ExceptT ReqError m ()
isExistInDbE h = checkIsExistE (hLog h) (isExistInDb h)

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h
