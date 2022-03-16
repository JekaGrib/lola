{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Post where

import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT,throwE)
import Data.List (zip4)
import Database.PostgreSQL.Simple (Connection)
import Logger
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutPost)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Methods.Common.MakeCatResp (makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import Psql.Selecty (Post (..), Tag(..),PostInfo(..))
import Methods.Post.LimitArg ( LimitArg (..), chooseArgs, isDateASC)
import Network.Wai (Request)
import Oops
import Api.Request.QueryStr (GetPosts(..),checkQStr)
import Types
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth,tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE)
import Psql.ToQuery
import Network.HTTP.Types (StdMethod(..),QueryText)
import TryRead (tryReadResourseId)
import qualified Methods.Draft (Handle, makeH)
import Methods.Draft (insertReturnAllDraft,isUserAuthorE_)
import Control.Monad (unless)
import Api.Request.EndPoint
import Psql.ToQuery.Delete
import Psql.ToQuery.Exists
import Psql.ToQuery.Insert
import Psql.ToQuery.SelectLimit
import Psql.ToQuery.Select
import Psql.ToQuery.Update
import Psql.Methods.Common


selectPosts' :: Connection -> PostId -> IO [Post]
selectPosts' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  select' conn $ 
    Select 
      ["posts.post_id", "posts.author_id", "author_info", "user_id", "post_name", "post_create_date", "post_category_id", "post_text", "post_main_pic_id"]
      "posts JOIN authors ON authors.author_id = posts.author_id "
      wh

selectLimPosts' :: Connection -> [Filter] -> OrderBy -> Page -> Limit -> IO [Post]
selectLimPosts' conn filterArgs orderBy page limit = do
  let wh = WhereAnd $ (fmap toWhere filterArgs) ++ [Where "true"]
  selectLimit' conn $ 
    SelectLim 
      ["posts.post_id", "posts.author_id", "author_info", "authors.user_id", "post_name", "post_create_date", "post_category_id", "post_text", "post_main_pic_id"]
      "posts JOIN authors ON authors.author_id = posts.author_id" 
      wh filterArgs orderBy page limit

selectPicsForPost' :: Connection -> PostId -> IO [PictureId]
selectPicsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn (Select ["pic_id"] "postspics" wh)

selectTagsForPost' :: Connection -> PostId -> IO [Tag]
selectTagsForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  select' conn $ 
    Select 
      ["tags.tag_id", "tag_name"] 
      "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" 
      wh

selectUsersForPost' :: Connection -> PostId -> IO [UserId]
selectUsersForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn $
    Select 
      ["user_id"]
      "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
      wh

selectPostInfos' :: Connection -> PostId -> IO [PostInfo]
selectPostInfos' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  select' conn $ 
    Select 
      ["a.author_id", "author_info", "post_name", "post_category_id", "post_text", "post_main_pic_id"]
      "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
      wh

