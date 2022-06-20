module Psql.Methods.Post where

import Database.PostgreSQL.Simple (Connection)
import Psql.Methods.Common
import Psql.Selecty (Post (..), Tag (..))
import Psql.ToQuery.Select (Select (..), Where (..), toWhere)
import Psql.ToQuery.SelectLimit (Filter, OrderBy, SelectLim (..))
import Types

selectPosts' :: Connection -> PostId -> IO [Post]
selectPosts' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  select' conn $
    Select
      [ "posts.post_id",
        "posts.author_id",
        "author_info",
        "user_id",
        "post_name",
        "post_create_date",
        "post_category_id",
        "post_text",
        "post_main_pic_id"
      ]
      "posts JOIN authors ON authors.author_id = posts.author_id "
      wh

selectLimPosts' :: Connection -> [Filter] -> OrderBy -> Page -> Limit -> IO [Post]
selectLimPosts' conn filterArgs orderBy page limit = do
  let wh = WhereAnd $ fmap toWhere filterArgs ++ [Where "true"]
  selectLimit' conn $
    SelectLim
      [ "posts.post_id",
        "posts.author_id",
        "author_info",
        "authors.user_id",
        "post_name",
        "post_create_date",
        "post_category_id",
        "post_text",
        "post_main_pic_id"
      ]
      "posts JOIN authors ON authors.author_id = posts.author_id"
      wh
      filterArgs
      orderBy
      page
      limit

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
