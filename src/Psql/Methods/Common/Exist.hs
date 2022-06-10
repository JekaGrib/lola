module Psql.Methods.Common.Exist where

import Database.PostgreSQL.Simple (Connection)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Psql.Methods.Common
import Psql.ToQuery.Exists (Exists (..))
import Psql.ToQuery.Select (Where (..))
import Types

isExist' :: Connection -> UncheckedExId -> IO Bool
isExist' conn (AuthorId auId) = do
  let wh = WherePair "author_id=?" (Id auId)
  isExistInDb' conn (Exists "authors" wh)
isExist' conn (CategoryId catId) = do
  let wh = WherePair "category_id=?" (Id catId)
  isExistInDb' conn (Exists "categories" wh)
isExist' conn (CommentId commId) = do
  let wh = WherePair "comment_id=?" (Id commId)
  isExistInDb' conn (Exists "comments" wh)
isExist' conn (DraftId drId) = do
  let wh = WherePair "draft_id=?" (Id drId)
  isExistInDb' conn (Exists "drafts" wh)
isExist' conn (PictureId picId) = do
  let wh = WherePair "pic_id=?" (Id picId)
  isExistInDb' conn (Exists "pics" wh)
isExist' conn (PostId postId) = do
  let wh = WherePair "post_id=?" (Id postId)
  isExistInDb' conn (Exists "posts" wh)
isExist' conn (TagId tagId) = do
  let wh = WherePair "tag_id=?" (Id tagId)
  isExistInDb' conn (Exists "tags" wh)
isExist' conn (UserId usId) = do
  let wh = WherePair "user_id=?" (Id usId)
  isExistInDb' conn (Exists "users" wh)
