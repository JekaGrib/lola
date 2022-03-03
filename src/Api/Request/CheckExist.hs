{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Api.Request.CheckExist where


data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , isExistAuthor :: AuthorId -> m Bool
  , isExistUser :: UserId -> m Bool
  }

isExistUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  isExistInDb' conn (Exists "users" wh)
isExistComm' conn commId = do
  let wh = WherePair "comment_id=?" (Id commId)
  isExistInDb' conn (Exists "comments" wh)
isExistPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  isExistInDb' conn (Exists "posts" wh)

class CheckExist a where
  checkExist :: (Monad m) => Handle m -> a -> ExceptT ReqError m a