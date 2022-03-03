{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Common.Exist where

import Methods.Common

data Handle m = Handle
  { hConf :: Config
  , isExist :: UncheckedExId -> m Bool
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (isExist' conn)

data UncheckedExId =
  AuthorId AuthorId
  | CategoryId CategoryId
  | CommentId CommentId
  | DraftId DraftId
  | PictureId PictureId
  | PostId PostId
  | TagId TagId
  | UserId UserId

class ToPretty a where
  toPretty :: a -> String

instance ToPretty UncheckedExId where
  toPretty (AuthorId iD) = "author_id: " ++ show iD
  toPretty (CategoryId iD) = "category_id: " ++ show iD
  toPretty (CommentId iD) = "comment_id: " ++ show iD
  toPretty (DraftId iD) = "draft_id: " ++ show iD
  toPretty (PictureId iD) = "pic_id: " ++ show iD
  toPretty (PostId iD) = "post_id: " ++ show iD
  toPretty (TagId iD) = "tag_id: " ++ show iD
  toPretty (UserId iD) = "user_id: " ++ show iD


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

isExistResE :: (MonadCatch m) => Handle m -> UncheckedExId -> ExceptT ReqError m ()
isExistResE h iD = do
  isEx <- catchDbErr $ isExist h iD
  unless isEx $
    throwE $ ResourseNotExistError $ toPretty iD ++ " doesn`t exist"

checkResourseEntity :: (MonadCatch m) => Handle m -> (Id -> UncheckedExId) -> Text -> ExceptT ReqError m ()
checkResourseEntity h func txt =
  tryReadId txt 


isExistResourseE :: (MonadCatch m) => Handle m -> UncheckedExId -> ExceptT ReqError m ()
isExistResourseE h iD = do
  isEx <- catchDbErr $ isExist h iD
  unless isEx $
    throwE $ ResourseNotExistError $ toPretty iD ++ " doesn`t exist"

isExistE :: (MonadCatch m) => Handle m -> UncheckedExId -> ExceptT ReqError m ()
isExistE h iD = do
  isEx <- catchDbErr $ isExist h iD
  unless isEx $
    throwE $ BadReqError $ toPretty iD ++ " doesn`t exist"

class CheckExist a where 
  checkExist :: (Monad m) => Handle m -> a -> ExceptT ReqError m ()

instance CheckExist UncheckedExId where
  checkExist = isExistE

instance CheckExist [UncheckedExId] where
  checkExist h = mapM_ (isExistE h)

instance CheckExist UncheckedExId where
  checkExist Nothing = return ()
  checkExist (Just iD) = isExistE h iD