{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Methods.Common.Exist where

import Methods.Common
import Psql.ToQuery
import Control.Monad.Trans.Except (ExceptT,throwE)
import Oops (ReqError(..))
import Control.Monad.Catch (MonadCatch)
import Types
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Psql.Methods.Common.Exist
import Methods.Common.Exist.UncheckedExId 

data Handle m = Handle
  { hConf :: Config
  , isExist :: UncheckedExId -> m Bool
  }

makeH :: Config -> Handle IO
makeH conf =
  let conn = extractConn conf
   in Handle
        conf
        (isExist' conn)


isExistResourseE :: (MonadCatch m) => Handle m -> UncheckedExId -> ExceptT ReqError m ()
isExistResourseE h iD = do
  isEx <- catchDbErrE $ isExist h iD
  unless isEx $
    throwE $ ResourseNotExistError $ toPretty iD ++ " doesn`t exist"


isExistE :: (MonadCatch m) => Handle m -> UncheckedExId -> ExceptT ReqError m ()
isExistE h iD = do
  isEx <- catchDbErrE $ isExist h iD
  unless isEx $
    throwE $ BadReqError $ toPretty iD ++ " doesn`t exist"

class CheckExist a where 
  checkExist :: (MonadCatch m) => Handle m -> a -> ExceptT ReqError m ()

instance CheckExist UncheckedExId where
  checkExist = isExistE

instance CheckExist [UncheckedExId] where
  checkExist h = mapM_ (isExistE h)

instance CheckExist a => CheckExist (Maybe a) where
  checkExist _ Nothing = return ()
  checkExist h (Just iD) = checkExist h iD

{-
fromUncheck (AuthorId iD) = iD
fromUncheck (CategoryId iD) = iD
fromUncheck (CommentId iD) = iD
fromUncheck (DraftId iD) = iD
fromUncheck (PictureId iD) = iD
fromUncheck (PostId iD) = iD
fromUncheck (TagId iD) = iD
fromUncheck (UserId iD) = iD

class ToWherePair a where
  toWherePair :: a -> WherePair

toWherePair (AuthorId auId) = WherePair "author_id=?" (Id auId)
toWherePair (CategoryId auId) = WherePair "category_id=?" (Id auId)
toWherePair (CommentId auId) = WherePair "comment_id=?" (Id auId)
toWherePair (DraftId auId) = WherePair "draft_id=?" (Id auId)
toWherePair (PictureId auId) = WherePair "pic_id=?" (Id auId)
toWherePair (PostId auId) = WherePair "post_id=?" (Id auId)
toWherePair (TagId auId) = WherePair "tag_id=?" (Id auId)
toWherePair (UserId auId) = WherePair "user_id=?" (Id auId)

class ToTable a where 
  toTable :: a -> Table

toTable (AuthorId auId) =  "authors"
toTable (CategoryId auId) =  "categories"
toTable (CommentId auId) =  "authors"
toTable (DraftId auId) =  "drafts"
toTable (PictureId auId) =  "pictures"
toTable (PostId auId) =  "posts"
toTable (TagId auId) =  "tags"
toTable (UserId auId) =  "users"-}