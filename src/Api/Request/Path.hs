{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Api.Request where

import Data.Aeson ((.:), FromJSON (parseJSON), withObject)
import Types
import Api.Request (DraftRequest (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson (Object, Value (..), decode)
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict (toList)
import Data.Text (Text, unpack)
import qualified Data.Vector as V
import Oops (ReqError (..), hideTokenErr)
import ParseQueryStr (checkLength)
import TryRead (checkBigIntId)
import Types
import Data.Scientific (Scientific,floatingOrInteger)

checkResourse :: Handle m -> [Text] -> ExceptT ReqError (Maybe Id)
checkResourse h ["users",usIdTxt] =
  iD <- tryReadId "user_id" usIdTxt
  isExistResE h (UserId iD)
  return . Just $ iD
checkResourse h ["authors",auIdTxt] =
  iD <- tryReadId "author_id" auIdTxt
  isExistResE h (AuthorId iD)
  return . Just $ iD
checkResourse h ["categories",catIdTxt] =
  iD <- tryReadId "category_id" catIdTxt
  isExistResE h (CategoryId iD)
  return . Just $ iD
checkResourse h ["tags",tagIdTxt] =
  iD <- tryReadId "tag_id" tagIdTxt
  isExistResE h (TagId iD)
  return . Just $ iD
checkResourse h ("drafts":draftIdTxt:_) =
  iD <- tryReadId "draft_id" draftIdTxt
  isExistResE h (DraftId iD)
  return . Just $ iD
checkResourse h ("posts":postIdTxt:_) =
  iD <- tryReadId "post_id" postIdTxt
  isExistResE h (PostId iD)
  return . Just $ iD
checkResourse h ("comments":commIdTxt:_) =
  iD <- tryReadId "comment_id" commIdTxt
  isExistResE h (CommentId iD)
  return . Just $ iD
checkResourse h ["pictures",picIdTxt] =
  iD <- tryReadId "picture_id" picIdTxt
  isExistResE h (PictureId iD)
  return . Just $ iD
checkResourse _ _ = return Nothing

