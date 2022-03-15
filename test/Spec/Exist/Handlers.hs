{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Exist.Handlers where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catch, throwM)
import Control.Monad.State (StateT (..), withStateT,modify)
import Data.Text (Text, unpack)
import Spec.Log (handLogDebug)
import Spec.Oops (UnexpectedArgsException (..))
import Spec.TestDB
import Types
import Spec.Types (MockAction (..))
import Spec.Exist.Types
import Methods.Common.Exist.UncheckedExId (UncheckedExId(..))
import Methods.Common.Exist (Handle(..))

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    isExistTest


isExistTest :: UncheckedExId -> StateT [MockAction] IO Bool
isExistTest unchId@(AuthorId x) = do
  modify (ExistMock (IsExist unchId) :)
  return (x <= 100)
isExistTest unchId@(CategoryId x) = do
  modify (ExistMock (IsExist unchId) :)
  return (x <= 100)
isExistTest unchId@(CommentId x) = do
  modify (ExistMock (IsExist unchId) :)
  return (x <= 100)
isExistTest unchId@(DraftId x) = do
  modify (ExistMock (IsExist unchId) :)
  return (x <= 100)
isExistTest unchId@(PictureId x) = do
  modify (ExistMock (IsExist unchId) :)
  return (x <= 100)
isExistTest unchId@(PostId x) = do
  modify (ExistMock (IsExist unchId) :)
  return (x <= 100)
isExistTest unchId@(TagId x) = do
  modify (ExistMock (IsExist unchId) :)
  return (x <= 100)
isExistTest unchId@(UserId x) = do
  modify (ExistMock (IsExist unchId) :)
  return (x <= 100)

