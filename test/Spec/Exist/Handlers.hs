module Spec.Exist.Handlers where

import Control.Monad.State (StateT (..), modify)
import Methods.Common.Exist (Handle (..))
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Spec.Conf (defConf)
import Spec.Exist.Types
import Spec.Types (MockAction (..))

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
