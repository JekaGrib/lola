{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Author.Handlers where

import Control.Monad.State (StateT (..), modify)
import Methods.Author
import Psql.Selecty (Author (..))
import qualified Spec.Auth.Handlers (handle)
import Spec.Author.Types
import Spec.Conf (defConf)
import qualified Spec.DeleteMany.Handlers (handle)
import qualified Spec.Exist.Handlers (handle)
import Spec.Log (handLogWarning)
import Spec.Types (MockAction (..))
import Types

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    selectDraftsForAuthorTest
    selectAuthorsForUserTest
    selectAuthorsTest
    updateDbAuthorTest
    updateDbAuthorForPostsTest
    deleteDbAuthorTest
    isUserAuthorTest
    insertReturnAuthorTest
    withTransactionDBTest
    Spec.DeleteMany.Handlers.handle
    Spec.Auth.Handlers.handle
    Spec.Exist.Handlers.handle

withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  modify (TRANSACTIONOPEN :)
  a <- m
  modify (TRANSACTIONCLOSE :)
  return a

selectDraftsForAuthorTest :: AuthorId -> StateT [MockAction] IO [DraftId]
selectDraftsForAuthorTest aId = do
  modify (AuthorMock (SelectDraftsForAuthor aId) :)
  return [2, 5]

selectAuthorsForUserTest :: UserId -> StateT [MockAction] IO [AuthorId]
selectAuthorsForUserTest uId = do
  modify (AuthorMock (SelectAuthorsForUser uId) :)
  return []

selectAuthorsTest :: AuthorId -> StateT [MockAction] IO [Author]
selectAuthorsTest aId = do
  modify (AuthorMock (SelectAuthors aId) :)
  return [Author aId "author" 3]

updateDbAuthorTest :: UserId -> AuthorInfo -> AuthorId -> StateT [MockAction] IO ()
updateDbAuthorTest uId aInfo aId =
  modify (AuthorMock (UpdateDbAuthor uId aInfo aId) :)

updateDbAuthorForPostsTest :: AuthorId -> AuthorId -> StateT [MockAction] IO ()
updateDbAuthorForPostsTest newAId aId =
  modify (AuthorMock (UpdateDbAuthorForPosts newAId aId) :)

deleteDbAuthorTest :: AuthorId -> StateT [MockAction] IO ()
deleteDbAuthorTest aId =
  modify (AuthorMock (DeleteDbAuthor aId) :)

isUserAuthorTest :: UserId -> StateT [MockAction] IO Bool
isUserAuthorTest uId = do
  modify (AuthorMock (IsUserAuthor uId) :)
  return False

insertReturnAuthorTest :: UserId -> AuthorInfo -> StateT [MockAction] IO AuthorId
insertReturnAuthorTest uId aInfo = do
  modify (AuthorMock (InsertReturnAuthor uId aInfo) :)
  return 14
