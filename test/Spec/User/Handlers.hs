{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.User.Handlers where

import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT (..), modify)
import Database.PostgreSQL.Simple (ExecStatus (FatalError), SqlError (..))
import Methods.User
import qualified Spec.Auth.Handlers (handle0)
import Spec.Conf (defConf)
import qualified Spec.Exist.Handlers (handle)
import qualified Spec.DeleteMany.Handlers (handle)
import Spec.Log (handLogDebug,handLogWarning)
import Spec.User.Types
import Spec.Types (MockAction (..))
import Types
import Oops (UnexpectedDbOutPutException(..))
import Psql.Selecty (Author(..),User(..),Auth(..),Author(..))
import Data.Time.Calendar (Day,fromGregorian)



handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    selectUsersTest 
    (selectAuthsForUserTest [Auth "37fa265330ad83eaa879efb1e2db6380896cf639" False])
    selectAuthorsForUserTest 
    selectDraftsForAuthorTest 
    updateDbUserForCommsTest
    updateDbAuthorForPostsTest
    updateDbTokenKeyForUserTest
    deleteDbUserTest
    deleteDbAuthorTest
    insertReturnUserTest
    getDayTest
    generateTokenKeyTest
    withTransactionDBTest
    Spec.DeleteMany.Handlers.handle
    Spec.Auth.Handlers.handle0
    Spec.Exist.Handlers.handle

throwSqlEx :: StateT [MockAction] IO a
throwSqlEx = throwM $ SqlError "oops" FatalError "oops" "oops" "oops"


handle1 :: Handle (StateT [MockAction] IO)
handle1 = handle {selectAuthsForUser = selectAuthsForUserTest [Auth "37fa265330ad83eaa879efb1e2db6380896cf639" True]}
{-}
handle2 :: Handle (StateT [MockAction] IO)
handle2 = handle {selectTagNames = selectTagNamesTest []}

handle3 :: Handle (StateT [MockAction] IO)
handle3 =
  handle
    { selectTagNames = selectTagNamesTestEx
    , insertReturnTag = insertReturnTagTestEx
    , updateDbTag = updateDbTagTestEx
    , deleteDbTag = deleteDbTagTestEx
    }

handle4 :: Handle (StateT [MockAction] IO)
handle4 =
  handle
    { deleteDbTagForDrafts = deleteDbTagForDraftsTestEx
    }

handle5 :: Handle (StateT [MockAction] IO)
handle5 =
  handle
    { insertReturnTag = insertReturnTagTestEx1
    }
-}
withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  modify (TRANSACTIONOPEN :)
  a <- m
  modify (TRANSACTIONCLOSE :)
  return a



selectUsersTest :: UserId -> StateT [MockAction] IO [User]
selectUsersTest uId = do
  modify (UserMock (SelectUsers uId) :)
  return [User "fName" "lName" 4 (fromGregorian 2020 02 02)]

selectAuthsForUserTest :: [Auth] -> UserId -> StateT [MockAction] IO [Auth]
selectAuthsForUserTest auths uId = do
  modify (UserMock (SelectAuthsForUser uId) :)
  return auths

selectAuthorsForUserTest :: UserId -> StateT [MockAction] IO [AuthorId]
selectAuthorsForUserTest uId = do
  modify (UserMock (SelectAuthorsForUser uId) :)
  return [4] 

selectDraftsForAuthorTest :: AuthorId -> StateT [MockAction] IO [DraftId]
selectDraftsForAuthorTest aId =  do
  modify (UserMock (SelectDraftsForAuthor aId) :)
  return [2,5]

updateDbUserForCommsTest :: UserId -> UserId -> StateT [MockAction] IO ()
updateDbUserForCommsTest uId newUId = do
  modify (UserMock (UpdateDbUserForComms uId newUId) :)

updateDbAuthorForPostsTest :: AuthorId -> AuthorId -> StateT [MockAction] IO ()
updateDbAuthorForPostsTest aId newAId =  do
  modify (UserMock (UpdateDbAuthorForPosts aId newAId) :)

updateDbTokenKeyForUserTest :: TokenKey -> UserId -> StateT [MockAction] IO ()
updateDbTokenKeyForUserTest tk uI = do
  modify (UserMock (UpdateDbTokenKeyForUser tk uI) :)

deleteDbUserTest :: UserId -> StateT [MockAction] IO ()
deleteDbUserTest uId =  do
  modify (UserMock (DeleteDbUser uId) :)

deleteDbAuthorTest :: AuthorId -> StateT [MockAction] IO ()
deleteDbAuthorTest aId = do
  modify (UserMock (DeleteDbAuthor aId) :)

insertReturnUserTest :: InsertUser -> StateT [MockAction] IO UserId
insertReturnUserTest insU = do
  modify (UserMock (InsertReturnUser insU) :)
  return 14

getDayTest :: StateT [MockAction] IO Day
getDayTest =  do
  modify (UserMock GetDay :)
  return (fromGregorian 2020 02 02)

generateTokenKeyTest :: StateT [MockAction] IO TokenKey
generateTokenKeyTest = do
  modify (UserMock GenerateTokenKey :)
  return "lilu"


{-selectTagNamesTest :: [TagName] -> TagId -> StateT [MockAction] IO [TagName]
selectTagNamesTest xs tagId = do
  modify (TagMock (SelectTagNames tagId) :)
  return xs

updateDbTagTest :: TagName -> TagId -> StateT [MockAction] IO ()
updateDbTagTest tagName tagId =
  modify (TagMock (UpdateDbTag tagName tagId) :)

deleteDbTagTest :: TagId -> StateT [MockAction] IO ()
deleteDbTagTest tagId =
  modify (TagMock (DeleteDbTag tagId) :)

deleteDbTagForDraftsTest :: TagId -> StateT [MockAction] IO ()
deleteDbTagForDraftsTest tagId =
  modify (TagMock (DeleteDbTagForDrafts tagId) :)

deleteDbTagForPostsTest :: TagId -> StateT [MockAction] IO ()
deleteDbTagForPostsTest tagId =
  modify (TagMock (DeleteDbTagForPosts tagId) :)

insertReturnTagTest :: TagName -> StateT [MockAction] IO TagId
insertReturnTagTest tagName = do
  modify (TagMock (InsertReturnTag tagName) :)
  return 14

selectTagNamesTestEx :: TagId -> StateT [MockAction] IO [TagName]
selectTagNamesTestEx _ = throwSqlEx

deleteDbTagTestEx :: TagId -> StateT [MockAction] IO ()
deleteDbTagTestEx _ = throwSqlEx

deleteDbTagForDraftsTestEx :: TagId -> StateT [MockAction] IO ()
deleteDbTagForDraftsTestEx _ = throwSqlEx

updateDbTagTestEx :: TagName -> TagId -> StateT [MockAction] IO ()
updateDbTagTestEx _ _ = throwSqlEx

insertReturnTagTestEx :: TagName -> StateT [MockAction] IO TagId
insertReturnTagTestEx _ = throwSqlEx

insertReturnTagTestEx1 :: TagName -> StateT [MockAction] IO TagId
insertReturnTagTestEx1 _ = throwM $ UnexpectedEmptyDbOutPutException-}