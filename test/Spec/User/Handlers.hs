{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.User.Handlers where

import Control.Monad.State (StateT (..), modify)
import Methods.User
import qualified Spec.Auth.Handlers (handle)
import Spec.Conf (defConf)
import qualified Spec.Exist.Handlers (handle)
import qualified Spec.DeleteMany.Handlers (handle)
import Spec.Log (handLogWarning)
import Spec.User.Types
import Spec.Types (MockAction (..))
import Types
import Psql.Selecty (User(..),Auth(..))
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
    Spec.Auth.Handlers.handle
    Spec.Exist.Handlers.handle


handle1 :: Handle (StateT [MockAction] IO)
handle1 = handle {selectAuthsForUser = selectAuthsForUserTest [Auth "37fa265330ad83eaa879efb1e2db6380896cf639" True]}

withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  modify (TRANSACTIONOPEN :)
  a <- m
  modify (TRANSACTIONCLOSE :)
  return a



selectUsersTest :: UserId -> StateT [MockAction] IO [User]
selectUsersTest uId = do
  modify (UserMock (SelectUsers uId) :)
  return [User "fName" "lName" 4 dayExample]

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
updateDbUserForCommsTest newUId uId  = 
  modify (UserMock (UpdateDbUserForComms newUId uId ) :)

updateDbAuthorForPostsTest :: AuthorId -> AuthorId -> StateT [MockAction] IO ()
updateDbAuthorForPostsTest newAId aId  =  
  modify (UserMock (UpdateDbAuthorForPosts newAId aId ) :)

updateDbTokenKeyForUserTest :: TokenKey -> UserId -> StateT [MockAction] IO ()
updateDbTokenKeyForUserTest tk uI = 
  modify (UserMock (UpdateDbTokenKeyForUser tk uI) :)

deleteDbUserTest :: UserId -> StateT [MockAction] IO ()
deleteDbUserTest uId =  
  modify (UserMock (DeleteDbUser uId) :)

deleteDbAuthorTest :: AuthorId -> StateT [MockAction] IO ()
deleteDbAuthorTest aId = 
  modify (UserMock (DeleteDbAuthor aId) :)

insertReturnUserTest :: InsertUser -> StateT [MockAction] IO UserId
insertReturnUserTest insU = do
  modify (UserMock (InsertReturnUser insU) :)
  return 14

getDayTest :: StateT [MockAction] IO Day
getDayTest =  do
  modify (UserMock GetDay :)
  return dayExample

dayExample :: Day
dayExample = fromGregorian 2020 02 02

generateTokenKeyTest :: StateT [MockAction] IO TokenKey
generateTokenKeyTest = do
  modify (UserMock GenerateTokenKey :)
  return "lilu"


