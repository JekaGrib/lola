{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Tag.Handlers where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catch, throwM)
import Control.Monad.State (StateT (..), withStateT)
import Data.Text (Text, unpack)
import Spec.Log (handLogDebug)
import Methods.Tag
import Spec.Oops (UnexpectedArgsException (..))
import Spec.TestDB
import Types
import Spec.Types (MockAction (..))

handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogDebug
    selectTxtsTest
    updateInDBTest
    deleteFromDbTest
    isExistInDbTest
    insertReturnTest
    withTransactionDBTest

withTransactionDBTest :: StateT [MockAction] IO a -> StateT [MockAction] IO a
withTransactionDBTest m = do
  StateT $ \acts -> return ((), TRANSACTIONOPEN : acts)
  a <- m `catch` (\(e :: SomeException) -> StateT $ \acts -> return (a, TRANSACTIONunROLL : acts)
  StateT $ \acts -> return (a, TRANSACTIONCLOSE : acts)

insertReturnTest :: String -> String -> [String] -> [Text] -> StateT [MockAction] IO Integer
insertReturnTest table returnName insNames insValues = StateT $ \acts ->
  return $ (14,(INSERTDATA table returnName insNames insValues : acts))

selectTxtsTest :: Table -> [Param] -> Where -> [Text] -> StateT [MockAction] IO [Text]
selectTxtsTest "tags" ["tag_name"] "tag_id=?" [txtNum] = StateT $ \(db, acts) ->
  return (selectTxtsFromTags txtNum db, (db, SELECTDATA : acts))
selectTxtsTest _ _ _ _ = throwM UnexpectedArgsException

updateInDBTest :: Table -> String -> String -> [Text] -> StateT [MockAction] IO ()
updateInDBTest "tags" "tag_name=?" "tag_id=?" [tagNameParam, tagIdParam] = StateT $ \(db, acts) ->
  return $ updateInTags tagNameParam tagIdParam db (UPDATEDATA : acts)
updateInDBTest _ _ _ _ = throwM UnexpectedArgsException

isExistInDbTest :: Table -> String -> String -> [Text] -> StateT [MockAction] IO Bool
isExistInDbTest "tags" "tag_id" "tag_id=?" [txtNum] = StateT $ \(db, acts) ->
  return (isExistInTags txtNum db, (db, EXISTCHEK : acts))
isExistInDbTest _ _ _ _ = throwM UnexpectedArgsException

deleteFromDbTest :: Table -> String -> [Text] -> StateT [MockAction] IO ()
deleteFromDbTest "draftstags" "tag_id=?" [txtNum] = StateT $ \(db, acts) ->
  return $ deleteFromDraftsTags txtNum db (DELETEDATA : acts)


