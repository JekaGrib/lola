{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Tag where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catch, throwM)
import Control.Monad.State (StateT (..), evalStateT, execStateT, withStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Builder (toLazyByteString,lazyByteString)
import Data.Text (Text, unpack)
import Logger (Priority (..))
import Spec.Log (handLogDebug)
import Methods.Common (jsonHeader,textHeader,ResponseInfo(..))
import Methods.Tag
import Oops (ReqError (..))
import Spec.Oops (UnexpectedArgsException (..))
import Api.Request.QueryStr (CreateTag (..),  UpdateTag (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types
import Spec.Types (MockAction (..))
import Spec.Tag.Handlers
import Spec.Tag.Types
import Spec.Tag.QStrExample
import Spec.Auth.Types
import Data.Aeson (encode)
import Api.Response (TagResponse(..))
import Network.HTTP.Types (status200,status201,status204)
import Api.Request.EndPoint (AppMethod(..))

testTag :: IO ()
testTag = hspec $ do
  describe "createTag" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createTag handle (CreateTag "cats")) []
      reverse state
        `shouldBe` 
        [LOG DEBUG,TagMock (InsertReturnTag "cats"),LOG INFO,LOG INFO]
      eitherResp <- evalStateT (runExceptT $ createTag handle (CreateTag "cats")) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status201 [textHeader,("Location","http://localhost:3000/tags/14")] "Status 201 Created")
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ createTag handle3 (CreateTag "cats")) []
      reverse state
        `shouldBe` 
        [LOG DEBUG]
      eitherResp <- evalStateT (runExceptT $ createTag handle3 (CreateTag "cats")) []
      eitherResp
        `shouldBe` 
          (Left (DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}"))
  describe "getTag" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getTag handle 3) []
      reverse state
        `shouldBe` 
        [LOG DEBUG,TagMock (SelectTagNames 3),LOG INFO,LOG INFO]
      eitherResp <- evalStateT (runExceptT $ getTag handle 3) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status200 [jsonHeader] (encode $ TagResponse 3 "cats"))
    it "throw DBError with multiple DB answer" $ do
      state <- execStateT (runExceptT $ getTag handle1 3) []
      reverse state
        `shouldBe` 
        [LOG DEBUG,TagMock (SelectTagNames 3),LOG INFO]
      eitherResp <- evalStateT (runExceptT $ getTag handle1 3) []
      eitherResp
        `shouldBe` 
          Left (DatabaseError "Output not single[\"cats\",\"food\"]")
    it "throw DBError with empty DB answer" $ do
      state <- execStateT (runExceptT $ getTag handle2 3) []
      reverse state
        `shouldBe` 
        [LOG DEBUG,TagMock (SelectTagNames 3),LOG INFO]
      eitherResp <- evalStateT (runExceptT $ getTag handle2 3) []
      eitherResp
        `shouldBe` 
          Left (DatabaseError "Empty output")
  describe "updateTag" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ updateTag handle 7 (UpdateTag "food")) []
      reverse state
        `shouldBe` 
        [LOG DEBUG,TagMock (UpdateDbTag "food" 7),LOG INFO,LOG INFO]
      eitherResp <- evalStateT (runExceptT $ updateTag handle 7 (UpdateTag "food")) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status200 [jsonHeader] (encode $ TagResponse 7 "food"))
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ updateTag handle3 7 (UpdateTag "food")) []
      reverse state
        `shouldBe` 
        [LOG DEBUG]
      eitherResp <- evalStateT (runExceptT $ updateTag handle3 7 (UpdateTag "food")) []
      eitherResp
        `shouldBe` 
          (Left (DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}"))
  describe "deleteTag" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ deleteTag handle 7 ) []
      reverse state
        `shouldBe` 
        [LOG DEBUG,TRANSACTIONOPEN,TagMock (DeleteDbTagForDrafts 7),TagMock (DeleteDbTagForPosts 7),TagMock (DeleteDbTag 7),TRANSACTIONCLOSE,LOG INFO,LOG INFO]
      eitherResp <- evalStateT (runExceptT $ deleteTag handle 7 ) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ deleteTag handle3 7 ) []
      reverse state
        `shouldBe` 
        [LOG DEBUG]
      eitherResp <- evalStateT (runExceptT $ deleteTag handle3 7 ) []
      eitherResp
        `shouldBe` 
          (Left (DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}"))
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ deleteTag handle4 7 ) []
      reverse state
        `shouldBe` 
        [LOG DEBUG]
      eitherResp <- evalStateT (runExceptT $ deleteTag handle4 7 ) []
      eitherResp
        `shouldBe` 
          (Left (DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}"))
  describe "workWithTags (ToPost)" $ do
    it "work with valid token" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr2 ToPost ) []
      reverse state
        `shouldBe` 
        [LOG INFO,LOG INFO,LOG DEBUG,AuthMock (SelectTokenKeyForUser 152),LOG INFO,LOG INFO,LOG DEBUG,TagMock (InsertReturnTag "dogs"),LOG INFO,LOG INFO]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr2 ToPost ) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status201 [textHeader,("Location","http://localhost:3000/tags/14")] "Status 201 Created")
    it "throw BadReq Error on wrong QString" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr1 ToPost ) []
      reverse state
        `shouldBe` 
        [LOG INFO,LOG INFO,LOG DEBUG,AuthMock (SelectTokenKeyForUser 152),LOG INFO,LOG INFO]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr1 ToPost ) []
      eitherResp
        `shouldBe` 
          (Left $ BadReqError "Can't find parameter:tag_name")