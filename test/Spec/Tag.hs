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
import Methods.Common (jsonHeaders,ResponseInfo(..))
import Methods.Tag
import Spec.Oops (UnexpectedArgsException (..))
import Api.Request.QueryStr (CreateTag (..),  UpdateTag (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types
import Spec.Types (MockAction (..))
import Spec.Tag.Handlers
import Spec.Tag.Types
import Data.Aeson (encode)
import Api.Response (TagResponse(..))
import Network.HTTP.Types (status200,status204)

testTag :: IO ()
testTag = hspec $ do
  describe "createTag" $
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createTag handle (CreateTag "cats")) []
      reverse state
        `shouldBe` 
        [LOG DEBUG,TagMock (InsertReturnTag "cats"),LOG INFO,LOG INFO]
      eitherResp <- evalStateT (runExceptT $ createTag handle (CreateTag "cats")) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status200 jsonHeaders (encode $ TagResponse 14 "cats"))
  describe "getTag" $
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getTag handle 3) []
      reverse state
        `shouldBe` 
        [LOG DEBUG,TagMock (SelectTagNames 3),LOG INFO,LOG INFO]
      eitherResp <- evalStateT (runExceptT $ getTag handle 3) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status200 jsonHeaders (encode $ TagResponse 3 "cats"))
  describe "updateTag" $
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ updateTag handle 7 (UpdateTag "food")) []
      reverse state
        `shouldBe` 
        [LOG DEBUG,TagMock (UpdateDbTag "food" 7),LOG INFO,LOG INFO]
      eitherResp <- evalStateT (runExceptT $ updateTag handle 7 (UpdateTag "food")) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status200 jsonHeaders (encode $ TagResponse 7 "food"))
  describe "deleteTag" $
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ deleteTag handle 7 ) []
      reverse state
        `shouldBe` 
        [LOG DEBUG,TRANSACTIONOPEN,TagMock (DeleteDbTagForDrafts 7),TagMock (DeleteDbTagForPosts 7),TagMock (DeleteDbTag 7),TRANSACTIONCLOSE,LOG INFO,LOG INFO]
      eitherResp <- evalStateT (runExceptT $ deleteTag handle 7 ) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status204 [] "Status 204 No data")
