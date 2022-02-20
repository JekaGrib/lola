{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Tag where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catch, throwM)
import Control.Monad.State (StateT (..), evalStateT, execStateT, withStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Builder (toLazyByteString)
import Data.Text (Text, unpack)
import Logger (Priority (..))
import Spec.Log (handLogDebug)
import Methods.Common (resBuilder)
import Methods.Tag
import Spec.Oops (UnexpectedArgsException (..))
import ParseQueryStr (CreateTag (..), DeleteTag (..), UpdateTag (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Spec.TestDB
import Types
import Spec.Types (MockAction (..))
import Spec.Tag.Handlers

testTag :: IO ()
testTag = hspec
  $ describe "createTag"
  $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createTag handle (CreateTag "cats")) (testDB1, [])
      (reverse . snd $ state)
        `shouldBe` 
        [LOG DEBUG
        , INSERTDATA "tags" "tag_id" ["tag_name"] ["cats"]
        , LOG INFO
        , LOG INFO
        ]
      respE <- evalStateT (runExceptT $ createTag handle (CreateTag "cats")) (testDB1, [])
      let respBuildE = fmap (toLazyByteString . resBuilder) respE
      respBuildE
        `shouldBe` Right "{\"tag_id\":16,\"tag_name\":\"cats\"}"
    it "work" $ do
      state <- execStateT (runExceptT $ getTag handle 4) (testDB1, [])
      (reverse . snd $ state)
        `shouldBe` [LOG DEBUG, LOG INFO, LOG INFO]
      respE <- evalStateT (runExceptT $ getTag handle 4) (testDB1, [])
      let respBuildE = fmap (toLazyByteString . resBuilder) respE
      respBuildE
        `shouldBe` Right "{\"tag_id\":4,\"tag_name\":\"Love\"}"
    it "work" $ do
      state <- execStateT (runExceptT $ updateTag handle (UpdateTag 2 "Salad")) (testDB1, [])
      (reverse . snd $ state)
        `shouldBe` [LOG DEBUG, LOG INFO, LOG DEBUG, LOG INFO, LOG INFO]
      respE <- evalStateT (runExceptT $ updateTag handle (UpdateTag 2 "Salad")) (testDB1, [])
      let respBuildE = fmap (toLazyByteString . resBuilder) respE
      respBuildE
        `shouldBe` Right "{\"tag_id\":2,\"tag_name\":\"Salad\"}"
    it "work" $ do
      state <- execStateT (runExceptT $ deleteTag handle (DeleteTag 3)) (testDB1, [])
      (reverse . snd $ state)
        `shouldBe` [LOG DEBUG, LOG INFO, LOG DEBUG, TRANSACTIONOPEN,  TRANSACTIONCLOSE, LOG INFO, LOG INFO]
      respE <- evalStateT (runExceptT $ deleteTag handle (DeleteTag 3)) (testDB1, [])
      let respBuildE = fmap (toLazyByteString . resBuilder) respE
      respBuildE
        `shouldBe` Right "{\"ok\":true}"
