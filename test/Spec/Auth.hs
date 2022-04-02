{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Auth where

import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Logger (Priority (..))
import Methods.Common.Auth
import Oops (ReqError (..))
import Spec.Auth.Handlers
import Spec.Auth.QStrExample
import Spec.Auth.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)

testAuth :: IO ()
testAuth = hspec $ do
  describe "tokenAdminAuth" $ do
    it "work with Admin token" $ do
      state <- execStateT (runExceptT $ tokenAdminAuth handle0 qStr1) []
      reverse state
        `shouldBe` [LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO]
    it "throw Secret Error on User token" $ do
      state <- execStateT (runExceptT $ tokenAdminAuth handle0 qStr4) []
      reverse state
        `shouldBe` [LOG INFO]
      eitherResp <- evalStateT (runExceptT $ tokenAdminAuth handle0 qStr4) []
      eitherResp
        `shouldBe` Left (SecretError "SecretTokenError \"INVALID token\"")
    it "throw Secret Error on wrong token" $ do
      state <- execStateT (runExceptT $ tokenAdminAuth handle0 qStr2) []
      reverse state
        `shouldBe` [LOG INFO]
      eitherResp <- evalStateT (runExceptT $ tokenAdminAuth handle0 qStr2) []
      eitherResp
        `shouldBe` Left (SecretError "SecretTokenError \"INVALID token\"")
    it "throw Secret Error on wrong token key" $ do
      state <- execStateT (runExceptT $ tokenAdminAuth handle0 qStr3) []
      reverse state
        `shouldBe` [LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO]
      eitherResp <- evalStateT (runExceptT $ tokenAdminAuth handle0 qStr3) []
      eitherResp
        `shouldBe` Left (SecretError "SecretTokenError \"INVALID token. Wrong token key or user_id\"")
  describe "tokenUserAuth" $ do
    it "work with Admin token" $ do
      state <- execStateT (runExceptT $ tokenUserAuth handle0 qStr1) []
      reverse state
        `shouldBe` [LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO]
    it "work with User token" $ do
      state <- execStateT (runExceptT $ tokenUserAuth handle0 qStr4) []
      reverse state
        `shouldBe` [LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO]
    it "throw SecretTokenError  on wrong token" $ do
      state <- execStateT (runExceptT $ tokenUserAuth handle0 qStr2) []
      reverse state
        `shouldBe` [LOG INFO]
      eitherResp <- evalStateT (runExceptT $ tokenUserAuth handle0 qStr2) []
      eitherResp
        `shouldBe` Left (SecretTokenError "INVALID token")
    it "throw SecretTokenError  on wrong token key" $ do
      state <- execStateT (runExceptT $ tokenUserAuth handle0 qStr3) []
      reverse state
        `shouldBe` [LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO]
      eitherResp <- evalStateT (runExceptT $ tokenUserAuth handle0 qStr3) []
      eitherResp
        `shouldBe` Left (SecretTokenError "INVALID token. Wrong token key or user_id")
