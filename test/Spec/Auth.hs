{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Auth where

import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Logger (Priority (..))
import Methods.Common.Auth (tokenAdminAuth)
import Oops (ReqError (..))
import Spec.Auth.Handlers
import Spec.Auth.QStrExample
import Spec.Auth.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)

testAuth :: IO ()
testAuth = hspec
  $ describe "tokenAdminAuth"
  $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ tokenAdminAuth handle qStr1) []
      reverse state
        `shouldBe` [LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO]
    it "throw Auth Error on wrong token" $ do
      state <- execStateT (runExceptT $ tokenAdminAuth handle qStr2) []
      reverse state
        `shouldBe` [LOG INFO]
      eitherResp <- evalStateT (runExceptT $ tokenAdminAuth handle qStr2) []
      eitherResp
        `shouldBe` Left (SecretError "SecretTokenError \"INVALID token\"")
    it "throw Auth Error on wrong token key" $ do
      state <- execStateT (runExceptT $ tokenAdminAuth handle qStr3) []
      reverse state
        `shouldBe` [LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO]
      eitherResp <- evalStateT (runExceptT $ tokenAdminAuth handle qStr3) []
      eitherResp
        `shouldBe` Left (SecretError "SecretTokenError \"INVALID token. Wrong token key or user_id\"")
