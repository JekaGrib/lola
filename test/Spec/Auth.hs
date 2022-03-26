{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Auth where

import Spec.Conf (defConf)
import Control.Monad.Catch (SomeException, catch, throwM)
import Control.Monad.State (StateT (..), evalStateT, execStateT, withStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Builder (toLazyByteString)
import Data.Text (Text, unpack)
import Logger (Priority (..))
import Oops (ReqError(..))
import Methods.Common.Auth (tokenAdminAuth)
import Spec.Oops (UnexpectedArgsException (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Spec.Types (MockAction (..))
import Spec.Auth.Handlers
import Spec.Auth.Types
import Spec.Auth.QStrExample

testAuth :: IO ()
testAuth = hspec $ 
  describe "tokenAdminAuth" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ tokenAdminAuth handle qStr1) []
      reverse state
        `shouldBe` 
        [LOG INFO,LOG DEBUG,AuthMock (SelectTokenKeyForUser 152),LOG INFO,LOG INFO]
    it "throw Auth Error on wrong token" $ do
      state <- execStateT (runExceptT $ tokenAdminAuth handle qStr2) []
      reverse state
        `shouldBe` 
        [LOG INFO]
      eitherResp <- evalStateT (runExceptT $ tokenAdminAuth handle qStr2 ) []
      eitherResp
        `shouldBe` 
          Left (SecretError "SecretTokenError \"INVALID token\"")
    it "throw Auth Error on wrong token key" $ do
      state <- execStateT (runExceptT $ tokenAdminAuth handle qStr3) []
      reverse state
        `shouldBe` 
        [LOG INFO,LOG DEBUG,AuthMock (SelectTokenKeyForUser 152),LOG INFO]
      eitherResp <- evalStateT (runExceptT $ tokenAdminAuth handle qStr3 ) []
      eitherResp
        `shouldBe` 
          Left (SecretError "SecretTokenError \"INVALID token. Wrong token key or user_id\"")