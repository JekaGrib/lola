{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.User where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (LogIn (..))
import Api.Response (TokenResponse (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Data.Text (pack)
import Logger (Priority (..))
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader,strSha1)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.User
import Network.HTTP.Types (status200, status201, status204)
import Oops (ReqError (..))
import Spec.Auth.Types
import Spec.Exist.Types
import Spec.User.Handlers
import Spec.User.QStrExample
import Spec.User.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)

testUser :: IO ()
testUser = hspec $ do
  describe "logIn" $ do
    it "work for user" $ do
      state <- execStateT (runExceptT $ logIn handle (LogIn 4 "pwd")) []
      reverse state
        `shouldBe` [UserMock (SelectAuthsForUser 4),UserMock GenerateTokenKey,UserMock (UpdateDbTokenKeyForUser "lilu" 4)]
      eitherResp <- evalStateT (runExceptT $ logIn handle (LogIn 4 "pwd")) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ TokenResponse $ pack ("4.stu." ++ strSha1 "stulilu")))
    it "work for admin" $ do
      state <- execStateT (runExceptT $ logIn handle1 (LogIn 4 "pwd")) []
      reverse state
        `shouldBe` [UserMock (SelectAuthsForUser 4),UserMock GenerateTokenKey,UserMock (UpdateDbTokenKeyForUser "lilu" 4)]
      eitherResp <- evalStateT (runExceptT $ logIn handle1 (LogIn 4 "pwd")) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ TokenResponse $ pack ("4.hij." ++ strSha1 "hijlilu")))
    it "throw LogIn Error to wrong password" $ do
      state <- execStateT (runExceptT $ logIn handle (LogIn 4 "oops")) []
      reverse state
        `shouldBe` [UserMock (SelectAuthsForUser 4)]
      eitherResp <- evalStateT (runExceptT $ logIn handle (LogIn 4 "oops")) []
      eitherResp
        `shouldBe`  (Left (SecretLogInError "INVALID password"))
  describe "workWithLogIn" $ do
    it "work for valid query string" $ do
      state <- execStateT (runExceptT $ workWithLogIn handle qStr3) []
      reverse state
        `shouldBe` [ExistMock (IsExist (UserId 4)),UserMock (SelectAuthsForUser 4),UserMock GenerateTokenKey,UserMock (UpdateDbTokenKeyForUser "lilu" 4)]
      eitherResp <- evalStateT (runExceptT $ workWithLogIn handle qStr3) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ TokenResponse $ pack ("4.stu." ++ strSha1 "stulilu")))
    it "throw BadReq Error on not exist user" $ do
      state <- execStateT (runExceptT $ workWithLogIn handle qStr4) []
      reverse state
        `shouldBe`  [ExistMock (IsExist (UserId 200))]
      eitherResp <- evalStateT (runExceptT $ workWithLogIn handle qStr4) []
      eitherResp
        `shouldBe` (Left (SecretLogInError "BadReqError \"user_id: 200 doesn`t exist\""))
    it "throw BadReq Error on not valid query string" $ do
      state <- execStateT (runExceptT $ workWithLogIn handle qStr1) []
      reverse state
        `shouldBe`  []
      eitherResp <- evalStateT (runExceptT $ workWithLogIn handle qStr1) []
      eitherResp
        `shouldBe` Left (SecretLogInError "BadReqError \"Can't find parameter:user_id\"")