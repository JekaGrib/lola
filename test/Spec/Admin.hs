{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Admin where

import Api.Request.QueryStr (CreateAdmin(..))
import Api.Response (TokenResponse (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Data.Text (pack)
import Methods.Common (ResponseInfo (..), jsonHeader,strSha1)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Admin
import Network.HTTP.Types ( status201)
import Spec.Exist.Types
import Spec.Admin.Handlers
import Spec.Admin.QStrExample
import Spec.Admin.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types

testAdmin :: IO ()
testAdmin = hspec $ do
  describe "createAdmin" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createAdmin handle (CreateAdmin "lola" "pwd" "fName" "lName" 6)) []
      reverse state
        `shouldBe` 
        [ AdminMock SelectKeys
        , AdminMock GetDay
        , AdminMock GenerateTokenKey
        , AdminMock (InsertReturnUser (InsertUser "37fa265330ad83eaa879efb1e2db6380896cf639" "fName" "lName" 6 dayExample True "lilu"))
        ]
      eitherResp <- evalStateT (runExceptT $ createAdmin handle (CreateAdmin "lola" "pwd" "fName" "lName" 6)) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status201 [jsonHeader,("Location","http://localhost:3000/users/14")] (encode $ TokenResponse $ pack ("14.hij." ++ strSha1 "hijlilu")))
  describe "workWithAdmin " $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithAdmin handle qStr1) []
      reverse state
        `shouldBe` 
        [ ExistMock (IsExist (PictureId 6))
        , AdminMock SelectKeys
        , AdminMock GetDay
        , AdminMock GenerateTokenKey
        , AdminMock (InsertReturnUser (InsertUser "37fa265330ad83eaa879efb1e2db6380896cf639" "fName" "lName" 6 dayExample True "lilu"))
        ]
      eitherResp <- evalStateT (runExceptT $ workWithAdmin handle qStr1) []
      eitherResp
        `shouldBe` 
          (Right $ ResponseInfo status201 [jsonHeader,("Location","http://localhost:3000/users/14")] (encode $ TokenResponse $ pack ("14.hij." ++ strSha1 "hijlilu")))

