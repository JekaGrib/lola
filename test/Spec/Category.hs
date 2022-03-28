{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Category where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateCategory (..))
import Api.Response (TagResponse (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Logger (Priority (..))
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Category
import Network.HTTP.Types (status200, status201, status204)
import Oops (ReqError (..))
import Spec.Auth.Types
import Spec.Exist.Types
import Spec.Category.Handlers
import Spec.Category.QStrExample
import Spec.Category.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)

testCat :: IO ()
testCat = hspec $ do
  describe "createCategory" $ do
    it "work with valid DB answer, without super category" $ do
      state <- execStateT (runExceptT $ createCategory handle (CreateCategory "food" Nothing)) []
      reverse state
        `shouldBe` [CatMock (InsertReturnCat "food")]
      eitherResp <- evalStateT (runExceptT $ createCategory handle (CreateCategory "food" Nothing)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/categories/14")] "Status 201 Created")
    it "work with valid DB answer, with super category" $ do
      state <- execStateT (runExceptT $ createCategory handle (CreateCategory "food" (Just 4))) []
      reverse state
        `shouldBe` [CatMock (InsertReturnSubCat "food" 4)]
      eitherResp <- evalStateT (runExceptT $ createCategory handle (CreateCategory "food" (Just 4))) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/categories/14")] "Status 201 Created")
    