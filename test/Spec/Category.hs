{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Category where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateCategory (..),UpdateCategory (..))
import Api.Response (CatResponse (..))
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
import Spec.MakeCatResp.Types
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
  describe "getCategory" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getCategory handle 4) []
      reverse state
        `shouldBe` 
        [ MakeCatRMock (SelectCats    4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats    1)
        , MakeCatRMock (SelectSubCats 1)
        ]
      eitherResp <- evalStateT (runExceptT $ getCategory handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ SubCatResponse 4 "d" [11,12] $ CatResponse 1 "a" [4,5,6]))
  describe "updateCategory" $ do
    it "work with valid DB answer, without super category" $ do
      state <- execStateT (runExceptT $ updateCategory handle 4 (UpdateCategory "food" Nothing)) []
      reverse state
        `shouldBe` 
        [ CatMock (UpdateDbCat "food" 4)
        , MakeCatRMock (SelectCats    4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats    1)
        , MakeCatRMock (SelectSubCats 1)]
      eitherResp <- evalStateT (runExceptT $ updateCategory handle 4 (UpdateCategory "food" Nothing)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ SubCatResponse 4 "d" [11,12] $ CatResponse 1 "a" [4,5,6]))
    it "work with valid DB answer, with super category" $ do
      state <- execStateT (runExceptT $ updateCategory handle 4 (UpdateCategory "food" (Just 2))) []
      reverse state
        `shouldBe` 
        [ MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectSubCats 11)
        , MakeCatRMock (SelectSubCats 12)
        , MakeCatRMock (SelectSubCats 16)
        , MakeCatRMock (SelectSubCats 17)
        , CatMock (UpdateDbSubCat "food" 2 4)
        , MakeCatRMock (SelectCats    4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats    1)
        , MakeCatRMock (SelectSubCats 1)
        ]
      eitherResp <- evalStateT (runExceptT $ updateCategory handle 4 (UpdateCategory "food" (Just 2))) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ SubCatResponse 4 "d" [11,12] $ CatResponse 1 "a" [4,5,6]))