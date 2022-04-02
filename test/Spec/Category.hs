{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Category where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateCategory (..),UpdateCategory (..))
import Api.Response (CatResponse (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Category
import Network.HTTP.Types (status200, status201, status204)
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
  describe "getCategory" $ 
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
        [ CatMock      (UpdateDbCat "food" 4)
        , MakeCatRMock (SelectCats    4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats    1)
        , MakeCatRMock (SelectSubCats 1)
        ]
    it "work with valid DB answer, with super category" $ do
      state <- execStateT (runExceptT $ updateCategory handle 4 (UpdateCategory "food" (Just 2))) []
      reverse state
        `shouldBe` 
        [ MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectSubCats 11)
        , MakeCatRMock (SelectSubCats 12)
        , MakeCatRMock (SelectSubCats 16)
        , MakeCatRMock (SelectSubCats 17)
        , CatMock      (UpdateDbSubCat "food" 2 4)
        , MakeCatRMock (SelectCats    4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats    1)
        , MakeCatRMock (SelectSubCats 1)
        ]
  describe "deleteCategory" $ 
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ deleteCategory handle 4) []
      reverse state
        `shouldBe` 
        [ MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectSubCats 11)
        , MakeCatRMock (SelectSubCats 12)
        , MakeCatRMock (SelectSubCats 16)
        , MakeCatRMock (SelectSubCats 17)
        , TRANSACTIONOPEN
        , CatMock (UpdateDbCatsForPosts  1 [4,11,12,16,17])
        , CatMock (UpdateDbCatsForDrafts 1 [4,11,12,16,17])
        , CatMock (DeleteDbCats [4,11,12,16,17])
        , TRANSACTIONCLOSE
        ]
      eitherResp <- evalStateT (runExceptT $ deleteCategory handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")    
  describe "workWithCats (ToPost)" $ do
    it "work with valid DB answer, without super category" $ do
      state <- execStateT (runExceptT $ workWithCats handle qStr2 ToPost) []
      reverse state
        `shouldBe` [AuthMock (SelectTokenKeyForUser 152),CatMock (InsertReturnCat "dogs")]
      eitherResp <- evalStateT (runExceptT $ workWithCats handle qStr2 ToPost) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/categories/14")] "Status 201 Created")
    it "work with valid DB answer, with super category" $ do
      state <- execStateT (runExceptT $ workWithCats handle qStr3 ToPost) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (CategoryId 2))
        , CatMock (InsertReturnSubCat "dogs" 2)
        ]
      eitherResp <- evalStateT (runExceptT $ workWithCats handle qStr3 ToPost) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/categories/14")] "Status 201 Created")
  describe "workWithCats (ToGet)" $ 
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithCats handle [] (ToGet 4)) []
      reverse state
        `shouldBe` 
        [ ExistMock (IsExist (CategoryId 4))
        , MakeCatRMock (SelectCats    4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats    1)
        , MakeCatRMock (SelectSubCats 1)
        ]
      eitherResp <- evalStateT (runExceptT $ workWithCats handle [] (ToGet 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ SubCatResponse 4 "d" [11,12] $ CatResponse 1 "a" [4,5,6]))
  describe "workWithCats (ToPut)" $ do
    it "work with valid DB answer, without super category" $ do
      state <- execStateT (runExceptT $ workWithCats handle qStr2 (ToPut 4)) []
      reverse state
        `shouldBe` 
        [ AuthMock     (SelectTokenKeyForUser 152)
        , ExistMock    (IsExist (CategoryId 4))
        , CatMock      (UpdateDbCat "dogs" 4)
        , MakeCatRMock (SelectCats    4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats    1)
        , MakeCatRMock (SelectSubCats 1)
        ]
    it "work with valid DB answer, with super category" $ do
      state <- execStateT (runExceptT $ workWithCats handle qStr3 (ToPut 4)) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (CategoryId 4))
        , ExistMock (IsExist (CategoryId 2))
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectSubCats 11)
        , MakeCatRMock (SelectSubCats 12)
        , MakeCatRMock (SelectSubCats 16)
        , MakeCatRMock (SelectSubCats 17)
        , CatMock      (UpdateDbSubCat "dogs" 2 4)
        , MakeCatRMock (SelectCats    4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats    1)
        , MakeCatRMock (SelectSubCats 1)
        ]
  describe "workWithCats (ToDelete)" $ 
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithCats handle qStr1 (ToDelete 4)) []
      reverse state
        `shouldBe` 
        [ AuthMock     (SelectTokenKeyForUser 152)
        , ExistMock    (IsExist (CategoryId 4))
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectSubCats 11)
        , MakeCatRMock (SelectSubCats 12)
        , MakeCatRMock (SelectSubCats 16)
        , MakeCatRMock (SelectSubCats 17)
        , TRANSACTIONOPEN
        , CatMock (UpdateDbCatsForPosts  1 [4,11,12,16,17])
        , CatMock (UpdateDbCatsForDrafts 1 [4,11,12,16,17])
        , CatMock (DeleteDbCats [4,11,12,16,17])
        , TRANSACTIONCLOSE
        ]
      eitherResp <- evalStateT (runExceptT $ workWithCats handle qStr1 (ToDelete 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")   