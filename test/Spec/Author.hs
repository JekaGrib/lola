{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Author where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateAuthor (..),UpdateAuthor (..))
import Api.Response (AuthorResponse (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Logger (Priority (..))
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Author
import Network.HTTP.Types (status200, status201, status204)
import Oops (ReqError (..))
import Spec.Auth.Types
import Spec.Exist.Types
import Spec.Author.Handlers
import Spec.Author.QStrExample
import Spec.Author.Types
import Spec.MakeCatResp.Types
import Spec.DeleteMany.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)

testAuthor :: IO ()
testAuthor = hspec $ do
  describe "createAuthor" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createAuthor handle (CreateAuthor 3 "author")) []
      reverse state
        `shouldBe` 
        [ AuthorMock (IsUserAuthor 3)
        , AuthorMock (InsertReturnAuthor 3 "author")
        ]
      eitherResp <- evalStateT (runExceptT $ createAuthor handle (CreateAuthor 3 "author")) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/authors/14")] "Status 201 Created")
  describe "getAuthor" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getAuthor handle 4) []
      reverse state
        `shouldBe` 
        [AuthorMock (SelectAuthors 4)]
      eitherResp <- evalStateT (runExceptT $ getAuthor handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] $ encode $ 
          AuthorResponse 4 "author" 3)
  describe "updateAuthor" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ updateAuthor handle 4 (UpdateAuthor 3 "author")) []
      reverse state
        `shouldBe` 
        [ AuthorMock (SelectAuthorsForUser 3)
        , AuthorMock (UpdateDbAuthor 3 "author" 4)
        ]
  describe "deleteAuthor" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ deleteAuthor handle 4) []
      reverse state
        `shouldBe` 
        [ AuthorMock (SelectDraftsForAuthor 4)
        , TRANSACTIONOPEN
        , AuthorMock (UpdateDbAuthorForPosts 1 4)
        , DeleteManyMock (DeleteDbPicsForDrafts [2,5])
        , DeleteManyMock (DeleteDbTagsForDrafts [2,5])
        , DeleteManyMock (DeleteDbDrafts [2,5])
        , AuthorMock (DeleteDbAuthor 4)
        , TRANSACTIONCLOSE
        ]
      eitherResp <- evalStateT (runExceptT $ deleteAuthor handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")    
{-}  describe "workWithCats (ToPost)" $ do
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
  describe "workWithCats (ToGet)" $ do
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
  describe "workWithCats (ToDelete)" $ do
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
-}
