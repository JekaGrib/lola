{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Comment where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateComment (..),UpdateComment (..),GetComments(..))
import Api.Response (CommentResponse (..),CommentIdTextUserResponse(..),CommentsResponse(..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Comment
import Network.HTTP.Types (status200, status201, status204)
import Spec.Auth.Types
import Spec.Exist.Types
import Spec.Comment.Handlers
import Spec.Comment.QStrExample
import Spec.Comment.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Methods.Common.Auth (AccessMode(..))
import Psql.ToQuery.SelectLimit (OrderBy (..))
import Types

testComm :: IO ()
testComm = hspec $ do
  describe "createComment" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createComment handle 3 (CreateComment 7 "cool")) []
      reverse state
        `shouldBe` [CommMock (InsertReturnComm "cool" 7 3)]
      eitherResp <- evalStateT (runExceptT $ createComment handle 3 (CreateComment 7 "cool")) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/comments/14")] "Status 201 Created")
  describe "getComment" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getComment handle 4) []
      reverse state
        `shouldBe` 
        [CommMock (SelectComm 4)]
      eitherResp <- evalStateT (runExceptT $ getComment handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ CommentResponse 4 "cool" 3 7))
  describe "getComments" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getComments handle (GetComments 7 1)) []
      reverse state
        `shouldBe` 
        [CommMock (SelectLimCommsForPost 7 (ByCommId DESC) 1 20)]
      eitherResp <- evalStateT (runExceptT $ getComments handle (GetComments 7 1)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ CommentsResponse 1 7 [CommentIdTextUserResponse 1 "cool" 3,CommentIdTextUserResponse 2 "ok" 4,CommentIdTextUserResponse 3 "yes" 5]))
  describe "updateComment" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ updateComment handle 3 2 (UpdateComment "yes")) []
      reverse state
        `shouldBe` 
        [ CommMock (SelectUsersForComm 2)
        , CommMock (UpdateDbComm "yes" 2)
        , CommMock (SelectPostsForComm 2)
        ]
  describe "deleteComment" $ do
    it "work with valid DB answer in UserMode" $ do
      state <- execStateT (runExceptT $ deleteComment handle 3 UserMode 7) []
      reverse state
        `shouldBe` 
        [ CommMock (SelectPostsForComm 7)
        , CommMock (SelectUsersForPost 7)
        , CommMock (SelectUsersForComm 7)
        , CommMock (DeleteDbComm 7)]
      eitherResp <- evalStateT (runExceptT $ deleteComment handle 3 UserMode 7) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")  
    it "work with valid DB answer in AdminMode" $ do
      state <- execStateT (runExceptT $ deleteComment handle 3 AdminMode 7) []
      reverse state
        `shouldBe` 
        [CommMock (DeleteDbComm 7)]
      eitherResp <- evalStateT (runExceptT $ deleteComment handle 3 UserMode 7) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")    
  describe "workWithComms (ToPost)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithComms handle qStr2 ToPost) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (PostId 7))
        , CommMock (InsertReturnComm "yes" 7 152)]
      eitherResp <- evalStateT (runExceptT $ workWithComms handle qStr2 ToPost) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/comments/14")] "Status 201 Created")
  describe "workWithComms (ToGet id)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithComms handle [] (ToGet 4)) []
      reverse state
        `shouldBe` 
        [ ExistMock (IsExist (CommentId 4))
        , CommMock (SelectComm 4)
        ]
      eitherResp <- evalStateT (runExceptT $ workWithComms handle [] (ToGet 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ CommentResponse 4 "cool" 3 7))
  describe "workWithComms (ToGetAll)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithComms handle qStr4 ToGetAll) []
      reverse state
        `shouldBe` 
        [ExistMock (IsExist (PostId 7)),CommMock (SelectLimCommsForPost 7 (ByCommId DESC) 1 20)]
      eitherResp <- evalStateT (runExceptT $ workWithComms handle qStr4 ToGetAll) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ CommentsResponse 1 7 [CommentIdTextUserResponse 1 "cool" 3,CommentIdTextUserResponse 2 "ok" 4,CommentIdTextUserResponse 3 "yes" 5]))
  describe "workWithComms (ToPut)" $ do
    it "work with valid DB answer, without super category" $ do
      state <- execStateT (runExceptT $ workWithComms handle qStr3 (ToPut 4)) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (CommentId 4))
        , CommMock (SelectUsersForComm 4)
        ]
  describe "workWithComms (ToDelete)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithComms handle qStr1 (ToDelete 4)) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (CommentId 4))
        , CommMock (DeleteDbComm 4)
        ]
      eitherResp <- evalStateT (runExceptT $ workWithComms handle qStr1 (ToDelete 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")   