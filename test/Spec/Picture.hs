{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Picture where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (LoadPicture(..))
import Api.Response (TagResponse (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Logger (Priority (..))
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Picture
import Network.HTTP.Types (status200, status201, status204)
import Oops (ReqError (..))
import Spec.Auth.Types
import Spec.Exist.Types
import Spec.Picture.Handlers
import Spec.Picture.QStrExample
import Spec.Picture.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)

testPic :: IO ()
testPic = hspec $ do
  describe "sendPicture" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ sendPicture handle 4) []
      reverse state
        `shouldBe` [PicMock (SelectPicBS 4)]
      eitherResp <- evalStateT (runExceptT $ sendPicture handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [("Content-Type","image/jpeg")] "picture")
    it "throw DBError to multiple DB answer" $ do
      state <- execStateT (runExceptT $ sendPicture handle1 4) []
      reverse state
        `shouldBe` [PicMock (SelectPicBS 4)]
      eitherResp <- evalStateT (runExceptT $ sendPicture handle1 4) []
      eitherResp
        `shouldBe` (Left (DatabaseError "Output not single[\"picture\",\"picture\"]"))
    it "throw DBError to empty DB answer" $ do
      state <- execStateT (runExceptT $ sendPicture handle2 4) []
      reverse state
        `shouldBe` [PicMock (SelectPicBS 4)]
      eitherResp <- evalStateT (runExceptT $ sendPicture handle2 4) []
      eitherResp
        `shouldBe` (Left (DatabaseError "Empty output"))
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ sendPicture handle3 4) []
      reverse state
        `shouldBe` []
      eitherResp <- evalStateT (runExceptT $ sendPicture handle3 4) []
      eitherResp
        `shouldBe` (Left $ DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}")
  describe "loadPicture" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ loadPicture handle (LoadPicture "url")) []
      reverse state
        `shouldBe` [PicMock (GoToUrl "url"),PicMock (InsertReturnPicBS sbsPicExample)]
      eitherResp <- evalStateT (runExceptT $ loadPicture handle (LoadPicture "url")) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader,("Location","http://localhost:3000/pictures/14")] "Status 201 Created")
    it "throw BadReq Error to invalid picture in url" $ do
      state <- execStateT (runExceptT $ loadPicture handle4 (LoadPicture "url")) []
      reverse state
        `shouldBe` [PicMock (GoToUrl "url")]
      eitherResp <- evalStateT (runExceptT $ loadPicture handle4 (LoadPicture "url")) []
      eitherResp
        `shouldBe` (Left (BadReqError "Invalid picture url:url"))
    it "throw BadReq Error to invalid url" $ do
      state <- execStateT (runExceptT $ loadPicture handle3 (LoadPicture "url")) []
      reverse state
        `shouldBe` []
      eitherResp <- evalStateT (runExceptT $ loadPicture handle3 (LoadPicture "url")) []
      eitherResp
        `shouldBe` (Left (BadReqError "Invalid picture url:url. InvalidUrlException \"oops\" \"oops\""))
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ loadPicture handle5 (LoadPicture "url")) []
      reverse state
        `shouldBe` [PicMock (GoToUrl "url")]
      eitherResp <- evalStateT (runExceptT $ loadPicture handle5 (LoadPicture "url")) []
      eitherResp
        `shouldBe` (Left $ DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}")
    it "throw DBError to UnexpectedDbOutPutException" $ do 
      state <- execStateT (runExceptT $ loadPicture handle6 (LoadPicture "url")) []
      reverse state
        `shouldBe` [PicMock (GoToUrl "url")]
      eitherResp <- evalStateT (runExceptT $ loadPicture handle6 (LoadPicture "url")) []
      eitherResp
        `shouldBe` (Left $ DatabaseError "UnexpectedEmptyDbOutPutException")
  describe "workWithPics (ToPost)" $ do
    it "work with valid token" $ do
      state <- execStateT (runExceptT $ workWithPics handle qStr2 ToPost) []
      reverse state
        `shouldBe` [AuthMock (SelectTokenKeyForUser 152),PicMock (GoToUrl "url"),PicMock (InsertReturnPicBS sbsPicExample)]
      eitherResp <- evalStateT (runExceptT $ workWithPics handle qStr2 ToPost) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/pictures/14")] "Status 201 Created")
    it "throw BadReq Error on wrong QString" $ do
      state <- execStateT (runExceptT $ workWithPics handle qStr1 ToPost) []
      reverse state
        `shouldBe` [AuthMock (SelectTokenKeyForUser 152)]
      eitherResp <- evalStateT (runExceptT $ workWithPics handle qStr1 ToPost) []
      eitherResp
        `shouldBe` (Left $ BadReqError "Can't find parameter:pic_url")
  describe "workWithPics (ToGet)" $ do
    it "work with exist tag" $ do
      state <- execStateT (runExceptT $ workWithPics handle qStr2 (ToGet 4)) []
      reverse state
        `shouldBe` [ExistMock (IsExist (PictureId 4)),PicMock (SelectPicBS 4)]
      eitherResp <- evalStateT (runExceptT $ workWithPics handle qStr2 (ToGet 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [("Content-Type","image/jpeg")] "picture")
    it "throw 404 Error on not exist pic" $ do
      state <- execStateT (runExceptT $ workWithPics handle qStr1 (ToGet 200)) []
      reverse state
        `shouldBe` [ExistMock (IsExist (PictureId 200))]
      eitherResp <- evalStateT (runExceptT $ workWithPics handle qStr1 (ToGet 200)) []
      eitherResp
        `shouldBe` (Left $ ResourseNotExistError "pic_id: 200 doesn`t exist")