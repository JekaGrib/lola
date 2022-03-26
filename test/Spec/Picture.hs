{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Picture where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateTag (..), UpdateTag (..))
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
  