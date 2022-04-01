{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.MakeCatResp where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateTag (..), UpdateTag (..))
import Api.Response (TagResponse (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Logger (Priority (..))
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Common.MakeCatResp
import Network.HTTP.Types (status200, status201, status204)
import Oops (ReqError (..))
import Spec.Auth.Types
import Spec.Exist.Types
import Spec.MakeCatResp.Handlers
import Spec.MakeCatResp.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Api.Response (CatResponse(..))

testMakeCatResp :: IO ()
testMakeCatResp = hspec $ do
  describe "makeCatResp" $ do
    it "work with super cat" $ do
      state <- execStateT (runExceptT $ makeCatResp handle 1) []
      reverse state
        `shouldBe` [MakeCatRMock (SelectCats 1),MakeCatRMock (SelectSubCats 1)]
      eitherResp <- evalStateT (runExceptT $ makeCatResp handle 1) []
      eitherResp
        `shouldBe` (Right $ CatResponse 1 "a" [4,5,6])
    it "work with sub cat" $ do
      state <- execStateT (runExceptT $ makeCatResp handle 16) []
      reverse state
        `shouldBe` 
        [ MakeCatRMock (SelectCats    16)
        , MakeCatRMock (SelectSubCats 16)
        , MakeCatRMock (SelectCats    12)
        , MakeCatRMock (SelectSubCats 12)
        , MakeCatRMock (SelectCats    4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats    1)
        , MakeCatRMock (SelectSubCats 1)
        ]
      eitherResp <- evalStateT (runExceptT $ makeCatResp handle 16) []
      eitherResp
        `shouldBe` (Right $ SubCatResponse 16 "p" [] $ SubCatResponse 12 "l" [16,17] $  SubCatResponse 4 "d" [11,12] $ CatResponse 1 "a" [4,5,6])

