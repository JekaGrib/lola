module Spec.MakeCatResp where

import Api.Response (CatResponse (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Methods.Common.MakeCatResp
import Spec.MakeCatResp.Handlers
import Spec.MakeCatResp.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)

testMakeCatResp :: IO ()
testMakeCatResp = hspec
  $ describe "makeCatResp"
  $ do
    it "work with super cat" $ do
      state <- execStateT (runExceptT $ makeCatResp handle 1) []
      reverse state
        `shouldBe` [MakeCatRMock (SelectCats 1), MakeCatRMock (SelectSubCats 1)]
      eitherResp <- evalStateT (runExceptT $ makeCatResp handle 1) []
      eitherResp
        `shouldBe` (Right $ CatResponse 1 "a" [4, 5, 6])
    it "work with sub cat" $ do
      state <- execStateT (runExceptT $ makeCatResp handle 16) []
      reverse state
        `shouldBe` [ MakeCatRMock (SelectCats 16),
                     MakeCatRMock (SelectSubCats 16),
                     MakeCatRMock (SelectCats 12),
                     MakeCatRMock (SelectSubCats 12),
                     MakeCatRMock (SelectCats 4),
                     MakeCatRMock (SelectSubCats 4),
                     MakeCatRMock (SelectCats 1),
                     MakeCatRMock (SelectSubCats 1)
                   ]
      eitherResp <- evalStateT (runExceptT $ makeCatResp handle 16) []
      eitherResp
        `shouldBe` (Right $ SubCatResponse 16 "p" [] $ SubCatResponse 12 "l" [16, 17] $ SubCatResponse 4 "d" [11, 12] $ CatResponse 1 "a" [4, 5, 6])
