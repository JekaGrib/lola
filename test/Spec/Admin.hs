module Spec.Admin where

import Api.Request.QueryStr (CreateUser (..))
import Api.Response (CreatedUser (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Data.Text (pack)
import Methods.Admin
import Methods.Common (ResponseInfo (..), jsonHeader, strSha1)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Network.HTTP.Types (status201)
import Spec.Admin.Handlers
import Spec.Admin.QStrExample
import Spec.Admin.Types
import Spec.Exist.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types

testAdmin :: IO ()
testAdmin = hspec $ do
  describe "createAdmin"
    $ it "work with valid DB answer"
    $ do
      state <-
        execStateT (runExceptT $ createAdmin handle (CreateUser "pwd" "fName" "lName" 6)) []
      let hashPWD = "37fa265330ad83eaa879efb1e2db6380896cf639"
      reverse state
        `shouldBe` [ AdminMock GetDay,
                     AdminMock GenerateTokenKey,
                     AdminMock
                       $ InsertReturnUser
                       $ InsertUser hashPWD "fName" "lName" 6 dayExample True "lilu"
                   ]
      eitherResp <-
        evalStateT (runExceptT $ createAdmin handle (CreateUser "pwd" "fName" "lName" 6)) []
      eitherResp
        `shouldBe` ( Right $
                       ResponseInfo
                         status201
                         [jsonHeader, ("Location", "http://localhost:3000/users/14")]
                         (encode $ CreatedUser 14 $ pack ("14.hij." ++ strSha1 "hijlilu"))
                   )
  describe "workWithAdmin "
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithAdmin handle qStr1) []
      let hashPWD = "37fa265330ad83eaa879efb1e2db6380896cf639"
      reverse state
        `shouldBe` [ AdminMock SelectKeys,
                     ExistMock (IsExist (PictureId 6)),
                     AdminMock GetDay,
                     AdminMock GenerateTokenKey,
                     AdminMock
                       $ InsertReturnUser
                       $ InsertUser hashPWD "fName" "lName" 6 dayExample True "lilu"
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithAdmin handle qStr1) []
      eitherResp
        `shouldBe` ( Right $
                       ResponseInfo
                         status201
                         [jsonHeader, ("Location", "http://localhost:3000/users/14")]
                         (encode $ CreatedUser 14 $ pack ("14.hij." ++ strSha1 "hijlilu"))
                   )
