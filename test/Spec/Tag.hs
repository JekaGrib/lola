module Spec.Tag where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateTag (..), UpdateTag (..))
import Api.Response (TagResponse (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Logger (Priority (..))
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Tag
import Network.HTTP.Types (status200, status201, status204)
import Error (ReqError (..))
import Spec.Auth.Types
import Spec.Exist.Types
import Spec.Tag.Handlers
import Spec.Tag.QStrExample
import Spec.Tag.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)

testTag :: IO ()
testTag = hspec $ do
  describe "createTag" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createTag handle (CreateTag "cats")) []
      reverse state
        `shouldBe` [LOG DEBUG, TagMock (InsertReturnTag "cats"), LOG INFO, LOG INFO]
      eitherResp <- evalStateT (runExceptT $ createTag handle (CreateTag "cats")) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/tags/14")] "Status 201 Created")
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ createTag handle3 (CreateTag "cats")) []
      reverse state
        `shouldBe` [LOG DEBUG]
      eitherResp <- evalStateT (runExceptT $ createTag handle3 (CreateTag "cats")) []
      eitherResp
        `shouldBe` (Left $ DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}")
    it "throw DBError to UnexpectedDbOutPutException" $ do
      state <- execStateT (runExceptT $ createTag handle5 (CreateTag "cats")) []
      reverse state
        `shouldBe` [LOG DEBUG]
      eitherResp <- evalStateT (runExceptT $ createTag handle5 (CreateTag "cats")) []
      eitherResp
        `shouldBe` (Left $ DatabaseError "UnexpectedEmptyDbOutPutException")
  describe "getTag" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getTag handle 3) []
      reverse state
        `shouldBe` [LOG DEBUG, TagMock (SelectTagNames 3), LOG INFO, LOG INFO]
      eitherResp <- evalStateT (runExceptT $ getTag handle 3) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ TagResponse 3 "cats"))
    it "throw DBError with multiple DB answer" $ do
      state <- execStateT (runExceptT $ getTag handle1 3) []
      reverse state
        `shouldBe` [LOG DEBUG, TagMock (SelectTagNames 3), LOG INFO]
      eitherResp <- evalStateT (runExceptT $ getTag handle1 3) []
      eitherResp
        `shouldBe` Left (DatabaseError "Output not single[\"cats\",\"food\"]")
    it "throw DBError with empty DB answer" $ do
      state <- execStateT (runExceptT $ getTag handle2 3) []
      reverse state
        `shouldBe` [LOG DEBUG, TagMock (SelectTagNames 3), LOG INFO]
      eitherResp <- evalStateT (runExceptT $ getTag handle2 3) []
      eitherResp
        `shouldBe` Left (DatabaseError "Empty output")
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ getTag handle3 7) []
      reverse state
        `shouldBe` [LOG DEBUG]
      eitherResp <- evalStateT (runExceptT $ getTag handle3 7) []
      eitherResp
        `shouldBe` (Left $ DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}")
  describe "updateTag" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ updateTag handle 7 (UpdateTag "food")) []
      reverse state
        `shouldBe` [LOG DEBUG, TagMock (UpdateDbTag "food" 7), LOG INFO, LOG INFO]
      eitherResp <- evalStateT (runExceptT $ updateTag handle 7 (UpdateTag "food")) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ TagResponse 7 "food"))
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ updateTag handle3 7 (UpdateTag "food")) []
      reverse state
        `shouldBe` [LOG DEBUG]
      eitherResp <- evalStateT (runExceptT $ updateTag handle3 7 (UpdateTag "food")) []
      eitherResp
        `shouldBe` (Left $ DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}")
  describe "deleteTag" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ deleteTag handle 7) []
      reverse state
        `shouldBe` [LOG DEBUG, TRANSACTIONOPEN, TagMock (DeleteDbTagForDrafts 7), TagMock (DeleteDbTagForPosts 7), TagMock (DeleteDbTag 7), TRANSACTIONCLOSE, LOG INFO, LOG INFO]
      eitherResp <- evalStateT (runExceptT $ deleteTag handle 7) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ deleteTag handle3 7) []
      reverse state
        `shouldBe` [LOG DEBUG]
      eitherResp <- evalStateT (runExceptT $ deleteTag handle3 7) []
      eitherResp
        `shouldBe` (Left $ DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}")
    it "throw DBError to fatal Sql error" $ do
      state <- execStateT (runExceptT $ deleteTag handle4 7) []
      reverse state
        `shouldBe` [LOG DEBUG]
      eitherResp <- evalStateT (runExceptT $ deleteTag handle4 7) []
      eitherResp
        `shouldBe` (Left $ DatabaseError "SqlError {sqlState = \"oops\", sqlExecStatus = FatalError, sqlErrorMsg = \"oops\", sqlErrorDetail = \"oops\", sqlErrorHint = \"oops\"}")
  describe "workWithTags (ToPost)" $ do
    it "work with valid token" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr2 ToPost) []
      reverse state
        `shouldBe` [LOG INFO, LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO, LOG DEBUG, TagMock (InsertReturnTag "dogs"), LOG INFO, LOG INFO]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr2 ToPost) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/tags/14")] "Status 201 Created")
    it "throw BadReq Error on wrong QString" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr1 ToPost) []
      reverse state
        `shouldBe` [LOG INFO, LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr1 ToPost) []
      eitherResp
        `shouldBe` (Left $ BadReqError "Can't find parameter:tag_name")
  describe "workWithTags (ToGet)" $ do
    it "work with exist tag" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr2 (ToGet 4)) []
      reverse state
        `shouldBe` [LOG INFO, ExistMock (IsExist (TagId 4)), LOG DEBUG, TagMock (SelectTagNames 4), LOG INFO, LOG INFO]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr2 (ToGet 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ TagResponse 4 "cats"))
    it "throw 404 Error on not exist tag" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr1 (ToGet 200)) []
      reverse state
        `shouldBe` [LOG INFO, ExistMock (IsExist (TagId 200))]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr1 (ToGet 200)) []
      eitherResp
        `shouldBe` (Left $ ResourseNotExistError "tag_id: 200 doesn`t exist")
  describe "workWithTags (ToPut )" $ do
    it "work with exist tag" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr2 (ToPut 4)) []
      reverse state
        `shouldBe` [LOG INFO, LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO, ExistMock (IsExist (TagId 4)), LOG DEBUG, TagMock (UpdateDbTag "dogs" 4), LOG INFO, LOG INFO]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr2 (ToPut 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ TagResponse 4 "dogs"))
    it "throw 404 Error on not exist tag" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr1 (ToPut 200)) []
      reverse state
        `shouldBe` [LOG INFO, LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO, ExistMock (IsExist (TagId 200))]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr1 (ToPut 200)) []
      eitherResp
        `shouldBe` (Left $ ResourseNotExistError "tag_id: 200 doesn`t exist")
    it "throw BadReq Error on wrong QString" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr1 (ToPut 4)) []
      reverse state
        `shouldBe` [LOG INFO, LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO, ExistMock (IsExist (TagId 4))]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr1 (ToPut 4)) []
      eitherResp
        `shouldBe` (Left $ BadReqError "Can't find parameter:tag_name")
  describe "workWithTags (ToDelete )" $ do
    it "work with exist tag" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr2 (ToDelete 4)) []
      reverse state
        `shouldBe` [LOG INFO, LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO, ExistMock (IsExist (TagId 4)), LOG DEBUG, TRANSACTIONOPEN, TagMock (DeleteDbTagForDrafts 4), TagMock (DeleteDbTagForPosts 4), TagMock (DeleteDbTag 4), TRANSACTIONCLOSE, LOG INFO, LOG INFO]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr2 (ToDelete 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
    it "throw 404 Error on not exist tag" $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr1 (ToDelete 200)) []
      reverse state
        `shouldBe` [LOG INFO, LOG INFO, LOG DEBUG, AuthMock (SelectTokenKeyForUser 152), LOG INFO, LOG INFO, ExistMock (IsExist (TagId 200))]
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr1 (ToDelete 200)) []
      eitherResp
        `shouldBe` (Left $ ResourseNotExistError "tag_id: 200 doesn`t exist")
  describe "workWithTags (ToPostId)"
    $ it "throw 404 Error "
    $ do
      state <- execStateT (runExceptT $ workWithTags handle qStr2 (ToPostId 4)) []
      reverse state
        `shouldBe` []
      eitherResp <- evalStateT (runExceptT $ workWithTags handle qStr2 (ToPostId 4)) []
      eitherResp
        `shouldBe` Left (ResourseNotExistError "Wrong method for tags resourse: ToPostId 4")
