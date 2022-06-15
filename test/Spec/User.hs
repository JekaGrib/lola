module Spec.User where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateUser (..), LogIn (..))
import Api.Response (TokenResponse (..), UserResponse (..), CreatedUser (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Data.Text (pack)
import Methods.Common (ResponseInfo (..), jsonHeader, strSha1, textHeader, txtSha1)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.User
import Network.HTTP.Types (status200, status201, status204)
import Error (ReqError (..))
import Spec.Auth.Types
import Spec.DeleteMany.Types
import Spec.Exist.Types
import Spec.Types (MockAction (..))
import Spec.User.Handlers
import Spec.User.QStrExample
import Spec.User.Types
import Test.Hspec (describe, hspec, it, shouldBe)
import Types

testUser :: IO ()
testUser = hspec $ do
  describe "logIn" $ do
    it "work for user" $ do
      state <- execStateT (runExceptT $ logIn handle (LogIn 4 "pwd")) []
      reverse state
        `shouldBe` [UserMock (SelectAuthsForUser 4), UserMock GenerateTokenKey, UserMock (UpdateDbTokenKeyForUser "lilu" 4)]
      eitherResp <- evalStateT (runExceptT $ logIn handle (LogIn 4 "pwd")) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ TokenResponse $ pack ("4.stu." ++ strSha1 "stulilu")))
    it "work for admin" $ do
      state <- execStateT (runExceptT $ logIn handle1 (LogIn 4 "pwd")) []
      reverse state
        `shouldBe` [UserMock (SelectAuthsForUser 4), UserMock GenerateTokenKey, UserMock (UpdateDbTokenKeyForUser "lilu" 4)]
      eitherResp <- evalStateT (runExceptT $ logIn handle1 (LogIn 4 "pwd")) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ TokenResponse $ pack ("4.hij." ++ strSha1 "hijlilu")))
    it "throw LogIn Error to wrong password" $ do
      state <- execStateT (runExceptT $ logIn handle (LogIn 4 "oops")) []
      reverse state
        `shouldBe` [UserMock (SelectAuthsForUser 4)]
      eitherResp <- evalStateT (runExceptT $ logIn handle (LogIn 4 "oops")) []
      eitherResp
        `shouldBe` Left (SecretLogInError "INVALID password")
  describe "workWithLogIn" $ do
    it "work for valid query string" $ do
      state <- execStateT (runExceptT $ workWithLogIn handle qStr3) []
      reverse state
        `shouldBe` [ExistMock (IsExist (UserId 4)), UserMock (SelectAuthsForUser 4), UserMock GenerateTokenKey, UserMock (UpdateDbTokenKeyForUser "lilu" 4)]
      eitherResp <- evalStateT (runExceptT $ workWithLogIn handle qStr3) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ TokenResponse $ pack ("4.stu." ++ strSha1 "stulilu")))
    it "throw BadReq Error on not exist user" $ do
      state <- execStateT (runExceptT $ workWithLogIn handle qStr4) []
      reverse state
        `shouldBe` [ExistMock (IsExist (UserId 200))]
      eitherResp <- evalStateT (runExceptT $ workWithLogIn handle qStr4) []
      eitherResp
        `shouldBe` Left (SecretLogInError "BadReqError \"user_id: 200 doesn`t exist\"")
    it "throw BadReq Error on not valid query string" $ do
      state <- execStateT (runExceptT $ workWithLogIn handle qStr1) []
      reverse state
        `shouldBe` []
      eitherResp <- evalStateT (runExceptT $ workWithLogIn handle qStr1) []
      eitherResp
        `shouldBe` Left (SecretLogInError "BadReqError \"Can't find parameter:user_id\"")
  describe "createUser"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ createUser handle (CreateUser "pwd" "fName" "lName" 4)) []
      reverse state
        `shouldBe` [UserMock GetDay, UserMock GenerateTokenKey, UserMock (InsertReturnUser (InsertUser (txtSha1 "pwd") "fName" "lName" 4 dayExample False "lilu"))]
      eitherResp <- evalStateT (runExceptT $ createUser handle (CreateUser "pwd" "fName" "lName" 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [jsonHeader, ("Location", "http://localhost:3000/users/14")] (encode $ CreatedUser 14 $ pack ("14.stu." ++ strSha1 "stulilu")))
  describe "getUser"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ getUser handle 4) []
      reverse state
        `shouldBe` [UserMock (SelectUsers 4)]
      eitherResp <- evalStateT (runExceptT $ getUser handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ UserResponse 4 "fName" "lName" 4 "http://localhost:3000/pictures/4" dayExample))
  describe "deleteUser"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ deleteUser handle 4) []
      reverse state
        `shouldBe` [ UserMock (SelectAuthorsForUser 4),
                     UserMock (SelectDraftsForAuthor 4),
                     TRANSACTIONOPEN,
                     UserMock (UpdateDbUserForComms 1 4),
                     UserMock (UpdateDbAuthorForPosts 1 4),
                     DeleteManyMock (DeleteDbPicsForDrafts [2, 5]),
                     DeleteManyMock (DeleteDbTagsForDrafts [2, 5]),
                     DeleteManyMock (DeleteDbDrafts [2, 5]),
                     UserMock (DeleteDbAuthor 4),
                     UserMock (DeleteDbUser 4),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ deleteUser handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
  describe "workWithUsers (ToPost)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithUsers handle qStr2 ToPost) []
      reverse state
        `shouldBe` [ExistMock (IsExist (PictureId 4)), UserMock GetDay, UserMock GenerateTokenKey, UserMock (InsertReturnUser (InsertUser (txtSha1 "pwd") "fName" "lName" 4 dayExample False "lilu"))]
      eitherResp <- evalStateT (runExceptT $ workWithUsers handle qStr2 ToPost) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [jsonHeader, ("Location", "http://localhost:3000/users/14")] (encode $ CreatedUser 14 $ pack ("14.stu." ++ strSha1 "stulilu")))
    it "throw BadReq Error to not exist picture" $ do
      state <- execStateT (runExceptT $ workWithUsers handle qStr5 ToPost) []
      reverse state
        `shouldBe` [ExistMock (IsExist (PictureId 200))]
      eitherResp <- evalStateT (runExceptT $ workWithUsers handle qStr5 ToPost) []
      eitherResp
        `shouldBe` Left (BadReqError "pic_id: 200 doesn`t exist")
  describe "workWithUsers (ToGet)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithUsers handle [] (ToGet 4)) []
      reverse state
        `shouldBe` [ExistMock (IsExist (UserId 4)), UserMock (SelectUsers 4)]
      eitherResp <- evalStateT (runExceptT $ workWithUsers handle [] (ToGet 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ UserResponse 4 "fName" "lName" 4 "http://localhost:3000/pictures/4" dayExample))
  describe "workWithUsers (ToDelete)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithUsers handle qStr1 (ToDelete 4)) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 152),
                     ExistMock (IsExist (UserId 4)),
                     UserMock (SelectAuthorsForUser 4),
                     UserMock (SelectDraftsForAuthor 4),
                     TRANSACTIONOPEN,
                     UserMock (UpdateDbUserForComms 1 4),
                     UserMock (UpdateDbAuthorForPosts 1 4),
                     DeleteManyMock (DeleteDbPicsForDrafts [2, 5]),
                     DeleteManyMock (DeleteDbTagsForDrafts [2, 5]),
                     DeleteManyMock (DeleteDbDrafts [2, 5]),
                     UserMock (DeleteDbAuthor 4),
                     UserMock (DeleteDbUser 4),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithUsers handle qStr1 (ToDelete 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
