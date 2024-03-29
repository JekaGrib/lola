module Spec.Comment where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr
  ( CreateComment (..),
    GetComments (..),
    UpdateComment (..),
  )
import Api.Response
  ( CommentIdTextUserResponse (..),
    CommentResponse (..),
    CommentsResponse (..),
    Created (..),
  )
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Error (ReqError (..))
import Methods.Comment
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Auth (AccessMode (..))
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Network.HTTP.Types (status200, status201, status204)
import Psql.ToQuery.SelectLimit (OrderBy (..))
import Spec.Auth.Types
import Spec.Comment.Handlers
import Spec.Comment.QStrExample
import Spec.Comment.Types
import Spec.Exist.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types

testComm :: IO ()
testComm = hspec $ do
  describe "createComment"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ createComment handle 3 (CreateComment 7 "cool")) []
      reverse state
        `shouldBe` [CommMock (InsertReturnComm "cool" 7 3)]
      eitherResp <-
        evalStateT (runExceptT $ createComment handle 3 (CreateComment 7 "cool")) []
      eitherResp
        `shouldBe` ( Right
                       $ ResponseInfo
                         status201
                         [jsonHeader, ("Location", "http://localhost:3000/comments/14")]
                       $ encode
                       $ Created 14 "comment"
                   )
  describe "getComment"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ getComment handle 4) []
      reverse state
        `shouldBe` [CommMock (SelectComm 4)]
      eitherResp <- evalStateT (runExceptT $ getComment handle 4) []
      eitherResp
        `shouldBe` ( Right $
                       ResponseInfo
                         status200
                         [jsonHeader]
                         (encode $ CommentResponse 4 "cool" 3 7)
                   )
  describe "getComments"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ getComments handle (GetComments 7 1)) []
      reverse state
        `shouldBe` [CommMock (SelectLimCommsForPost 7 (ByCommentId DESC) 1 20)]
      eitherResp <- evalStateT (runExceptT $ getComments handle (GetComments 7 1)) []
      eitherResp
        `shouldBe` ( Right $
                       ResponseInfo
                         status200
                         [jsonHeader]
                         ( encode $
                             CommentsResponse
                               1
                               7
                               [ CommentIdTextUserResponse 1 "cool" 3,
                                 CommentIdTextUserResponse 2 "ok" 4,
                                 CommentIdTextUserResponse 3 "yes" 5
                               ]
                         )
                   )
  describe "updateComment" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ updateComment handle 3 2 (UpdateComment "yes")) []
      reverse state
        `shouldBe` [ CommMock (SelectUsersForComm 2),
                     CommMock (UpdateDbComm "yes" 2),
                     CommMock (SelectPostsForComm 2)
                   ]
    it "throw Forbidden Error if user not comment author" $ do
      eitherResp <-
        evalStateT (runExceptT $ updateComment handle 25 2 (UpdateComment "yes")) []
      eitherResp
        `shouldBe` Left (ForbiddenError "user_id: 25 is not author of comment_id: 2")
  describe "deleteComment" $ do
    it "work with valid DB answer in UserMode" $ do
      state <- execStateT (runExceptT $ deleteComment handle 3 UserMode 7) []
      reverse state
        `shouldBe` [ CommMock (SelectPostsForComm 7),
                     CommMock (SelectUsersForPost 7),
                     CommMock (SelectUsersForComm 7),
                     CommMock (DeleteDbComm 7)
                   ]
      eitherResp <- evalStateT (runExceptT $ deleteComment handle 3 UserMode 7) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
    it "throw Forbidden Error in UserMode if user not author of comment or post" $ do
      eitherResp <- evalStateT (runExceptT $ deleteComment handle 25 UserMode 7) []
      eitherResp
        `shouldBe` ( Left $
                       ForbiddenError
                         "user_id: 25 is not author of comment_id: 7\
                         \ and not author of post_id: 7"
                   )
    it "work with valid DB answer in AdminMode" $ do
      state <- execStateT (runExceptT $ deleteComment handle 3 AdminMode 7) []
      reverse state
        `shouldBe` [CommMock (DeleteDbComm 7)]
      eitherResp <- evalStateT (runExceptT $ deleteComment handle 3 UserMode 7) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
  describe "workWithComments (ToPost)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithComments handle qStr2 ToPost) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 152),
                     ExistMock (IsExist (PostId 7)),
                     CommMock (InsertReturnComm "yes" 7 152)
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithComments handle qStr2 ToPost) []
      eitherResp
        `shouldBe` ( Right
                       $ ResponseInfo
                         status201
                         [jsonHeader, ("Location", "http://localhost:3000/comments/14")]
                       $ encode
                       $ Created 14 "comment"
                   )
  describe "workWithComments (ToGet id)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithComments handle [] (ToGet 4)) []
      reverse state
        `shouldBe` [ ExistMock (IsExist (CommentId 4)),
                     CommMock (SelectComm 4)
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithComments handle [] (ToGet 4)) []
      eitherResp
        `shouldBe` ( Right $
                       ResponseInfo
                         status200
                         [jsonHeader]
                         (encode $ CommentResponse 4 "cool" 3 7)
                   )
  describe "workWithComments (ToGetAll)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithComments handle qStr4 ToGetAll) []
      reverse state
        `shouldBe` [ ExistMock (IsExist (PostId 7)),
                     CommMock (SelectLimCommsForPost 7 (ByCommentId DESC) 1 20)
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithComments handle qStr4 ToGetAll) []
      eitherResp
        `shouldBe` ( Right $
                       ResponseInfo
                         status200
                         [jsonHeader]
                         ( encode $
                             CommentsResponse
                               1
                               7
                               [ CommentIdTextUserResponse 1 "cool" 3,
                                 CommentIdTextUserResponse 2 "ok" 4,
                                 CommentIdTextUserResponse 3 "yes" 5
                               ]
                         )
                   )
  describe "workWithComments (ToPut)"
    $ it "work with valid DB answer, without super category"
    $ do
      state <- execStateT (runExceptT $ workWithComments handle qStr3 (ToPut 4)) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 152),
                     ExistMock (IsExist (CommentId 4)),
                     CommMock (SelectUsersForComm 4)
                   ]
  describe "workWithComments (ToDelete)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithComments handle qStr1 (ToDelete 4)) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 152),
                     ExistMock (IsExist (CommentId 4)),
                     CommMock (DeleteDbComm 4)
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithComments handle qStr1 (ToDelete 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
