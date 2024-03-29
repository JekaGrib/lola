module Spec.Author where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateAuthor (..), UpdateAuthor (..))
import Api.Response (AuthorResponse (..), Created (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Error (ReqError (..))
import Methods.Author
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Network.HTTP.Types (status200, status201, status204)
import Spec.Auth.Types
import Spec.Author.Handlers
import Spec.Author.QStrExample
import Spec.Author.Types
import Spec.DeleteMany.Types
import Spec.Exist.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)

testAuthor :: IO ()
testAuthor = hspec $ do
  describe "createAuthor" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createAuthor handle (CreateAuthor 3 "author")) []
      reverse state
        `shouldBe` [ AuthorMock (IsUserAuthor 3),
                     AuthorMock (InsertReturnAuthor 3 "author")
                   ]
      eitherResp <- evalStateT (runExceptT $ createAuthor handle (CreateAuthor 3 "author")) []
      eitherResp
        `shouldBe` ( Right
                       $ ResponseInfo
                         status201
                         [jsonHeader, ("Location", "http://localhost:3000/authors/14")]
                       $ encode
                       $ Created 14 "author"
                   )
    it "throw Bad Request Error if user already author" $ do
      eitherResp <- evalStateT (runExceptT $ createAuthor handle (CreateAuthor 25 "author")) []
      eitherResp
        `shouldBe` Left (BadReqError "User is already author.")
  describe "getAuthor"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ getAuthor handle 4) []
      reverse state
        `shouldBe` [AuthorMock (SelectAuthors 4)]
      eitherResp <- evalStateT (runExceptT $ getAuthor handle 4) []
      eitherResp
        `shouldBe` ( Right $ ResponseInfo status200 [jsonHeader] $ encode $
                       AuthorResponse 4 "author" 3
                   )
  describe "updateAuthor" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ updateAuthor handle 4 (UpdateAuthor 3 "author")) []
      reverse state
        `shouldBe` [ AuthorMock (SelectAuthorsForUser 3),
                     AuthorMock (UpdateDbAuthor 3 "author" 4)
                   ]
    it "throw Bad Request Error if user already other author" $ do
      eitherResp <-
        evalStateT (runExceptT $ updateAuthor handle 4 (UpdateAuthor 25 "author")) []
      eitherResp
        `shouldBe` Left (BadReqError "User_id: 25 is already other author")
  describe "deleteAuthor"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ deleteAuthor handle 4) []
      reverse state
        `shouldBe` [ AuthorMock (SelectDraftsForAuthor 4),
                     TRANSACTIONOPEN,
                     AuthorMock (UpdateDbAuthorForPosts 1 4),
                     DeleteManyMock (DeleteDbPicsForDrafts [2, 5]),
                     DeleteManyMock (DeleteDbTagsForDrafts [2, 5]),
                     DeleteManyMock (DeleteDbDrafts [2, 5]),
                     AuthorMock (DeleteDbAuthor 4),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ deleteAuthor handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
  describe "workWithAuthors (ToPost)"
    $ it "work with valid DB answer, without super category"
    $ do
      state <- execStateT (runExceptT $ workWithAuthors handle qStr2 ToPost) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 152),
                     ExistMock (IsExist (UserId 3)),
                     AuthorMock (IsUserAuthor 3),
                     AuthorMock (InsertReturnAuthor 3 "author")
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithAuthors handle qStr2 ToPost) []
      eitherResp
        `shouldBe` ( Right
                       $ ResponseInfo
                         status201
                         [jsonHeader, ("Location", "http://localhost:3000/authors/14")]
                       $ encode
                       $ Created 14 "author"
                   )
  describe "workWithAuthors (ToGet)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithAuthors handle qStr1 (ToGet 4)) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 152),
                     ExistMock (IsExist (AuthorId 4)),
                     AuthorMock (SelectAuthors 4)
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithAuthors handle qStr1 (ToGet 4)) []
      eitherResp
        `shouldBe` ( Right $ ResponseInfo status200 [jsonHeader] $ encode $
                       AuthorResponse 4 "author" 3
                   )
  describe "workWithAuthors (ToPut)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithAuthors handle qStr2 (ToPut 4)) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 152),
                     ExistMock (IsExist (AuthorId 4)),
                     ExistMock (IsExist (UserId 3)),
                     AuthorMock (SelectAuthorsForUser 3),
                     AuthorMock (UpdateDbAuthor 3 "author" 4)
                   ]
  describe "workWithAuthors (ToDelete)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithAuthors handle qStr1 (ToDelete 4)) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 152),
                     ExistMock (IsExist (AuthorId 4)),
                     AuthorMock (SelectDraftsForAuthor 4),
                     TRANSACTIONOPEN,
                     AuthorMock (UpdateDbAuthorForPosts 1 4),
                     DeleteManyMock (DeleteDbPicsForDrafts [2, 5]),
                     DeleteManyMock (DeleteDbTagsForDrafts [2, 5]),
                     DeleteManyMock (DeleteDbDrafts [2, 5]),
                     AuthorMock (DeleteDbAuthor 4),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithAuthors handle qStr1 (ToDelete 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
