{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Post where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateAuthor (..),UpdateAuthor (..))
import Api.Response (PostResponse(..),PostIdOrNull(..),AuthorResponse(..),CatResponse(..),TagResponse(..),PicIdUrl(..),PostsResponse(..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Logger (Priority (..))
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Post
import Network.HTTP.Types (status200, status201, status204)
import Oops (ReqError (..))
import Spec.Auth.Types
import Spec.Exist.Types
import Spec.Post.Handlers
import Spec.Post.QStrExample
import Spec.Post.Types
import Spec.MakeCatResp.Types
import Spec.Draft.Types
import Spec.DeleteMany.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types
import Data.Text (pack)

testPost :: IO ()
testPost = hspec $ do
  describe "createPostsDraft" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createPostsDraft handle 3 7) []
      reverse state
        `shouldBe` 
        [ DraftMock (SelectAuthorsForUser 3)
        , PostMock (SelectPostInfos 7)
        , PostMock (SelectUsersForPost 7)
        , PostMock (SelectPicsForPost 7)
        , PostMock (SelectTagsForPost 7)
        , TRANSACTIONOPEN
        , DraftMock (InsertReturnDraft (InsertDraft (Just 7) 7 "post" 4 "lalala" 8))
        , DraftMock (InsertManyDraftsPics [(14,6),(14,9),(14,12)])
        , DraftMock (InsertManyDraftsTags [(14,15),(14,18),(14,20)])
        , TRANSACTIONCLOSE
        ]
      eitherResp <- evalStateT (runExceptT $ createPostsDraft handle 3 7) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/drafts/14")] "Status 201 Created")
  describe "getPost" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getPost handle 4) []
      reverse state
        `shouldBe` 
        [ PostMock (SelectPosts 4)
        , PostMock (SelectPicsForPost 4)
        , PostMock (SelectTagsForPost 4)
        , MakeCatRMock (SelectCats 4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats 1)
        , MakeCatRMock (SelectSubCats 1)
        ]
      eitherResp <- evalStateT (runExceptT $ getPost handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] $ encode $ postResp0)
{-}  describe "getPosts" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getPosts handle (GetPosts 1 ) ) []
      reverse state
        `shouldBe` 
        [ PostMock (SelectPosts 4)
        , PostMock (SelectPicsForPost 4)
        , PostMock (SelectTagsForPost 4)
        , MakeCatRMock (SelectCats 4)
        , MakeCatRMock (SelectSubCats 4)
        , MakeCatRMock (SelectCats 1)
        , MakeCatRMock (SelectSubCats 1)
        ]
      eitherResp <- evalStateT (runExceptT $ getPosts handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] $ encode $ postResp0)
  describe "updateAuthor" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ updateAuthor handle 4 (UpdateAuthor 3 "author")) []
      reverse state
        `shouldBe` 
        [ AuthorMock (SelectAuthorsForUser 3)
        , AuthorMock (UpdateDbAuthor 3 "author" 4)
        ]
  describe "deleteAuthor" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ deleteAuthor handle 4) []
      reverse state
        `shouldBe` 
        [ AuthorMock (SelectDraftsForAuthor 4)
        , TRANSACTIONOPEN
        , AuthorMock (UpdateDbAuthorForPosts 1 4)
        , DeleteManyMock (DeleteDbPicsForDrafts [2,5])
        , DeleteManyMock (DeleteDbTagsForDrafts [2,5])
        , DeleteManyMock (DeleteDbDrafts [2,5])
        , AuthorMock (DeleteDbAuthor 4)
        , TRANSACTIONCLOSE
        ]
      eitherResp <- evalStateT (runExceptT $ deleteAuthor handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")    
  describe "workWithAuthors (ToPost)" $ do
    it "work with valid DB answer, without super category" $ do
      state <- execStateT (runExceptT $ workWithAuthors handle qStr2 ToPost) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (UserId 3))
        , AuthorMock (IsUserAuthor 3)
        , AuthorMock (InsertReturnAuthor 3 "author")
        ]
      eitherResp <- evalStateT (runExceptT $ workWithAuthors handle qStr2 ToPost) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/authors/14")] "Status 201 Created")
  describe "workWithAuthors (ToGet)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithAuthors handle qStr1 (ToGet 4)) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (AuthorId 4))
        , AuthorMock (SelectAuthors 4)
        ]
      eitherResp <- evalStateT (runExceptT $ workWithAuthors handle qStr1 (ToGet 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] $ encode $ 
          AuthorResponse 4 "author" 3)
  describe "workWithAuthors (ToPut)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithAuthors handle qStr2 (ToPut 4)) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (AuthorId 4))
        , ExistMock (IsExist (UserId 3))
        , AuthorMock (SelectAuthorsForUser 3)
        , AuthorMock (UpdateDbAuthor 3 "author" 4)
        ]
  describe "workWithAuthors (ToDelete)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithAuthors handle qStr1 (ToDelete 4)) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (AuthorId 4))
        , AuthorMock (SelectDraftsForAuthor 4)
        , TRANSACTIONOPEN
        , AuthorMock (UpdateDbAuthorForPosts 1 4)
        , DeleteManyMock (DeleteDbPicsForDrafts [2,5])
        , DeleteManyMock (DeleteDbTagsForDrafts [2,5])
        , DeleteManyMock (DeleteDbDrafts [2,5])
        , AuthorMock (DeleteDbAuthor 4)
        , TRANSACTIONCLOSE
        ]
      eitherResp <- evalStateT (runExceptT $ workWithAuthors handle qStr1 (ToDelete 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")   

-}

postResp0 :: PostResponse
postResp0 = 
  let catResp = SubCatResponse 4 "d" [11,12] $ CatResponse 1 "a" [4,5,6]
      toPicUrl iD = pack $ "http://localhost:3000/pictures/" ++ show iD
      picsResps = [PicIdUrl 6 (toPicUrl 6),PicIdUrl 9 (toPicUrl 9),PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats",TagResponse 18 "dogs",TagResponse 20 "birds"]
  in  PostResponse 4 (AuthorResponse 7 "author" 3) "post" dayExample catResp "lalala" 8 (toPicUrl 8) picsResps tagsResps 

postResp1 :: PostResponse
postResp1 = 
  let catResp = SubCatResponse 4 "d" [11,12] $ CatResponse 1 "a" [4,5,6]
      toPicUrl iD = pack $ "http://localhost:3000/pictures/" ++ show iD
      picsResps = [PicIdUrl 6 (toPicUrl 6),PicIdUrl 9 (toPicUrl 9),PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats",TagResponse 18 "dogs",TagResponse 20 "birds"]
  in  PostResponse 1 (AuthorResponse 7 "author" 3) "post1" dayExample catResp "lalala" 8 (toPicUrl 8) picsResps tagsResps 

postResp2 :: PostResponse
postResp2 = 
  let catResp = CatResponse 2 "b" [7]
      toPicUrl iD = pack $ "http://localhost:3000/pictures/" ++ show iD
      picsResps = [PicIdUrl 6 (toPicUrl 6),PicIdUrl 9 (toPicUrl 9),PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats",TagResponse 18 "dogs",TagResponse 20 "birds"]
  in  PostResponse 2 (AuthorResponse 8 "author" 4) "post2" dayExample catResp "lalala" 7 (toPicUrl 7) picsResps tagsResps 

postResp3 :: PostResponse
postResp3 = 
  let catResp = CatResponse 1 "a" [4,5,6]
      toPicUrl iD = pack $ "http://localhost:3000/pictures/" ++ show iD
      picsResps = [PicIdUrl 6 (toPicUrl 6),PicIdUrl 9 (toPicUrl 9),PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats",TagResponse 18 "dogs",TagResponse 20 "birds"]
  in  PostResponse 3 (AuthorResponse 9 "author" 5) "post3" dayExample catResp "lalala" 6 (toPicUrl 6) picsResps tagsResps   