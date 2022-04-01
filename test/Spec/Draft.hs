{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.Draft where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.JSON (DraftRequest (..))
import Api.Request.QueryStr (GetDrafts(..))
import Api.Response (DraftResponse(..),PostIdOrNull(..),AuthorResponse(..),CatResponse(..),TagResponse(..),PicIdUrl(..),DraftsResponse(..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Logger (Priority (..))
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Draft
import Network.HTTP.Types (status200, status201, status204)
import Oops (ReqError (..))
import Spec.Auth.Types
import Spec.Exist.Types
import Spec.Draft.Handlers
import Spec.Draft.QStrExample
import Spec.Draft.Types
import Spec.MakeCatResp.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Methods.Common.Auth (AccessMode(..))
import Psql.ToQuery.SelectLimit (OrderBy (..))
import Types
import Data.Text (pack)

testDraft :: IO ()
testDraft = hspec $ do
  describe "createNewDraft" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createNewDraft handle 4 (DraftRequest "ok" 12 "lala" 3 [5,7,24] [2,8,41])) []
      reverse state
        `shouldBe` 
        [ DraftMock (SelectAuthorsForUser 4)
        , TRANSACTIONOPEN
        , DraftMock (InsertReturnDraft (InsertDraft Nothing 7 "ok" 12 "lala" 3))
        , DraftMock (InsertManyDraftsPics [(14,5),(14,7),(14,24)])
        , DraftMock (InsertManyDraftsTags [(14,2),(14,8),(14,41)])
        , TRANSACTIONCLOSE
        ]
      eitherResp <- evalStateT (runExceptT $ createNewDraft handle 4 (DraftRequest "ok" 12 "lala" 3 [5,7,24] [2,8,41])) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/drafts/14")] "Status 201 Created")
  describe "getDraft" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getDraft handle 3 4) []
      reverse state
        `shouldBe` 
        [ DraftMock (SelectAuthorsForUser 3)
        , DraftMock (SelectUsersForDraft 4)
        , DraftMock (SelectDrafts 4)
        , DraftMock (SelectPicsForDraft 4)
        , DraftMock (SelectTagsForDraft 4)
        , MakeCatRMock (SelectCats 9)
        , MakeCatRMock (SelectSubCats 9)
        , MakeCatRMock (SelectCats 3)
        , MakeCatRMock (SelectSubCats 3)
        ]
      eitherResp <- evalStateT (runExceptT $ getDraft handle 3 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] $ encode $ draftResp0)
  describe "getDrafts" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ getDrafts handle 4 (GetDrafts 1)) []
      reverse state
        `shouldBe` 
        [DraftMock (SelectAuthorsForUser 4)
        ,DraftMock (SelectLimDraftsForAuthor 7 (ByDraftId DESC) 1 5)
        ,DraftMock (SelectPicsForDraft 1)
        ,DraftMock (SelectTagsForDraft 1)
        ,MakeCatRMock (SelectCats 15)
        ,MakeCatRMock (SelectSubCats 15)
        ,MakeCatRMock (SelectCats 9)
        ,MakeCatRMock (SelectSubCats 9)
        ,MakeCatRMock (SelectCats 3)
        ,MakeCatRMock (SelectSubCats 3)
        ,DraftMock (SelectPicsForDraft 5)
        ,DraftMock (SelectTagsForDraft 5)
        ,MakeCatRMock (SelectCats 24)
        ,MakeCatRMock (SelectSubCats 24)
        ,MakeCatRMock (SelectCats 20)
        ,MakeCatRMock (SelectSubCats 20)
        ,MakeCatRMock (SelectCats 15)
        ,MakeCatRMock (SelectSubCats 15)
        ,MakeCatRMock (SelectCats 9)
        ,MakeCatRMock (SelectSubCats 9)
        ,MakeCatRMock (SelectCats 3)
        ,MakeCatRMock (SelectSubCats 3)
        ,DraftMock (SelectPicsForDraft 12)
        ,DraftMock (SelectTagsForDraft 12)
        ,MakeCatRMock (SelectCats 17)
        ,MakeCatRMock (SelectSubCats 17)
        ,MakeCatRMock (SelectCats 12)
        ,MakeCatRMock (SelectSubCats 12)
        ,MakeCatRMock (SelectCats 4)
        ,MakeCatRMock (SelectSubCats 4)
        ,MakeCatRMock (SelectCats 1)
        ,MakeCatRMock (SelectSubCats 1)
        ]
      eitherResp <- evalStateT (runExceptT $ getDrafts handle 4 (GetDrafts 1)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] $ encode $ 
          DraftsResponse 1 [draftResp1,draftResp2,draftResp3])
{-}  describe "updateComment" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ updateComment handle 3 2 (UpdateComment "yes")) []
      reverse state
        `shouldBe` 
        [ CommMock (SelectUsersForComm 2)
        , CommMock (UpdateDbComm "yes" 2)
        , CommMock (SelectPostsForComm 2)
        ]
  describe "deleteComment" $ do
    it "work with valid DB answer in UserMode" $ do
      state <- execStateT (runExceptT $ deleteComment handle 3 UserMode 7) []
      reverse state
        `shouldBe` 
        [ CommMock (SelectPostsForComm 7)
        , CommMock (SelectUsersForPost 7)
        , CommMock (SelectUsersForComm 7)
        , CommMock (DeleteDbComm 7)]
      eitherResp <- evalStateT (runExceptT $ deleteComment handle 3 UserMode 7) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")  
    it "work with valid DB answer in AdminMode" $ do
      state <- execStateT (runExceptT $ deleteComment handle 3 AdminMode 7) []
      reverse state
        `shouldBe` 
        [CommMock (DeleteDbComm 7)]
      eitherResp <- evalStateT (runExceptT $ deleteComment handle 3 UserMode 7) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")    
  describe "workWithComms (ToPost)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithComms handle qStr2 ToPost) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (PostId 7))
        , CommMock (InsertReturnComm "yes" 7 152)]
      eitherResp <- evalStateT (runExceptT $ workWithComms handle qStr2 ToPost) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/comments/14")] "Status 201 Created")
  describe "workWithComms (ToGet id)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithComms handle [] (ToGet 4)) []
      reverse state
        `shouldBe` 
        [ ExistMock (IsExist (CommentId 4))
        , CommMock (SelectComm 4)
        ]
      eitherResp <- evalStateT (runExceptT $ workWithComms handle [] (ToGet 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ CommentResponse 4 "cool" 3 7))
  describe "workWithComms (ToGetAll)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithComms handle qStr4 ToGetAll) []
      reverse state
        `shouldBe` 
        [ExistMock (IsExist (PostId 7)),CommMock (SelectLimCommsForPost 7 (ByCommId DESC) 1 20)]
      eitherResp <- evalStateT (runExceptT $ workWithComms handle qStr4 ToGetAll) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] (encode $ CommentsResponse 1 7 [CommentIdTextUserResponse 1 "cool" 3,CommentIdTextUserResponse 2 "ok" 4,CommentIdTextUserResponse 3 "yes" 5]))
  describe "workWithComms (ToPut)" $ do
    it "work with valid DB answer, without super category" $ do
      state <- execStateT (runExceptT $ workWithComms handle qStr3 (ToPut 4)) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (CommentId 4))
        , CommMock (SelectUsersForComm 4)
        ]
  describe "workWithComms (ToDelete)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithComms handle qStr1 (ToDelete 4)) []
      reverse state
        `shouldBe` 
        [ AuthMock (SelectTokenKeyForUser 152)
        , ExistMock (IsExist (CommentId 4))
        , CommMock (DeleteDbComm 4)
        ]
      eitherResp <- evalStateT (runExceptT $ workWithComms handle qStr1 (ToDelete 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")   
-}

draftResp0 :: DraftResponse
draftResp0 = 
  let catResp = SubCatResponse 9 "i" [15] $ CatResponse 3 "c" [9,10]
      toPicUrl iD = pack $ "http://localhost:3000/pictures/" ++ show iD
      picsResps = [PicIdUrl 6 (toPicUrl 6),PicIdUrl 9 (toPicUrl 9),PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats",TagResponse 18 "dogs",TagResponse 20 "birds"]
  in  DraftResponse 4 (PostIdExist 7) (AuthorResponse 7 "author" 3) "draft" catResp "lalala" 6 (toPicUrl 6) picsResps tagsResps 

draftResp1 :: DraftResponse
draftResp1 = 
  let catResp = SubCatResponse 15 "o" [19,20] $ SubCatResponse 9 "i" [15] $ CatResponse 3 "c" [9,10]
      toPicUrl iD = pack $ "http://localhost:3000/pictures/" ++ show iD
      picsResps = [PicIdUrl 6 (toPicUrl 6),PicIdUrl 9 (toPicUrl 9),PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats",TagResponse 18 "dogs",TagResponse 20 "birds"]
  in  DraftResponse 1 (PostIdExist 7) (AuthorResponse 7 "author" 4) "draft" catResp "lalala" 6 (toPicUrl 6) picsResps tagsResps 

draftResp2 :: DraftResponse
draftResp2 = 
  let catResp = SubCatResponse 24 "u" [] $ SubCatResponse 20 "t" [] $ SubCatResponse 15 "o" [19,20] $ SubCatResponse 9 "i" [15] $ CatResponse 3 "c" [9,10]
      toPicUrl iD = pack $ "http://localhost:3000/pictures/" ++ show iD
      picsResps = [PicIdUrl 6 (toPicUrl 6),PicIdUrl 9 (toPicUrl 9),PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats",TagResponse 18 "dogs",TagResponse 20 "birds"]
  in  DraftResponse 5 PostIdNull (AuthorResponse 7 "author" 4) "draft5" catResp "lalala" 4 (toPicUrl 4) picsResps tagsResps 

draftResp3 :: DraftResponse
draftResp3 = 
  let catResp = SubCatResponse 17 "q" [] $ SubCatResponse 12 "l" [16,17] $ SubCatResponse 4 "d" [11,12] $ CatResponse 1 "a" [4,5,6]
      toPicUrl iD = pack $ "http://localhost:3000/pictures/" ++ show iD
      picsResps = [PicIdUrl 6 (toPicUrl 6),PicIdUrl 9 (toPicUrl 9),PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats",TagResponse 18 "dogs",TagResponse 20 "birds"]
  in  DraftResponse 12 PostIdNull (AuthorResponse 7 "author" 4) "draft12" catResp "lalala" 13 (toPicUrl 13) picsResps tagsResps 