{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Draft where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.JSON (DraftRequest (..))
import Api.Request.QueryStr (GetDrafts (..))
import Api.Response (AuthorResponse (..), CatResponse (..), DraftResponse (..), DraftsResponse (..), PicIdUrl (..), PostIdOrNull (..), TagResponse (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Data.Text (Text, pack)
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Draft
import Network.HTTP.Types (status200, status201, status204)
import Psql.ToQuery.SelectLimit (OrderBy (..))
import Spec.Auth.Types
import Spec.DeleteMany.Types
import Spec.Draft.Handlers
import Spec.Draft.JSONExample
import Spec.Draft.QStrExample
import Spec.Draft.Types
import Spec.Exist.Types
import Spec.MakeCatResp.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types
import Oops (ReqError(..))

testDraft :: IO ()
testDraft = hspec $ do
  describe "createNewDraft" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ createNewDraft handle 4 (DraftRequest "ok" 12 "lala" 3 [5, 7, 24] [2, 8, 41])) []
      reverse state
        `shouldBe` [ DraftMock (SelectAuthorsForUser 4),
                     TRANSACTIONOPEN,
                     DraftMock (InsertReturnDraft (InsertDraft Nothing 7 "ok" 12 "lala" 3)),
                     DraftMock (InsertManyDraftsPics [(14, 5), (14, 7), (14, 24)]),
                     DraftMock (InsertManyDraftsTags [(14, 2), (14, 8), (14, 41)]),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ createNewDraft handle 4 (DraftRequest "ok" 12 "lala" 3 [5, 7, 24] [2, 8, 41])) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/drafts/14")] "Status 201 Created")
    it "throw Forbidden Error if user not author" $ do
      eitherResp <- evalStateT (runExceptT $ createNewDraft handle 25 (DraftRequest "ok" 12 "lala" 3 [5, 7, 24] [2, 8, 41])) []
      eitherResp
        `shouldBe` Left (ForbiddenError "user_id: 25 isn`t author")
  describe "getDraft"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ getDraft handle 3 4) []
      reverse state
        `shouldBe` [ DraftMock (SelectAuthorsForUser 3),
                     DraftMock (SelectUsersForDraft 4),
                     DraftMock (SelectDrafts 4),
                     DraftMock (SelectPicsForDraft 4),
                     DraftMock (SelectTagsForDraft 4),
                     MakeCatRMock (SelectCats 9),
                     MakeCatRMock (SelectSubCats 9),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3)
                   ]
      eitherResp <- evalStateT (runExceptT $ getDraft handle 3 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] $ encode draftResp0)
  describe "getDrafts"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ getDrafts handle 4 (GetDrafts 1)) []
      reverse state
        `shouldBe` [ DraftMock (SelectAuthorsForUser 4),
                     DraftMock (SelectLimDraftsForAuthor 7 (ByDraftId DESC) 1 5),
                     DraftMock (SelectPicsForDraft 1),
                     DraftMock (SelectTagsForDraft 1),
                     MakeCatRMock (SelectCats 15),
                     MakeCatRMock (SelectSubCats 15),
                     MakeCatRMock (SelectCats 9),
                     MakeCatRMock (SelectSubCats 9),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3),
                     DraftMock (SelectPicsForDraft 5),
                     DraftMock (SelectTagsForDraft 5),
                     MakeCatRMock (SelectCats 24),
                     MakeCatRMock (SelectSubCats 24),
                     MakeCatRMock (SelectCats 20),
                     MakeCatRMock (SelectSubCats 20),
                     MakeCatRMock (SelectCats 15),
                     MakeCatRMock (SelectSubCats 15),
                     MakeCatRMock (SelectCats 9),
                     MakeCatRMock (SelectSubCats 9),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3),
                     DraftMock (SelectPicsForDraft 12),
                     DraftMock (SelectTagsForDraft 12),
                     MakeCatRMock (SelectCats 17),
                     MakeCatRMock (SelectSubCats 17),
                     MakeCatRMock (SelectCats 12),
                     MakeCatRMock (SelectSubCats 12),
                     MakeCatRMock (SelectCats 4),
                     MakeCatRMock (SelectSubCats 4),
                     MakeCatRMock (SelectCats 1),
                     MakeCatRMock (SelectSubCats 1)
                   ]
      eitherResp <- evalStateT (runExceptT $ getDrafts handle 4 (GetDrafts 1)) []
      eitherResp
        `shouldBe` ( Right $ ResponseInfo status200 [jsonHeader] $ encode $
                       DraftsResponse 1 [draftResp1, draftResp2, draftResp3]
                   )
  describe "updateDraft" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ updateDraft handle 3 14 (DraftRequest "ok" 12 "lala" 3 [5, 7, 24] [2, 8, 41])) []
      reverse state
        `shouldBe` [ DraftMock (SelectUsersForDraft 14),
                     DraftMock (SelectAuthorsForUser 3),
                     DraftMock (SelectTags [2, 8, 41]),
                     MakeCatRMock (SelectCats 12),
                     MakeCatRMock (SelectSubCats 12),
                     MakeCatRMock (SelectCats 4),
                     MakeCatRMock (SelectSubCats 4),
                     MakeCatRMock (SelectCats 1),
                     MakeCatRMock (SelectSubCats 1),
                     DraftMock (SelectPostsForDraft 14),
                     TRANSACTIONOPEN,
                     DeleteManyMock (DeleteDbPicsForDrafts [14]),
                     DeleteManyMock (DeleteDbTagsForDrafts [14]),
                     DraftMock (UpdateDraft 14 (UpdateDbDraft "ok" 12 "lala" 3)),
                     DraftMock (InsertManyDraftsPics [(14, 5), (14, 7), (14, 24)]),
                     DraftMock (InsertManyDraftsTags [(14, 2), (14, 8), (14, 41)]),
                     TRANSACTIONCLOSE
                   ]
    it "throw Forbidden Error if user not author of draft" $ do
      eitherResp <- evalStateT (runExceptT $ updateDraft handle 25 14 (DraftRequest "ok" 12 "lala" 3 [5, 7, 24] [2, 8, 41])) []
      eitherResp
        `shouldBe` Left (ForbiddenError "user_id: 25 is not author of draft_id: 14")  
  describe "deleteDraft" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ deleteDraft handle 3 7) []
      reverse state
        `shouldBe` [ DraftMock (SelectAuthorsForUser 3),
                     DraftMock (SelectUsersForDraft 7),
                     TRANSACTIONOPEN,
                     DeleteManyMock (DeleteDbPicsForDrafts [7]),
                     DeleteManyMock (DeleteDbTagsForDrafts [7]),
                     DeleteManyMock (DeleteDbDrafts [7]),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ deleteDraft handle 3 7) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
    it "throw Forbidden Error if user not author" $ do
      eitherResp <- evalStateT (runExceptT $ deleteDraft handle 25 7) []
      eitherResp
        `shouldBe` Left (ForbiddenError "user_id: 25 isn`t author")
  describe "publishDraft" $ do
    it "work with valid DB answer for update post" $ do
      state <- execStateT (runExceptT $ publishDraft handle 3 7) []
      reverse state
        `shouldBe` [ DraftMock (SelectAuthorsForUser 3),
                     DraftMock (SelectUsersForDraft 7),
                     DraftMock (SelectDrafts 7),
                     DraftMock (SelectPicsForDraft 7),
                     DraftMock (SelectTagsForDraft 7),
                     MakeCatRMock (SelectCats 9),
                     MakeCatRMock (SelectSubCats 9),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3),
                     DraftMock (SelectDaysForPost 7),
                     TRANSACTIONOPEN,
                     DraftMock (UpdatePost 7 (UpdateDbPost "draft" 9 "lalala" 6)),
                     DeleteManyMock (DeleteDbPicsForPost 7),
                     DeleteManyMock (DeleteDbTagsForPost 7),
                     DraftMock (InsertManyPostsPics [(7, 6), (7, 9), (7, 12)]),
                     DraftMock (InsertManyPostsTags [(7, 15), (7, 18), (7, 20)]),
                     TRANSACTIONCLOSE
                   ]
    it "work with valid DB answer for create new post" $ do
      state <- execStateT (runExceptT $ publishDraft handle 3 25) []
      reverse state
        `shouldBe` [ DraftMock (SelectAuthorsForUser 3),
                     DraftMock (SelectUsersForDraft 25),
                     DraftMock (SelectDrafts 25),
                     DraftMock (SelectPicsForDraft 25),
                     DraftMock (SelectTagsForDraft 25),
                     MakeCatRMock (SelectCats 9),
                     MakeCatRMock (SelectSubCats 9),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3),
                     GETDay,
                     TRANSACTIONOPEN,
                     DraftMock (InsertReturnPost (InsertPost 7 "draft" dayExample 9 "lalala" 6)),
                     DraftMock (InsertManyPostsPics [(20, 6), (20, 9), (20, 12)]),
                     DraftMock (InsertManyPostsTags [(20, 15), (20, 18), (20, 20)]),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ publishDraft handle 3 25) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/posts/20")] "Status 201 Created")
    it "throw Forbidden Error if user not author" $ do
      eitherResp <- evalStateT (runExceptT $ publishDraft handle 25 7) []
      eitherResp
        `shouldBe` Left (ForbiddenError "user_id: 25 isn`t author")
  describe "workWithDrafts (ToPost)" $ do
    it "work with valid DB answer" $ do
      state <- execStateT (runExceptT $ workWithDrafts handle qStr1 ToPost json1) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 3),
                     ExistMock (IsExist (CategoryId 3)),
                     ExistMock (IsExist (PictureId 42)),
                     ExistMock (IsExist (PictureId 6)),
                     ExistMock (IsExist (PictureId 9)),
                     ExistMock (IsExist (PictureId 12)),
                     ExistMock (IsExist (TagId 15)),
                     ExistMock (IsExist (TagId 18)),
                     ExistMock (IsExist (TagId 20)),
                     DraftMock (SelectAuthorsForUser 3),
                     TRANSACTIONOPEN,
                     DraftMock (InsertReturnDraft (InsertDraft Nothing 7 "rock" 3 "heyhey" 42)),
                     DraftMock (InsertManyDraftsPics [(14, 6), (14, 9), (14, 12)]),
                     DraftMock (InsertManyDraftsTags [(14, 15), (14, 18), (14, 20)]),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithDrafts handle qStr1 ToPost json1) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/drafts/14")] "Status 201 Created")
    it "throw Bad Request Error on wrong request body(id not a number)" $ do    
      eitherResp <- evalStateT (runExceptT $ workWithDrafts handle qStr1 ToPost json2) []
      eitherResp
        `shouldBe` Left (BadReqError "Can`t parse parameter: draft_category_id. It should be number")
    it "throw Bad Request Error on wrong request body(draft_text is a number)" $ do    
      eitherResp <- evalStateT (runExceptT $ workWithDrafts handle qStr1 ToPost json3) []
      eitherResp
        `shouldBe` Left (BadReqError "Can`t parse parameter: draft_text. It should be text")
    it "throw Bad Request Error on wrong request body(draft_tags_ids not a number list)" $ do    
      eitherResp <- evalStateT (runExceptT $ workWithDrafts handle qStr1 ToPost json4) []
      eitherResp
        `shouldBe` Left (BadReqError "Can`t parse parameter: draft_tags_ids. It should be number array. Example: [1,5,8]")
    it "throw Bad Request Error on wrong request body(draft_text missing)" $ do    
      eitherResp <- evalStateT (runExceptT $ workWithDrafts handle qStr1 ToPost json5) []
      eitherResp
        `shouldBe` Left (BadReqError "Can`t find parameter: draft_text")
  describe "workWithDrafts (ToPost id)" $ do
    it "work with valid DB answer for update post" $ do
      state <- execStateT (runExceptT $ workWithDrafts handle qStr1 (ToPostId 7) "") []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 3),
                     ExistMock (IsExist (DraftId 7)),
                     DraftMock (SelectAuthorsForUser 3),
                     DraftMock (SelectUsersForDraft 7),
                     DraftMock (SelectDrafts 7),
                     DraftMock (SelectPicsForDraft 7),
                     DraftMock (SelectTagsForDraft 7),
                     MakeCatRMock (SelectCats 9),
                     MakeCatRMock (SelectSubCats 9),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3),
                     DraftMock (SelectDaysForPost 7),
                     TRANSACTIONOPEN,
                     DraftMock (UpdatePost 7 (UpdateDbPost "draft" 9 "lalala" 6)),
                     DeleteManyMock (DeleteDbPicsForPost 7),
                     DeleteManyMock (DeleteDbTagsForPost 7),
                     DraftMock (InsertManyPostsPics [(7, 6), (7, 9), (7, 12)]),
                     DraftMock (InsertManyPostsTags [(7, 15), (7, 18), (7, 20)]),
                     TRANSACTIONCLOSE
                   ]
    it "work with valid DB answer for create new post" $ do
      state <- execStateT (runExceptT $ workWithDrafts handle qStr1 (ToPostId 25) "") []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 3),
                     ExistMock (IsExist (DraftId 25)),
                     DraftMock (SelectAuthorsForUser 3),
                     DraftMock (SelectUsersForDraft 25),
                     DraftMock (SelectDrafts 25),
                     DraftMock (SelectPicsForDraft 25),
                     DraftMock (SelectTagsForDraft 25),
                     MakeCatRMock (SelectCats 9),
                     MakeCatRMock (SelectSubCats 9),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3),
                     GETDay,
                     TRANSACTIONOPEN,
                     DraftMock (InsertReturnPost (InsertPost 7 "draft" dayExample 9 "lalala" 6)),
                     DraftMock (InsertManyPostsPics [(20, 6), (20, 9), (20, 12)]),
                     DraftMock (InsertManyPostsTags [(20, 15), (20, 18), (20, 20)]),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithDrafts handle qStr1 (ToPostId 25) json1) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status201 [textHeader, ("Location", "http://localhost:3000/posts/20")] "Status 201 Created")
  describe "workWithDrafts (ToGet)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithDrafts handle qStr1 (ToGet 4) "") []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 3),
                     ExistMock (IsExist (DraftId 4)),
                     DraftMock (SelectAuthorsForUser 3),
                     DraftMock (SelectUsersForDraft 4),
                     DraftMock (SelectDrafts 4),
                     DraftMock (SelectPicsForDraft 4),
                     DraftMock (SelectTagsForDraft 4),
                     MakeCatRMock (SelectCats 9),
                     MakeCatRMock (SelectSubCats 9),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3)
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithDrafts handle qStr1 (ToGet 4) "") []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] $ encode draftResp0)
  describe "workWithDrafts (ToGetAll)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithDrafts handle qStr2 ToGetAll "") []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 4),
                     DraftMock (SelectAuthorsForUser 4),
                     DraftMock (SelectLimDraftsForAuthor 7 (ByDraftId DESC) 1 5),
                     DraftMock (SelectPicsForDraft 1),
                     DraftMock (SelectTagsForDraft 1),
                     MakeCatRMock (SelectCats 15),
                     MakeCatRMock (SelectSubCats 15),
                     MakeCatRMock (SelectCats 9),
                     MakeCatRMock (SelectSubCats 9),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3),
                     DraftMock (SelectPicsForDraft 5),
                     DraftMock (SelectTagsForDraft 5),
                     MakeCatRMock (SelectCats 24),
                     MakeCatRMock (SelectSubCats 24),
                     MakeCatRMock (SelectCats 20),
                     MakeCatRMock (SelectSubCats 20),
                     MakeCatRMock (SelectCats 15),
                     MakeCatRMock (SelectSubCats 15),
                     MakeCatRMock (SelectCats 9),
                     MakeCatRMock (SelectSubCats 9),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3),
                     DraftMock (SelectPicsForDraft 12),
                     DraftMock (SelectTagsForDraft 12),
                     MakeCatRMock (SelectCats 17),
                     MakeCatRMock (SelectSubCats 17),
                     MakeCatRMock (SelectCats 12),
                     MakeCatRMock (SelectSubCats 12),
                     MakeCatRMock (SelectCats 4),
                     MakeCatRMock (SelectSubCats 4),
                     MakeCatRMock (SelectCats 1),
                     MakeCatRMock (SelectSubCats 1)
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithDrafts handle qStr2 ToGetAll "") []
      eitherResp
        `shouldBe` ( Right $ ResponseInfo status200 [jsonHeader] $ encode $
                       DraftsResponse 1 [draftResp1, draftResp2, draftResp3]
                   )
  describe "workWithDrafts (ToPut)"
    $ it "work with valid DB answer, without super category"
    $ do
      state <- execStateT (runExceptT $ workWithDrafts handle qStr1 (ToPut 4) json1) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 3),
                     ExistMock (IsExist (DraftId 4)),
                     ExistMock (IsExist (CategoryId 3)),
                     ExistMock (IsExist (PictureId 42)),
                     ExistMock (IsExist (PictureId 6)),
                     ExistMock (IsExist (PictureId 9)),
                     ExistMock (IsExist (PictureId 12)),
                     ExistMock (IsExist (TagId 15)),
                     ExistMock (IsExist (TagId 18)),
                     ExistMock (IsExist (TagId 20)),
                     DraftMock (SelectUsersForDraft 4),
                     DraftMock (SelectAuthorsForUser 3),
                     DraftMock (SelectTags [15, 18, 20]),
                     MakeCatRMock (SelectCats 3),
                     MakeCatRMock (SelectSubCats 3),
                     DraftMock (SelectPostsForDraft 4),
                     TRANSACTIONOPEN,
                     DeleteManyMock (DeleteDbPicsForDrafts [4]),
                     DeleteManyMock (DeleteDbTagsForDrafts [4]),
                     DraftMock (UpdateDraft 4 (UpdateDbDraft "rock" 3 "heyhey" 42)),
                     DraftMock (InsertManyDraftsPics [(4, 6), (4, 9), (4, 12)]),
                     DraftMock (InsertManyDraftsTags [(4, 15), (4, 18), (4, 20)]),
                     TRANSACTIONCLOSE
                   ]
  describe "workWithDrafts (ToDelete)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithDrafts handle qStr1 (ToDelete 4) "") []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 3),
                     ExistMock (IsExist (DraftId 4)),
                     DraftMock (SelectAuthorsForUser 3),
                     DraftMock (SelectUsersForDraft 4),
                     TRANSACTIONOPEN,
                     DeleteManyMock (DeleteDbPicsForDrafts [4]),
                     DeleteManyMock (DeleteDbTagsForDrafts [4]),
                     DeleteManyMock (DeleteDbDrafts [4]),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithDrafts handle qStr1 (ToDelete 4) "") []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")

toPicUrl :: Integer -> Text
toPicUrl iD = pack $ "http://localhost:3000/pictures/" ++ show iD

draftResp0 :: DraftResponse
draftResp0 =
  let catResp = SubCatResponse 9 "i" [15] $ CatResponse 3 "c" [9, 10]
      picsResps = [PicIdUrl 6 (toPicUrl 6), PicIdUrl 9 (toPicUrl 9), PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats", TagResponse 18 "dogs", TagResponse 20 "birds"]
   in DraftResponse 4 (PostIdExist 7) (AuthorResponse 7 "author" 3) "draft" catResp "lalala" 6 (toPicUrl 6) picsResps tagsResps

draftResp1 :: DraftResponse
draftResp1 =
  let catResp = SubCatResponse 15 "o" [19, 20] $ SubCatResponse 9 "i" [15] $ CatResponse 3 "c" [9, 10]
      picsResps = [PicIdUrl 6 (toPicUrl 6), PicIdUrl 9 (toPicUrl 9), PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats", TagResponse 18 "dogs", TagResponse 20 "birds"]
   in DraftResponse 1 (PostIdExist 7) (AuthorResponse 7 "author" 4) "draft" catResp "lalala" 6 (toPicUrl 6) picsResps tagsResps

draftResp2 :: DraftResponse
draftResp2 =
  let catResp = SubCatResponse 24 "u" [] $ SubCatResponse 20 "t" [] $ SubCatResponse 15 "o" [19, 20] $ SubCatResponse 9 "i" [15] $ CatResponse 3 "c" [9, 10]
      picsResps = [PicIdUrl 6 (toPicUrl 6), PicIdUrl 9 (toPicUrl 9), PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats", TagResponse 18 "dogs", TagResponse 20 "birds"]
   in DraftResponse 5 PostIdNull (AuthorResponse 7 "author" 4) "draft5" catResp "lalala" 4 (toPicUrl 4) picsResps tagsResps

draftResp3 :: DraftResponse
draftResp3 =
  let catResp = SubCatResponse 17 "q" [] $ SubCatResponse 12 "l" [16, 17] $ SubCatResponse 4 "d" [11, 12] $ CatResponse 1 "a" [4, 5, 6]
      picsResps = [PicIdUrl 6 (toPicUrl 6), PicIdUrl 9 (toPicUrl 9), PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats", TagResponse 18 "dogs", TagResponse 20 "birds"]
   in DraftResponse 12 PostIdNull (AuthorResponse 7 "author" 4) "draft12" catResp "lalala" 13 (toPicUrl 13) picsResps tagsResps
