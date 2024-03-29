module Spec.Post where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (GetPosts (..), GetPostsF (..), GetPostsOrd (..))
import Api.Response
  ( AuthorResponse (..),
    CatResponse (..),
    PicIdUrl (..),
    PostResponse (..),
    PostsResponse (..),
    SubCatResponse (..),
    SuperCatResponse (..),
    TagResponse (..),
  )
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Data.Text (Text, pack)
import Methods.Common (ResponseInfo (..), jsonHeader, textHeader)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Post
import Network.HTTP.Types (status200, status204)
import Psql.ToQuery.SelectLimit (OrderBy (..))
import Spec.Auth.Types
import Spec.DeleteMany.Types
import Spec.Exist.Types
import Spec.MakeCatResp.Types
import Spec.Post.Handlers
import Spec.Post.QStrExample
import Spec.Post.Types
import Spec.Types (MockAction (..))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types

testPost :: IO ()
testPost = hspec $ do
  let no = Nothing
  describe "getPost"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ getPost handle 4) []
      reverse state
        `shouldBe` [ PostMock (SelectPosts 4),
                     PostMock (SelectPicsForPost 4),
                     PostMock (SelectTagsForPost 4),
                     MakeCatRMock (SelectCats 4),
                     MakeCatRMock (SelectSubCats 4),
                     MakeCatRMock (SelectCats 1),
                     MakeCatRMock (SelectSubCats 1)
                   ]
      eitherResp <- evalStateT (runExceptT $ getPost handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] $ encode postResp0)
  describe "getPosts"
    $ it "work with valid DB answer"
    $ do
      state <-
        execStateT
          ( runExceptT $
              getPosts
                handle
                ( GetPosts
                    1
                    (GetPostsF no no no no no no no no no no no)
                    (GetPostsOrd no no no no)
                )
          )
          []
      reverse state
        `shouldBe` [ PostMock (SelectLimPosts [] (OrderList [ByPostDate DESC, ByPostId DESC]) 1 5),
                     PostMock (SelectPicsForPost 1),
                     PostMock (SelectTagsForPost 1),
                     MakeCatRMock (SelectCats 4),
                     MakeCatRMock (SelectSubCats 4),
                     MakeCatRMock (SelectCats 1),
                     MakeCatRMock (SelectSubCats 1),
                     PostMock (SelectPicsForPost 2),
                     PostMock (SelectTagsForPost 2),
                     MakeCatRMock (SelectCats 2),
                     MakeCatRMock (SelectSubCats 2),
                     PostMock (SelectPicsForPost 3),
                     PostMock (SelectTagsForPost 3),
                     MakeCatRMock (SelectCats 1),
                     MakeCatRMock (SelectSubCats 1)
                   ]
      eitherResp <-
        evalStateT
          ( runExceptT $
              getPosts
                handle
                ( GetPosts
                    1
                    (GetPostsF no no no no no no no no no no no)
                    (GetPostsOrd no no no no)
                )
          )
          []
      eitherResp
        `shouldBe` ( Right $ ResponseInfo status200 [jsonHeader] $ encode $
                       PostsResponse 1 [postResp1, postResp2, postResp3]
                   )
  describe "deletePost"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ deletePost handle 4) []
      reverse state
        `shouldBe` [ TRANSACTIONOPEN,
                     DeleteManyMock (DeleteDbPicsForPost 4),
                     DeleteManyMock (DeleteDbTagsForPost 4),
                     DeleteManyMock (DeleteDbCommsForPost 4),
                     DeleteManyMock (SelectDraftsForPost 4),
                     DeleteManyMock (DeleteDbPicsForDrafts [5, 7]),
                     DeleteManyMock (DeleteDbTagsForDrafts [5, 7]),
                     DeleteManyMock (DeleteDbDrafts [5, 7]),
                     DeleteManyMock (DeleteDbPost 4),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ deletePost handle 4) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")
  describe "workWithPosts (ToGet)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithPosts handle [] (ToGet 4)) []
      reverse state
        `shouldBe` [ ExistMock (IsExist (PostId 4)),
                     PostMock (SelectPosts 4),
                     PostMock (SelectPicsForPost 4),
                     PostMock (SelectTagsForPost 4),
                     MakeCatRMock (SelectCats 4),
                     MakeCatRMock (SelectSubCats 4),
                     MakeCatRMock (SelectCats 1),
                     MakeCatRMock (SelectSubCats 1)
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithPosts handle [] (ToGet 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status200 [jsonHeader] $ encode postResp0)
  describe "workWithPosts (ToGetAll)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithPosts handle qStr3 ToGetAll) []
      reverse state
        `shouldBe` [ PostMock (SelectLimPosts [] (OrderList [ByPostDate DESC, ByPostId DESC]) 1 5),
                     PostMock (SelectPicsForPost 1),
                     PostMock (SelectTagsForPost 1),
                     MakeCatRMock (SelectCats 4),
                     MakeCatRMock (SelectSubCats 4),
                     MakeCatRMock (SelectCats 1),
                     MakeCatRMock (SelectSubCats 1),
                     PostMock (SelectPicsForPost 2),
                     PostMock (SelectTagsForPost 2),
                     MakeCatRMock (SelectCats 2),
                     MakeCatRMock (SelectSubCats 2),
                     PostMock (SelectPicsForPost 3),
                     PostMock (SelectTagsForPost 3),
                     MakeCatRMock (SelectCats 1),
                     MakeCatRMock (SelectSubCats 1)
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithPosts handle qStr3 ToGetAll) []
      eitherResp
        `shouldBe` ( Right $ ResponseInfo status200 [jsonHeader] $ encode $
                       PostsResponse 1 [postResp1, postResp2, postResp3]
                   )
  describe "workWithPosts (ToDelete)"
    $ it "work with valid DB answer"
    $ do
      state <- execStateT (runExceptT $ workWithPosts handle qStr1 (ToDelete 4)) []
      reverse state
        `shouldBe` [ AuthMock (SelectTokenKeyForUser 3),
                     ExistMock (IsExist (PostId 4)),
                     TRANSACTIONOPEN,
                     DeleteManyMock (DeleteDbPicsForPost 4),
                     DeleteManyMock (DeleteDbTagsForPost 4),
                     DeleteManyMock (DeleteDbCommsForPost 4),
                     DeleteManyMock (SelectDraftsForPost 4),
                     DeleteManyMock (DeleteDbPicsForDrafts [5, 7]),
                     DeleteManyMock (DeleteDbTagsForDrafts [5, 7]),
                     DeleteManyMock (DeleteDbDrafts [5, 7]),
                     DeleteManyMock (DeleteDbPost 4),
                     TRANSACTIONCLOSE
                   ]
      eitherResp <- evalStateT (runExceptT $ workWithPosts handle qStr1 (ToDelete 4)) []
      eitherResp
        `shouldBe` (Right $ ResponseInfo status204 [textHeader] "Status 204 No data")

toPicUrl :: Integer -> Text
toPicUrl iD = pack $ "http://localhost:3000/pictures/" ++ show iD

postResp0 :: PostResponse
postResp0 =
  let catResp = Sub $ SubCatResponse 4 "d" [11, 12] $ Super $ SuperCatResponse 1 "a" [4, 5, 6]
      picsResps = [PicIdUrl 6 (toPicUrl 6), PicIdUrl 9 (toPicUrl 9), PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats", TagResponse 18 "dogs", TagResponse 20 "birds"]
   in PostResponse
        4
        (AuthorResponse 7 "author" 3)
        "post"
        dayExample
        catResp
        "lalala"
        8
        (toPicUrl 8)
        picsResps
        tagsResps

postResp1 :: PostResponse
postResp1 = postResp0 {postIdP = 1}

postResp2 :: PostResponse
postResp2 =
  let catResp = Super $ SuperCatResponse 2 "b" [7, 8]
      picsResps = [PicIdUrl 6 (toPicUrl 6), PicIdUrl 9 (toPicUrl 9), PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats", TagResponse 18 "dogs", TagResponse 20 "birds"]
   in PostResponse
        2
        (AuthorResponse 8 "author" 4)
        "post2"
        dayExample
        catResp
        "lalala"
        7
        (toPicUrl 7)
        picsResps
        tagsResps

postResp3 :: PostResponse
postResp3 =
  let catResp = Super $ SuperCatResponse 1 "a" [4, 5, 6]
      picsResps = [PicIdUrl 6 (toPicUrl 6), PicIdUrl 9 (toPicUrl 9), PicIdUrl 12 (toPicUrl 12)]
      tagsResps = [TagResponse 15 "cats", TagResponse 18 "dogs", TagResponse 20 "birds"]
   in PostResponse
        3
        (AuthorResponse 9 "author" 5)
        "post3"
        dayExample
        catResp
        "lalala"
        6
        (toPicUrl 6)
        picsResps
        tagsResps
