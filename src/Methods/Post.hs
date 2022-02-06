--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}




module Methods.Post where
          
import           Api.Response
import           Logger
import           Types
import           Oops
import           Methods.Handle
import Methods.Handle.Select (Post(..))
import Methods.Category (makeCatResp)
import ParseQueryStr 
import Conf (Config(..))
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Data.Text                      ( pack, unpack, Text )
import           Database.PostgreSQL.Simple (query, withTransaction, execute, executeMany,Connection,Only(..),Binary(Binary))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( catch, throwM, MonadCatch)
import Methods.Post.LimitArg
import           Data.List                      ( intercalate, zip4 )
import           Data.Time.Calendar             ( showGregorian)


  
getPost :: (MonadCatch m) => MethodsHandle m -> PostId -> ExceptT ReqError m ResponseInfo 
getPost h postIdNum = do
  let postIdParam = numToTxt postIdNum
  Post pId auId auInfo usId pName pDate pCatId pText picId <- selectOneIfExistE h "posts JOIN authors ON authors.author_id = posts.author_id " ["posts.post_id","posts.author_id","author_info","user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"] "post_id=?" postIdParam 
  onlyPicsIds <- selectListFromDbE h "postspics" ["pic_id"] "post_id=?" [postIdParam] 
  let picsIds = fmap fromOnly onlyPicsIds
  tagS <- selectListFromDbE h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?" [postIdParam] 
  catResp <- makeCatResp h  pCatId
  okHelper $ PostResponse {post_id = postIdNum, author4 = AuthorResponse auId auInfo usId, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}

getPosts :: (MonadCatch m) => MethodsHandle m -> Request -> Integer -> ExceptT ReqError m ResponseInfo 
getPosts h req pageNum = do
  let extractParams = ["posts.post_id","posts.author_id","author_info","authors.user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
  LimitArg filterArgs sortArgs <- chooseArgs req 
  let defTable = "posts JOIN authors ON authors.author_id = posts.author_id"
  let defOrderBy = if isDateASC sortArgs then "post_create_date ASC, post_id ASC" else "post_create_date DESC, post_id DESC"
  let defWhere = "true"
  let defValues = []
  params <- selectListLimitFromDbE h defTable defOrderBy pageNum (cPostsLimit . hConf $ h) extractParams defWhere defValues filterArgs sortArgs 
  let postIdsText = fmap (pack . show . post_idP) params
  let postCatsIds = fmap post_cat_idP params 
  manyCatResp <- mapM (makeCatResp h) postCatsIds
  manyOnlyPostPicsIds <- mapM (selectListFromDbE h "postspics" ["pic_id"] "post_id=?") $ fmap (:[]) postIdsText  
  let manyPostPicsIds = (fmap . fmap) fromOnly manyOnlyPostPicsIds
  tagSMany <- mapM (selectListFromDbE h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?") $ fmap (:[]) postIdsText  
  let allParams = zip4 params manyCatResp manyPostPicsIds tagSMany
  okHelper $ PostsResponse {page10 = pageNum , posts10 = fmap (\((Post pId auId auInfo usId pName pDate pCat pText picId),catResp,pics,tagS) -> PostResponse {post_id = pId, author4 = AuthorResponse auId auInfo usId, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl pics, post_tags = fmap inTagResp tagS}) allParams}


deletePost :: (MonadCatch m) => MethodsHandle m -> DeletePost -> ExceptT ReqError m ResponseInfo 
deletePost h (DeletePost postIdNum) = do
  let postIdParam = numToTxt postIdNum
  isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
  withTransactionDBE h $ deleteAllAboutPost h postIdNum
  okHelper $ OkResponse { ok = True }

deleteAllAboutPost :: (MonadCatch m) => MethodsHandle m  -> PostId -> m ()
deleteAllAboutPost h postId = do
  let postIdTxt = pack . show $ postId
  deletePostsPicsTags h [postId]
  deleteFromDb h "comments" "post_id=?" [postIdTxt]
  onlyDraftsIds <- select h "drafts" ["draft_id"] "post_id=?" [postIdTxt]  
  deleteAllAboutDrafts h $ fmap fromOnly onlyDraftsIds
  deleteFromDb h "posts" "post_id=?" [postIdTxt]

deletePostsPicsTags :: (MonadCatch m) => MethodsHandle m  -> [PostId] -> m ()
deletePostsPicsTags _ [] = return ()
deletePostsPicsTags h postsIds = do
  let values = fmap (pack . show) postsIds
  let where' = intercalate " OR " . fmap (const "post_id=?") $ postsIds
  deleteFromDb h "postspics" where' values
  deleteFromDb h "poststags" where' values

deleteAllAboutDrafts :: (MonadCatch m) => MethodsHandle m  -> [DraftId] -> m ()
deleteAllAboutDrafts _ [] = return ()
deleteAllAboutDrafts h draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteDraftsPicsTags h draftsIds
  deleteFromDb h "drafts" where' values

deleteDraftsPicsTags :: (MonadCatch m) => MethodsHandle m  -> [DraftId] -> m ()
deleteDraftsPicsTags _ [] = return ()
deleteDraftsPicsTags h draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteFromDb h "draftspics" where' values
  deleteFromDb h "draftstags" where' values