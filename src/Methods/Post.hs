{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Methods.Post where
          
import           Api.Response (PostResponse(..), OkResponse(..),PostsResponse(..),AuthorResponse(..))
import           Logger
import           Types
import           Oops
import           Methods.Handle
import Methods.Handle.Select (Post(..))
import Methods.Category (makeCatResp)
import ParseQueryStr (DeletePost(..))
import Conf (Config(..))
import           Network.Wai (Request)
import           Data.Text                      ( pack )
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( MonadCatch)
import Methods.Post.LimitArg (chooseArgs, isDateASC,LimitArg(..))
import           Data.List                      ( intercalate, zip4 )
import           Data.Time.Calendar             ( showGregorian)


  
getPost :: (MonadCatch m) => Handle m -> PostId -> ExceptT ReqError m ResponseInfo 
getPost h postIdNum = do
  let postIdParam = numToTxt postIdNum
  Post pId auId auInfo usId pName pDate pCatId pText picId <- checkOneIfExistE h (selectPost h) "posts JOIN authors ON authors.author_id = posts.author_id " ["posts.post_id","posts.author_id","author_info","user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"] "post_id=?" postIdParam 
  picsIds <- checkListE h $ selectNum h "postspics" ["pic_id"] "post_id=?" [postIdParam] 
  tagS <- checkListE h $ selectTag h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?" [postIdParam] 
  catResp <- makeCatResp h  pCatId
  lift $ logInfo (hLog h) $ "Post_id: " ++ show pId ++ " sending in response" 
  okHelper $ PostResponse {post_id = pId, author4 = AuthorResponse auId auInfo usId, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}

getPosts :: (MonadCatch m) => Handle m -> Request -> Integer -> ExceptT ReqError m ResponseInfo 
getPosts h req pageNum = do
  let extractParams = ["posts.post_id","posts.author_id","author_info","authors.user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
  LimitArg filterArgs sortArgs <- chooseArgs req 
  let defTable = "posts JOIN authors ON authors.author_id = posts.author_id"
  let defOrderBy = if isDateASC sortArgs then "post_create_date ASC, post_id ASC" else "post_create_date DESC, post_id DESC"
  let defWhere = "true"
  let defValues = []
  posts <- checkListE h $ selectLimitPost h defTable defOrderBy pageNum (cPostsLimit . hConf $ h) extractParams defWhere defValues filterArgs sortArgs 
  let postIdsText = fmap (pack . show . post_idP) posts
  let postCatsIds = fmap post_cat_idP posts 
  manyCatResp <- mapM (makeCatResp h) postCatsIds
  manyPostPicsIds <- mapM (checkListE h . selectNum h "postspics" ["pic_id"] "post_id=?") $ fmap (:[]) postIdsText  
  tagSMany <- mapM (checkListE h . selectTag h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?") $ fmap (:[]) postIdsText  
  let allParams = zip4 posts manyCatResp manyPostPicsIds tagSMany
  lift $ logInfo (hLog h) $ "Post_ids: " ++ show (fmap post_idP posts) ++ " sending in response" 
  okHelper $ PostsResponse {page10 = pageNum , posts10 = fmap (\((Post pId auId auInfo usId pName pDate _ pText picId),catResp,pics,tagS) -> PostResponse {post_id = pId, author4 = AuthorResponse auId auInfo usId, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl pics, post_tags = fmap inTagResp tagS}) allParams}


deletePost :: (MonadCatch m) => Handle m -> DeletePost -> ExceptT ReqError m ResponseInfo 
deletePost h (DeletePost postIdNum) = do
  let postIdParam = numToTxt postIdNum
  isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
  withTransactionDBE h $ deleteAllAboutPost h postIdNum
  lift $ logInfo (hLog h) $ "Post_id: " ++ show postIdNum ++ " deleted" 
  okHelper $ OkResponse { ok = True }

deleteAllAboutPost :: (MonadCatch m) => Handle m  -> PostId -> m ()
deleteAllAboutPost h postId = do
  let postIdTxt = pack . show $ postId
  deletePostsPicsTags h [postId]
  deleteFromDb h "comments" "post_id=?" [postIdTxt]
  draftsIds <- selectNum h "drafts" ["draft_id"] "post_id=?" [postIdTxt]  
  deleteAllAboutDrafts h draftsIds
  deleteFromDb h "posts" "post_id=?" [postIdTxt]

deletePostsPicsTags :: (MonadCatch m) => Handle m  -> [PostId] -> m ()
deletePostsPicsTags _ [] = return ()
deletePostsPicsTags h postsIds = do
  let values = fmap (pack . show) postsIds
  let where' = intercalate " OR " . fmap (const "post_id=?") $ postsIds
  deleteFromDb h "postspics" where' values
  deleteFromDb h "poststags" where' values

deleteAllAboutDrafts :: (MonadCatch m) => Handle m  -> [DraftId] -> m ()
deleteAllAboutDrafts _ [] = return ()
deleteAllAboutDrafts h draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteDraftsPicsTags h draftsIds
  deleteFromDb h "drafts" where' values

deleteDraftsPicsTags :: (MonadCatch m) => Handle m  -> [DraftId] -> m ()
deleteDraftsPicsTags _ [] = return ()
deleteDraftsPicsTags h draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteFromDb h "draftspics" where' values
  deleteFromDb h "draftstags" where' values