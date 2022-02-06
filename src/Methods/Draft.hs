--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}




module Methods.Draft where
          
import           Api.Response
import Api.Request (DraftRequest(..))
import           Logger
import           Types
import           Oops
import           Methods.Handle
import Methods.Handle.Select (Author(..),Draft(..),PostInfo(..),Tag(..))
import Methods.Auth (AccessMode(..))
import ParseQueryStr 
import Conf (Config(..))
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Data.Text                      ( pack, unpack, Text )
import           Database.PostgreSQL.Simple (query, withTransaction, execute, executeMany,Connection,Only(..),Binary(Binary))
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( catch, throwM, MonadCatch)
import           Data.List                      ( intercalate, zip4, nub)
import           Data.Time.Calendar             ( showGregorian)
import Methods.Category (makeCatResp)
import Methods.Post (deleteAllAboutPost, deletePostsPicsTags, deleteAllAboutDrafts,deleteDraftsPicsTags)


createNewDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> DraftRequest -> ExceptT ReqError m ResponseInfo
createNewDraft h usIdNum body = do
  let nameParam    = draft_name   body
  let catIdParam   = draft_cat_id body
  let txtParam     = draft_textDR  body
  let picId        = draft_main_pic_id body
  let tagsIds      = nub . draft_tags_ids $ body
  let picsIds      = draft_pics_ids $ body
  isExistInDbE h "categories" "category_id" "category_id=?" [pack . show $ catIdParam] 
  mapM_ (isExistInDbE h "tags" "tag_id" "tag_id=?") $ fmap ( (:[]) . pack . show) tagsIds
  Author auId auInfo usId <- isUserAuthorE h  usIdNum 
  let where' = intercalate " OR " . fmap (const "tag_id=?") $ tagsIds
  tagS <- selectListFromDbE h "tags" ["tag_id","tag_name"] where' (fmap (pack . show) tagsIds)
  catResp <- makeCatResp h  catIdParam
  let insNames  = ["author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
  let insValues = [pack . show $ auId,nameParam,pack . show $ catIdParam,txtParam,pack . show $ picId]
  draftId <- withTransactionDBE h $ do  
    draftIds <-  insertReturn h "drafts" "draft_id" insNames insValues           
    draftId <- checkSingleOutPut draftIds
    insertMany h "draftspics" ["draft_id","pic_id"] (zip (repeat draftId) picsIds)
    insertMany h "draftstags" ["draft_id","tag_id"] (zip (repeat draftId) tagsIds)
    return draftId  
  okHelper $ DraftResponse { draft_id2 = draftId, post_id2 = PostIdNull , author2 = AuthorResponse auId auInfo usId, draft_name2 = nameParam , draft_cat2 = catResp , draft_text2 = txtParam , draft_main_pic_id2 =  picId , draft_main_pic_url2 = makeMyPicUrl picId , draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}


createPostsDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> CreatePostsDraft -> ExceptT ReqError m ResponseInfo
createPostsDraft h usIdNum (CreatePostsDraft postIdNum) = do
  let postIdParam = numToTxt postIdNum
  isUserAuthorE_ h  usIdNum 
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  let params = ["a.author_id","author_info","post_name","post_category_id","post_text","post_main_pic_id"]
  PostInfo auId auInfo postName postCatId postTxt mPicId <- selectOneIfExistE h table params "post_id=?" postIdParam
  isPostAuthor h  postIdParam usIdNum          
  onlyPicsIds <- selectListFromDbE h "postspics" ["pic_id"] "post_id=?" [postIdParam] 
  let picsIds = fmap fromOnly onlyPicsIds
  tagS <- selectListFromDbE h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?" [postIdParam]
  let tagsIds = fmap tag_idT tagS
  catResp <- makeCatResp h  postCatId
  let insNames  = ["post_id","author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
  let insValues = [postIdParam,pack . show $ auId,postName,pack . show $ postCatId,postTxt,pack . show $ mPicId]
  draftId <-  insertReturnE h "drafts" "draft_id" insNames insValues
  insertManyE h "draftspics" ["draft_id","pic_id"] (zip (repeat draftId) picsIds)
  insertManyE h "draftstags" ["draft_id","tag_id"] (zip (repeat draftId) tagsIds) 
  okHelper $ DraftResponse {draft_id2 = draftId, post_id2 = PostIdExist postIdNum, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = postName , draft_cat2 = catResp, draft_text2 = postTxt, draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl mPicId , draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}

getDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> GetDraft -> ExceptT ReqError m ResponseInfo
getDraft h usIdNum (GetDraft draftIdNum) = do
  let draftIdParam = numToTxt draftIdNum
  let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
  let params = ["d.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
  Draft drId auInfo postId draftName draftCatId draftTxt mPicId <- selectOneIfExistE h table params "draft_id=?" draftIdParam         
  Author auId _ _ <- isUserAuthorE h  usIdNum  
  isDraftAuthor h  draftIdParam usIdNum
  onlyPicsIds <- selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?" [draftIdParam]
  let picsIds = fmap fromOnly onlyPicsIds
  tagS <- selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam] 
  catResp <- makeCatResp h  draftCatId
  okHelper $ DraftResponse { draft_id2 = draftIdNum, post_id2 = isNULL postId, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName , draft_cat2 = catResp, draft_text2 = draftTxt , draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl mPicId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}

getDrafts :: (MonadCatch m) => MethodsHandle m -> UserId -> GetDrafts -> ExceptT ReqError m ResponseInfo
getDrafts h usIdNum (GetDrafts pageNum) = do
  Author auId _ _ <- isUserAuthorE h  usIdNum  
  let table = "drafts JOIN authors ON authors.author_id = drafts.author_id"
  let orderBy = "draft_id DESC"
  let extractParams = ["drafts.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
  let where' = "drafts.author_id = ?"
  let values = [pack . show $ auId]
  drafts <- selectListLimitFromDbE h table orderBy pageNum (cDraftsLimit . hConf $ h) extractParams where' values [] []
  let alldraftIdsText = fmap (pack . show . draft_idD) drafts
  let allCatIdsNum = fmap draft_cat_idD drafts
  manyCatResp <- mapM (makeCatResp h ) allCatIdsNum
  manyOnlyDraftPicsIds   <- mapM (selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?") $ fmap (:[]) alldraftIdsText  
  let manyDraftPicsIds = (fmap . fmap) fromOnly manyOnlyDraftPicsIds
  tagSMany <- mapM (selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" ) $ fmap (:[]) alldraftIdsText
  let allParams = zip4 drafts manyCatResp manyDraftPicsIds tagSMany
  okHelper $ DraftsResponse 
    { page9 = pageNum
    , drafts9 = fmap (\(( Draft draftId auInfo postId draftName draftCat draftText draftMainPicId ),catResp,pics,tagS) -> DraftResponse { draft_id2 = draftId, post_id2 = isNULL postId , author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName , draft_cat2 = catResp, draft_text2 = draftText, draft_main_pic_id2 =  draftMainPicId, draft_main_pic_url2 = makeMyPicUrl draftMainPicId , draft_tags2 = fmap inTagResp tagS, draft_pics2 =  fmap inPicIdUrl pics}) allParams }

updateDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> DraftId -> DraftRequest -> ExceptT ReqError m ResponseInfo
updateDraft h usIdNum draftIdNum body = do
  let draftIdParam = numToTxt draftIdNum
  let nameParam    = draft_name   body
  let catIdParam   = draft_cat_id body
  let txtParam     = draft_textDR  body
  let picId        = draft_main_pic_id body
  let tagsIds      = nub . draft_tags_ids $ body
  let picsIds      = draft_pics_ids $ body
  isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftIdParam] 
  isExistInDbE h "categories" "category_id" "category_id=?" [pack . show $ catIdParam] 
  mapM_ (isExistInDbE h "tags" "tag_id" "tag_id=?" ) $ fmap ( (:[]) . pack . show) tagsIds
  Author auId auInfo usId <- isUserAuthorE h  usIdNum  
  Only postId <- selectOneE h "drafts" ["COALESCE (post_id, '0') AS post_id"] "draft_id=?" [draftIdParam] 
  let where' = intercalate " OR " . fmap (const "tag_id=?") $ tagsIds
  tagS <- selectListFromDbE h "tags" ["tag_id","tag_name"] where' (fmap (pack . show) tagsIds)
  catResp <- makeCatResp h  catIdParam  
  withTransactionDBE h $ do
    deleteDraftsPicsTags h [draftIdNum]
    updateInDb h "drafts" "draft_name=?,draft_category_id=?,draft_text=?,draft_main_pic_id=?" "draft_id=?" [nameParam,pack . show $ catIdParam,txtParam,pack . show $ picId,draftIdParam]
    insertMany h "draftspics" ["draft_id","pic_id"] (zip (repeat draftIdNum) picsIds)
    insertMany h "draftstags" ["draft_id","tag_id"] (zip (repeat draftIdNum) tagsIds)
  okHelper $ DraftResponse {draft_id2 = draftIdNum, post_id2 = isNULL postId, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = nameParam, draft_cat2 = catResp, draft_text2 = txtParam, draft_main_pic_id2 =  picId, draft_main_pic_url2 = makeMyPicUrl picId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}

deleteDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> DeleteDraft -> ExceptT ReqError m ResponseInfo
deleteDraft h usIdNum (DeleteDraft draftIdNum) = do
  let draftIdParam = numToTxt draftIdNum
  isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftIdParam] 
  isUserAuthorE_ h  usIdNum  
  isDraftAuthor h  draftIdParam usIdNum
  withTransactionDBE h $  deleteAllAboutDrafts h  [draftIdNum]
  okHelper $ OkResponse { ok = True }

publishDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> PublishDraft -> ExceptT ReqError m ResponseInfo
publishDraft h usIdNum (PublishDraft draftIdNum) = do
  let draftIdParam = numToTxt draftIdNum       
  let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
  let params = ["d.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
  Draft drId auInfo draftPostId draftName draftCatId draftTxt mPicId <- selectOneIfExistE h table params "draft_id=?" draftIdParam
  Author auId _ _ <- isUserAuthorE h  usIdNum  
  isDraftAuthor h  draftIdParam usIdNum
  case draftPostId of
    0 -> do    
      onlyPicsIds <- selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?" [draftIdParam] 
      let picsIds = fmap fromOnly onlyPicsIds
      tagS <- selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam]
      day <- lift $ getDay h 
      let insNames  = ["author_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
      let insValues = [pack . show $ auId,draftName,pack day,pack . show $ draftCatId,draftTxt,pack . show $ mPicId]          
      postId <-  insertReturnE h "posts" "post_id" insNames insValues          
      insertManyE h "postspics" ["post_id","pic_id"] (zip (repeat postId) picsIds)
      insertManyE h "poststags" ["post_id","tag_id"] (zip (repeat postId) (fmap tag_idT tagS))
      catResp <- makeCatResp h  draftCatId 
      okHelper $ PostResponse {post_id = postId, author4 = AuthorResponse auId auInfo usIdNum, post_name = draftName , post_create_date = pack day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = makeMyPicUrl mPicId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}
    _ -> do       
      onlyPicsIds <- selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?" [draftIdParam] 
      let picsIds = fmap fromOnly onlyPicsIds
      catResp <- makeCatResp h  draftCatId
      Only day <- selectOneE h "posts" ["post_create_date"] "post_id=?" [pack . show $ draftPostId]    
      tagS <- selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam] 
      withTransactionDBE h $ do
        updateInDb h "posts" "post_name=?,post_category_id=?,post_text=?,post_main_pic_id=?" "post_id=?" [draftName,pack . show $ draftCatId,draftTxt,pack . show $ mPicId,pack . show $ draftPostId]
        deletePostsPicsTags h [draftPostId]
        insertMany h "postspics" ["post_id","pic_id"] (zip (repeat draftPostId) picsIds)
        insertMany h "poststags" ["post_id","tag_id"] (zip (repeat draftPostId) (fmap tag_idT tagS))
      okHelper $ PostResponse {post_id = draftPostId, author4 = AuthorResponse auId auInfo usIdNum, post_name = draftName , post_create_date = pack . showGregorian $ day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = makeMyPicUrl mPicId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}


isDraftAuthor :: (MonadCatch m) => MethodsHandle m  -> Text -> UserId -> ExceptT ReqError m ()
isDraftAuthor h  draftIdParam usIdNum = do
  let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
  Only usDraftId <- selectOneE h table ["user_id"] "draft_id=?" [draftIdParam]  
  case usDraftId == usIdNum of
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of draft_id: " ++ unpack draftIdParam

isPostAuthor :: (MonadCatch m) => MethodsHandle m  -> Text -> UserId -> ExceptT ReqError m ()
isPostAuthor h  postIdParam usIdNum = do
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  Only usPostId <- selectOneE h table ["user_id"] "post_id=?" [postIdParam]  
  case usPostId  == usIdNum of
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of post_id: " ++ unpack postIdParam

isUserAuthorE :: (MonadCatch m) => MethodsHandle m -> UserId -> ExceptT ReqError m Author
isUserAuthorE h  usIdNum = do
  lift $ logDebug (hLog h) $ "Checking in DB is user author"  
  maybeAu <- selectMaybeOneE h "authors" ["author_id","author_info","user_id"] "user_id=?" [pack . show $ usIdNum] 
  case maybeAu of
    Nothing -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " isn`t author"
    Just author -> return author

isUserAuthorE_ :: (MonadCatch m) => MethodsHandle m -> UserId -> ExceptT ReqError m ()
isUserAuthorE_ h usIdNum = do
  isUserAuthorE h  usIdNum
  return ()

 

isNULL :: PostId -> PostIdOrNull
isNULL 0      = PostIdNull
isNULL postId = PostIdExist postId