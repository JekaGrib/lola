{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Methods.Draft where
          
import           Api.Response (DraftResponse(..),DraftsResponse(..),OkResponse(..),PostIdOrNull(..),CatResponse(..),TagResponse(..),AuthorResponse(..),PostResponse(..),PicIdUrl(pic_idPU))
import Api.Request (DraftRequest(..))
import           Logger
import           Types
import           Oops
import           Methods.Handle
import Methods.Handle.Select (Author(..),Draft(..),PostInfo(..),Tag(..))
import ParseQueryStr (CreatePostsDraft(..),GetDraft(..),GetDrafts(..),DeleteDraft(..),PublishDraft(..))
import Conf (Config(..))
import           Data.Text                      ( pack, unpack, Text )
import           Database.PostgreSQL.Simple (Only(..))
import           Control.Monad.Trans.Except (ExceptT,throwE)
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            (  MonadCatch)
import           Data.List                      ( intercalate, zip4)
import           Data.Time.Calendar             ( showGregorian)
import Methods.Category (makeCatResp,fromCatResp)
import Methods.Post ( deletePostsPicsTags, deleteAllAboutDrafts,deleteDraftsPicsTags)
import           Control.Monad (unless)


createNewDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> DraftRequest -> ExceptT ReqError m ResponseInfo
createNewDraft h usIdNum drReq@(DraftRequest _ nameParam catIdParam txtParam picId picsIds tagsIds) = do
  DraftInfo auResp@(AuthorResponse auId _ _) tagResps catResp <- getDraftInfo h usIdNum drReq
  draftId <- withTransactionDBE h $ do
    let insNames  = ["author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
    let insValues = [numToTxt auId,nameParam,numToTxt catIdParam,txtParam,numToTxt picId]
    draftIds <-  insertReturn h "drafts" "draft_id" insNames insValues           
    draftId <- checkSingleOutPut draftIds
    insertMany h "draftspics" ["draft_id","pic_id"] (zip (repeat draftId) picsIds)
    insertMany h "draftstags" ["draft_id","tag_id"] (zip (repeat draftId) tagsIds)
    return draftId  
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " created"
  okHelper $ DraftResponse { draft_id2 = draftId, post_id2 = PostIdNull , author2 = auResp, draft_name2 = nameParam , draft_cat2 = catResp , draft_text2 = txtParam , draft_main_pic_id2 =  picId , draft_main_pic_url2 = makeMyPicUrl picId , draft_tags2 = tagResps, draft_pics2 = fmap inPicIdUrl picsIds}


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
  draftId <- withTransactionDBE h $ do
    let insNames  = ["post_id","author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
    let insValues = [postIdParam,pack . show $ auId,postName,pack . show $ postCatId,postTxt,pack . show $ mPicId]
    draftIds <-  insertReturn h "drafts" "draft_id" insNames insValues
    draftId <- checkSingleOutPut draftIds
    insertMany h "draftspics" ["draft_id","pic_id"] (zip (repeat draftId) picsIds)
    insertMany h "draftstags" ["draft_id","tag_id"] (zip (repeat draftId) tagsIds)
    return draftId
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " created for post_id: " ++ show postIdNum
  okHelper $ DraftResponse {draft_id2 = draftId, post_id2 = PostIdExist postIdNum, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = postName , draft_cat2 = catResp, draft_text2 = postTxt, draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl mPicId , draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}

getDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> GetDraft -> ExceptT ReqError m ResponseInfo
getDraft h usIdNum (GetDraft draftIdNum) = do
  resp <- selectDraftAndMakeResp h usIdNum draftIdNum
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftIdNum ++ " sending in response" 
  okHelper resp

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
  lift $ logInfo (hLog h) $ "Draft_ids: " ++ show (fmap draft_idD drafts) ++ " sending in response" 
  okHelper $ DraftsResponse 
    { page9 = pageNum
    , drafts9 = fmap (\( Draft draftId auInfo postId draftName _ draftText draftMainPicId ,catResp,pics,tagS) -> DraftResponse { draft_id2 = draftId, post_id2 = isNULL postId , author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName , draft_cat2 = catResp, draft_text2 = draftText, draft_main_pic_id2 =  draftMainPicId, draft_main_pic_url2 = makeMyPicUrl draftMainPicId , draft_tags2 = fmap inTagResp tagS, draft_pics2 =  fmap inPicIdUrl pics}) allParams }

updateDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> DraftId -> DraftRequest -> ExceptT ReqError m ResponseInfo
updateDraft h usIdNum draftIdNum drReq@(DraftRequest _ nameParam catIdParam txtParam picId picsIds tagsIds)  = do
  let draftIdParam = numToTxt draftIdNum
  DraftInfo auResp tagResps catResp <- getDraftInfo h usIdNum drReq
  Only postId <- selectOneE h "drafts" ["COALESCE (post_id, '0') AS post_id"] "draft_id=?" [draftIdParam] 
  withTransactionDBE h $ do
    deleteDraftsPicsTags h [draftIdNum]
    updateInDb h "drafts" "draft_name=?,draft_category_id=?,draft_text=?,draft_main_pic_id=?" "draft_id=?" [nameParam,pack . show $ catIdParam,txtParam,pack . show $ picId,draftIdParam]
    insertMany h "draftspics" ["draft_id","pic_id"] (zip (repeat draftIdNum) picsIds)
    insertMany h "draftstags" ["draft_id","tag_id"] (zip (repeat draftIdNum) tagsIds)
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftIdNum ++ " updated"
  okHelper $ DraftResponse {draft_id2 = draftIdNum, post_id2 = isNULL postId, author2 = auResp, draft_name2 = nameParam, draft_cat2 = catResp, draft_text2 = txtParam, draft_main_pic_id2 =  picId, draft_main_pic_url2 = makeMyPicUrl picId, draft_tags2 = tagResps, draft_pics2 = fmap inPicIdUrl picsIds}

deleteDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> DeleteDraft -> ExceptT ReqError m ResponseInfo
deleteDraft h usIdNum (DeleteDraft draftIdNum) = do
  let draftIdParam = numToTxt draftIdNum
  isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftIdParam] 
  isUserAuthorE_ h  usIdNum  
  isDraftAuthor h  draftIdParam usIdNum
  withTransactionDBE h $  deleteAllAboutDrafts h  [draftIdNum]
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftIdNum ++ " deleted"
  okHelper $ OkResponse { ok = True }

publishDraft :: (MonadCatch m) => MethodsHandle m -> UserId -> PublishDraft -> ExceptT ReqError m ResponseInfo
publishDraft h usIdNum (PublishDraft draftIdNum) = do
  DraftResponse draftId draftPostId auResp@(AuthorResponse auId _ _) draftName catResp draftTxt mPicId mPicUrl picIdUrls tagResps <- selectDraftAndMakeResp h usIdNum draftIdNum
  case draftPostId of
    PostIdNull -> do    
      day <- lift $ getDay h 
      postId <- withTransactionDBE h $ do
        let insNames  = ["author_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
        let insValues = [pack . show $ auId,draftName,pack day,numToTxt . fromCatResp $ catResp,draftTxt,numToTxt mPicId]          
        postsId <- insertReturn h "posts" "post_id" insNames insValues
        postId <- checkSingleOutPut postsId  
        insertMany h "postspics" ["post_id","pic_id"] (zip (repeat postId) (fmap pic_idPU picIdUrls))
        insertMany h "poststags" ["post_id","tag_id"] (zip (repeat postId) (fmap tag_idTR tagResps))
        return postId
      lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " published as post_id: " ++ show postId
      okHelper $ PostResponse {post_id = postId, author4 = auResp, post_name = draftName , post_create_date = pack day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = mPicUrl, post_pics = picIdUrls, post_tags = tagResps}
    PostIdExist postId  -> do       
      Only day <- selectOneE h "posts" ["post_create_date"] "post_id=?" [numToTxt postId]    
      withTransactionDBE h $ do
        updateInDb h "posts" "post_name=?,post_category_id=?,post_text=?,post_main_pic_id=?" "post_id=?" [draftName,numToTxt . fromCatResp $ catResp,draftTxt,numToTxt mPicId,numToTxt postId]
        deletePostsPicsTags h [postId]
        insertMany h "postspics" ["post_id","pic_id"] (zip (repeat postId) (fmap pic_idPU picIdUrls))
        insertMany h "poststags" ["post_id","tag_id"] (zip (repeat postId) (fmap tag_idTR tagResps))
      lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " published as post_id: " ++ show postId
      okHelper $ PostResponse {post_id = postId, author4 = auResp, post_name = draftName , post_create_date = pack . showGregorian $ day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = mPicUrl, post_pics = picIdUrls, post_tags = tagResps}

  
selectDraftAndMakeResp :: (MonadCatch m) => MethodsHandle m -> UserId -> DraftId -> ExceptT ReqError m DraftResponse
selectDraftAndMakeResp h usIdNum draftIdNum = do
  let draftIdParam = numToTxt draftIdNum
  let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
  let params = ["d.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
  Draft drId auInfo draftPostId draftName draftCatId draftTxt mPicId <- selectOneIfExistE h table params "draft_id=?" draftIdParam         
  Author auId _ _ <- isUserAuthorE h  usIdNum  
  isDraftAuthor h  draftIdParam usIdNum
  onlyPicsIds <- selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?" [draftIdParam] 
  let picsIds = fmap fromOnly onlyPicsIds
  tagS <- selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam]
  catResp <- makeCatResp h  draftCatId 
  return DraftResponse { draft_id2 = drId, post_id2 = isNULL draftPostId, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName , draft_cat2 = catResp, draft_text2 = draftTxt , draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl mPicId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}

data DraftInfo = DraftInfo AuthorResponse [TagResponse] CatResponse

getDraftInfo :: (MonadCatch m) => MethodsHandle m -> UserId -> DraftRequest -> ExceptT ReqError m DraftInfo
getDraftInfo h usIdNum (DraftRequest _ _ catIdParam _ picId picsIds tagsIds) = do
  isExistInDbE h "categories" "category_id" "category_id=?" [pack . show $ catIdParam] 
  mapM_ (isExistInDbE h "tags" "tag_id" "tag_id=?") $ fmap ( (:[]) . pack . show) tagsIds
  mapM_ (isExistInDbE h "pics" "pic_id" "pic_id=?") $ fmap ( (:[]) . pack . show) (picId:picsIds)
  (Author auId auInfo usId) <- isUserAuthorE h  usIdNum 
  let where' = intercalate " OR " . fmap (const "tag_id=?") $ tagsIds
  tagS <- selectListFromDbE h "tags" ["tag_id","tag_name"] where' (fmap (pack . show) tagsIds)
  catResp <- makeCatResp h  catIdParam
  return $ DraftInfo (AuthorResponse auId auInfo usId) (fmap inTagResp tagS) catResp

isDraftAuthor :: (MonadCatch m) => MethodsHandle m  -> Text -> UserId -> ExceptT ReqError m ()
isDraftAuthor h  draftIdParam usIdNum = do
  let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
  Only usDraftId <- selectOneE h table ["user_id"] "draft_id=?" [draftIdParam]  
  unless (usDraftId == usIdNum) $
    throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of draft_id: " ++ unpack draftIdParam

isPostAuthor :: (MonadCatch m) => MethodsHandle m  -> Text -> UserId -> ExceptT ReqError m ()
isPostAuthor h  postIdParam usIdNum = do
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  Only usPostId <- selectOneE h table ["user_id"] "post_id=?" [postIdParam]  
  unless (usPostId  == usIdNum) $
    throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of post_id: " ++ unpack postIdParam

isUserAuthorE :: (MonadCatch m) => MethodsHandle m -> UserId -> ExceptT ReqError m Author
isUserAuthorE h  usIdNum = do
  lift $ logDebug (hLog h) "Checking in DB is user author"  
  maybeAu <- selectMaybeOneE h "authors" ["author_id","author_info","user_id"] "user_id=?" [pack . show $ usIdNum] 
  case maybeAu of
    Nothing -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " isn`t author"
    Just author -> return author

isUserAuthorE_ :: (MonadCatch m) => MethodsHandle m -> UserId -> ExceptT ReqError m ()
isUserAuthorE_ h usIdNum = do
  _ <- isUserAuthorE h  usIdNum
  return ()

isNULL :: PostId -> PostIdOrNull
isNULL 0      = PostIdNull
isNULL postId = PostIdExist postId

