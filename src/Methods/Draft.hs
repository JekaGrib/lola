{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Methods.Draft where

import Api.Request.JSON (DraftRequest (..))
import Api.Response (AuthorResponse (..), CatResponse (..), DraftResponse (..), DraftsResponse (..), OkResponse (..), PicIdUrl (pic_idPU), PostIdOrNull (..), PostResponse (..), TagResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List (intercalate, zip4)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Category (fromCatResp)
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutDrafts, deletePicsTagsForDrafts, deletePicsTagsForPost)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Methods.Common.MakeCatResp (makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import Methods.Common.Selecty (Author (..), Draft (..), PostInfo (..), Tag (..))
import Oops
import Api.Request.QueryStr ( GetDrafts (..),checkQStr)
import Types
import Data.Time.Calendar ( Day)
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth,tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE,UncheckedExId(..))
import Methods.Common.ToQuery
import Network.HTTP.Types (StdMethod(..))
import TryRead (tryReadResourseId)


data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , selectDrafts :: DraftId -> m [Draft]
  , selectUsersForDraft :: DraftId -> m [UserId]
  , selectUsersForPost :: PostId -> m [UserId]
  , selectTags :: TagId -> m [Tag]
  , selectDaysForPost :: PostId -> m [Day]
  , selectLimDraftsForAuthor :: AuthorId -> OrderBy -> Page -> Limit -> m [Draft]
  , selectPicsForDraft :: PostId -> m [PictureId]
  , selectTagsForDraft :: DraftId -> m [Tag]
  , selectAuthorsForUser :: UserId -> m [Author]
  , updateDbDraft :: DraftId -> UpdateDbDraft -> m ()
  , updateDbPost :: PostId -> UpdateDbPost -> ToUpdate -> Where -> [DbValue] -> m ()
  , insertReturnDraft :: InsertDraft -> m DraftId
  , insertManyDraftsPics :: [(DraftId, PictureId)] -> m ()
  , insertManyDraftsTags :: [(DraftId, TagId)] -> m ()
  , insertReturnPost :: InsertPost -> m PostId
  , insertManyPostsPics :: [(PostId, PictureId)] -> m ()
  , insertManyPostsTags :: [(PostId, TagId)] -> m ()
  , getDay :: m Day
  , withTransactionDB :: forall a. m a -> m a
  , hCatResp :: Methods.Common.MakeCatResp.Handle m
  , hDelMany :: Methods.Common.DeleteMany.Handle m
  , hAuth :: Methods.Common.Auth.Handle m
  , hExist :: Methods.Common.Exist.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectDrafts' conn)
        (selectUsersForDraft' conn)
        (selectUsersForPost' conn)
        (selectTags' conn)
        (selectDaysForPost' conn)
        (selectLimDraftsForAuthor' conn)
        (selectPicsForDraft' conn)
        (selectTagsForDraft' conn)
        (selectPostsForDraft' conn)
        (selectAuthorsForUser' conn)
        (updateDbDraft' conn)
        (updateDbPost' conn)
        (insertReturnDraft' conn)
        (insertManyDraftsPics' conn)
        (insertManyDraftsTags' conn)
        (insertReturnPost' conn)
        (insertManyPostsPics' conn)
        (insertManyPostsTags' conn)
        getDay'
        (withTransaction conn)
        (Methods.Common.MakeCatResp.makeH conf logH)
        (Methods.Common.DeleteMany.makeH conf)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)



selectDrafts' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  select' conn $
    Select 
      ["d.draft_id", "author_info", "COALESCE (post_id, '0') AS post_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
      "drafts AS d JOIN authors AS a ON d.author_id=a.author_id" 
      wh
selectUsersForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  selectOnly' conn $
    Select 
      ["user_id"]
      "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
      wh
selectUsersForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn $
    Select 
      ["user_id"]
      "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
      wh
selectTags' conn tagIds = do
  let toWhPair tagId = WherePair "tag_id=?" (Id tagId)
  let wh = WhereOr $ map toWhPair tagIds
  select' conn $
    Select 
      ["tag_id", "tag_name"]
      "tags"
      wh
selectDaysForPost' conn postId = do
  let wh = WherePair "post_id=?" (Id postId)
  selectOnly' conn (Select ["post_create_date"] "posts" wh)
selectLimDraftsForAuthor' conn auId orderBy page limit = do
  let wh = WherePair "author_id=?" (Id postId)
  selectLimit' conn $ 
    SelectLim 
      ["drafts.draft_id", "author_info", "COALESCE (post_id, '0') AS post_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
      "drafts JOIN authors ON authors.author_id = drafts.author_id" 
      wh [] orderBy  page limit
selectPicsForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  selectOnly' conn (Select ["pic_id"] "draftspics" wh)
selectTagsForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  select' conn $ 
    Select 
      ["tags.tag_id", "tag_name"] 
      "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" 
      wh
selectPostsForDraft' conn draftId = do
  let wh = WherePair "draft_id=?" (Id draftId)
  selectOnly' conn $ 
    Select 
      ["COALESCE (post_id, '0') AS post_id"] 
      "drats" 
      wh
selectAuthorsForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  select' conn $ 
    Select 
    ["author_id", "author_info", "user_id"] 
    "authors" 
    wh
updateDbDraft' conn drId (UpdateDbDraft name catId txt picId) = do
  let set1 = SetPair "draft_name=?"        (Txt name)
  let set2 = SetPair "draft_category_id=?" (Id catId)
  let set3 = SetPair "draft_text=?"        (Txt txt)
  let set4 = SetPair "draft_main_pic_id=?" (Id picId)
  let wh = WherePair "draft_id=?"          (Id drId)
  updateInDb' conn (Update "drafts" [set1,set2,set3,set4] wh)
updateDbPost' conn postId (UpdateDbPost name catId txt picId) = do
  let set1 = SetPair "post_name=?"        (Txt name)
  let set2 = SetPair "post_category_id=?" (Id catId)
  let set3 = SetPair "post_text=?"        (Txt txt)
  let set4 = SetPair "post_main_pic_id=?" (Id picId)
  let wh = WherePair "post_id=?"          (Id postId)
  updateInDb' conn (Update "posts" [set1,set2,set3,set4] wh)

insertReturnDraft' conn (InsertDraft auId drName catId drTxt picId) = do
  let insPair1 = InsertPair "author_id"         (Id  auId)
  let insPair2 = InsertPair "draft_name"        (Txt drName)
  let insPair3 = InsertPair "draft_category_id" (Id  catId)
  let insPair4 = InsertPair "draft_text"        (Txt drTxt)
  let insPair5 = InsertPair "draft_main_pic_id" (Id  picId)
  let insPairs = [insPair1,insPair2,insPair3,insPair4,insPair5]
  insertReturn' conn (InsertRet "drafts" insPairs "draft_id")
insertManyDraftsPics' conn xs = do
  let insPair = InsertManyPair  ("draft_id", "pic_id") xs
  insertMany' conn (InsertMany "draftspics" insPair)
insertManyDraftsTags' conn xs = do
  let insPair = InsertManyPair  ("draft_id", "tag_id") xs
  insertMany' conn (InsertMany "draftstags" insPair)
insertReturnPost' conn (InsertPost auId name day catId txt picId) = do
  let insPair1 = InsertPair "author_id"        (Id  auId)
  let insPair2 = InsertPair "post_name"        (Txt name)
  let insPair2 = InsertPair "post_create_date" (Day day)
  let insPair3 = InsertPair "post_category_id" (Id  catId)
  let insPair4 = InsertPair "post_text"        (Txt txt)
  let insPair5 = InsertPair "post_main_pic_id" (Id  picId)
  let insPairs = [insPair1,insPair2,insPair3,insPair4,insPair5]
  insertReturn' conn (InsertRet "posts" insPairs "post_id")
insertManyPostsPics' conn xs = do
  let insPair = InsertManyPair  ("post_id", "pic_id") xs
  insertMany' conn (InsertMany "postspics" insPair)
insertManyPostsTags' conn xs = do
  let insPair = InsertManyPair  ("post_id", "tag_id") xs
  insertMany' conn (InsertMany "poststags" insPair)


workWithDrafts :: (MonadCatch m) => Handle m -> ReqInfo -> ExceptT ReqError m ResponseInfo
workWithDrafts h@Handle{..} (ReqInfo meth path qStr (Just json)) =
  case (meth,path) of
    (POST,["drafts"]) -> do
      lift $ logInfo (hLog h) "Create new draft command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      body <- checkDraftReqJson hExist json
      createNewDraft (hDr methH) usId body
    (POST,["drafts",draftIdTxt,"posts"]) -> do
      lift $ logInfo (hLog h) "Publish draft command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      draftId <- checkDraftResourse h draftIdTxt
      publishDraft (hDr methH) usId
    (GET,["drafts",draftIdTxt]) -> do
      lift $ logInfo (hLog h) "Get draft command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      draftId <- checkDraftResourse h draftIdTxt
      getDraft (hDr methH) usId draftId
    (GET,["drafts"]) -> do
      lift $ logInfo (hLog h) "Get drafts command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      checkQStr hExist qStr >>= getDrafts (hDr methH) usId
    (PUT,["drafts",draftIdTxt]) -> do
      lift $ logInfo (hLog h) "Update draft command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      draftId <- checkDraftResourse h draftIdTxt
      body <- checkDraftReqJson hExist json
      updateDraft (hDr methH) usIdN draftId body
    (DELETE,["drafts",draftIdTxt]) -> do
      lift $ logInfo (hLog h) "Delete draft command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      draftId <- checkDraftResourse h draftIdTxt
      deleteDraft (hDr methH) usId draftId    
    (x,y) -> throwE $ ResourseNotExistError $ "Unknown method-path combination: " ++ show (x,y)
    


createNewDraft :: (MonadCatch m) => Handle m -> UserId -> DraftRequest -> ExceptT ReqError m ResponseInfo
createNewDraft h@Handle{..} usId drReq@(DraftRequest nameParam catIdParam txtParam picId picsIds tagsIds) = do
  DraftInfo auResp@(AuthorResponse auId _ _) tagResps catResp <- getDraftInfo h usId drReq
  draftId <- withTransactionDBE h $ do
    let insDr = InsertDraft auId nameParam catIdParam txtParam picId
    insertReturnAllDraft h picsIds tagsIds insDr
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " created"
  okHelper $ DraftResponse {draft_id2 = draftId, post_id2 = PostIdNull, author2 = auResp, draft_name2 = nameParam, draft_cat2 = catResp, draft_text2 = txtParam, draft_main_pic_id2 = picId, draft_main_pic_url2 = makeMyPicUrl (hConf h) picId, draft_tags2 = tagResps, draft_pics2 = fmap (inPicIdUrl (hConf h)) picsIds}



getDraft :: (MonadCatch m) => Handle m -> UserId -> DraftId -> ExceptT ReqError m ResponseInfo
getDraft h@Handle{..} usId draftId = do
  resp <- selectDraftAndMakeResp h usId draftId
  lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " sending in response"
  okHelper resp

getDrafts :: (MonadCatch m) => Handle m -> UserId -> GetDrafts -> ExceptT ReqError m ResponseInfo
getDrafts h@Handle{..} usId (GetDrafts pageNum) = do
  Author auId _ _ <- isUserAuthorE h usId
  let orderBy = ByDraftId DESC
  drafts <- catchSelE hLog $ selectLimDraftsForAuthor auId orderBy pageNum (cDraftsLimit hConf)  
  draftsResps <- mapM makeDraftResp h drafts
  lift $ logInfo (hLog h) $ "Draft_ids: " ++ show (fmap draft_idD drafts) ++ " sending in response"
  okHelper $
    DraftsResponse
      { page9 = pageNum,
        drafts9 = draftsResps
      }

updateDraft :: (MonadCatch m) => Handle m -> UserId -> DraftId -> DraftRequest -> ExceptT ReqError m ResponseInfo
updateDraft h@Handle{..} usId draftId drReq@(DraftRequest nameParam catIdParam txtParam picId picsIds tagsIds) = do
  isDraftAuthor h draftId usId
  DraftInfo auResp tagResps catResp <- getDraftInfo h usId drReq
  postId <- catchOneSelE hLog $ selectPostsForDraft draftId
  withTransactionDBE h $ do
    deletePicsTagsForDrafts hDelMany [draftId]
    let updDr = UpdateDbDraft nameParam catIdParam txtParam picId 
    updateDbDraft conn draftId updDr
    insertManyDraftsPics (zip (repeat draftId) picsIds)
    insertManyDraftsTags (zip (repeat draftId) tagsIds)
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " updated"
  okHelper $ DraftResponse {draft_id2 = draftId, post_id2 = isNULL postId, author2 = auResp, draft_name2 = nameParam, draft_cat2 = catResp, draft_text2 = txtParam, draft_main_pic_id2 = picId, draft_main_pic_url2 = makeMyPicUrl hConf picId, draft_tags2 = tagResps, draft_pics2 = fmap (inPicIdUrl hConf ) picsIds}

deleteDraft :: (MonadCatch m) => Handle m -> UserId -> DraftId -> ExceptT ReqError m ResponseInfo
deleteDraft h@Handle{..} usId draftId = do
  isUserAuthorE_ h usId
  isDraftAuthor h draftId usId
  withTransactionDBE h $ deleteAllAboutDrafts hDelMany [draftId]
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " deleted"
  okHelper $ OkResponse {ok = True}

publishDraft :: (MonadCatch m) => Handle m -> UserId -> DraftId -> ExceptT ReqError m ResponseInfo
publishDraft h@Handle{..} usId draftId = do
  DraftResponse draftId draftPostId auResp@(AuthorResponse auId _ _) draftName catResp draftTxt mPicId mPicUrl picIdUrls tagResps <- selectDraftAndMakeResp h usId draftIdParam
  case draftPostId of
    PostIdNull -> do
      day <- lift $ getDay h
      postId <- withTransactionDBE h $ do
        let insPost = InsertPost auId draftName day (fromCatResp catResp) draftTxt mPicId
        postId <- insertReturnPost insPost
        insertManyPostsPics (zip (repeat postId) (fmap pic_idPU picIdUrls))
        insertManyPostsTags (zip (repeat postId) (fmap tag_idTR tagResps))
        return postId
      lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " published as post_id: " ++ show postId
      okHelper $ PostResponse {post_id = postId, author4 = auResp, post_name = draftName, post_create_date = day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = mPicUrl, post_pics = picIdUrls, post_tags = tagResps}
    PostIdExist postId -> do
      day <- cathOneSelE hLog $ selectDays h "posts" ["post_create_date"] "post_id=?" [Id postId]
      withTransactionDBE h $ do
        let updPost = UpdateDbPost draftName (fromCatResp catResp) draftTxt mPicId postId
        updateDbPost postId updPost
        deletePicsTagsForPost hDelMany  postId
        insertManyPostsPics (zip (repeat postId) (fmap pic_idPU picIdUrls))
        insertManyPostsTags (zip (repeat postId) (fmap tag_idTR tagResps))
      lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " published as post_id: " ++ show postId
      okHelper $ PostResponse {post_id = postId, author4 = auResp, post_name = draftName, post_create_date = day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = mPicUrl, post_pics = picIdUrls, post_tags = tagResps}

selectDraftAndMakeResp :: (MonadCatch m) => Handle m -> UserId -> DraftId -> ExceptT ReqError m DraftResponse
selectDraftAndMakeResp h@Handle{..} usId draftId = do
  Author auId _ _ <- isUserAuthorE h usId
  isDraftAuthor h draftId usId
  draft <- catchOneSelE hLog $ selectDrafts  draftId
  makeDraftResp h auId draft

makeDraftResp :: (MonadCatch m) => Handle m -> AuthorId -> Draft -> ExceptT ReqError m DraftResponse
makeDraftResp Handle{..} auId (Draft drId auInfo draftPostId draftName draftCatId draftTxt mPicId) = do
  picsIds <- catchSelE hLog $ selectPicsForDraft draftIdParam
  tagS <- catchSelE hLog $ selectTagsForDraft draftIdParam
  catResp <- makeCatResp hCatResp  draftCatId
  return DraftResponse {draft_id2 = drId, post_id2 = isNULL draftPostId, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName, draft_cat2 = catResp, draft_text2 = draftTxt, draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl (hConf h) mPicId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap (inPicIdUrl (hConf h)) picsIds}

data DraftInfo = DraftInfo AuthorResponse [TagResponse] CatResponse

getDraftInfo :: (MonadCatch m) => Handle m -> UserId -> DraftRequest -> ExceptT ReqError m DraftInfo
getDraftInfo h@Handle{..} usId (DraftRequest _ catIdParam _ picId picsIds tagsIds) = do
  Author auId auInfo usId <- isUserAuthorE h usId
  tagS <- catchSelE hLog  $ selectTags tagsIds
  catResp <- makeCatResp hCatResp catIdParam
  return $ DraftInfo (AuthorResponse auId auInfo usId) (fmap inTagResp tagS) catResp

insertReturnAllDraft :: (MonadCatch m) => Handle m -> [PictureId] -> [TagId] -> [Param] -> [DbValue] -> m DraftId
insertReturnAllDraft h@Handle{..} picsIds tagsIds insDr = withTransactionDBE h $ do
  draftId <- insertReturnDraft insDr
  insertManyDraftsPics (zip (repeat draftId) picsIds)
  insertManyDraftsTags (zip (repeat draftId) tagsIds)
  return draftId

isDraftAuthor :: (MonadCatch m) => Handle m -> DraftId -> UserId -> ExceptT ReqError m ()
isDraftAuthor Handle{..} draftId usId = do
  usDraftId <- catchOneSelE hLog $ selectUsersForDraft draftId
  unless (usDraftId == usId)
    $ throwE
    $ ForbiddenError
    $ "user_id: " ++ show usId ++ " is not author of draft_id: " ++ show draftId


isUserAuthorE :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m Author
isUserAuthorE Handle{..} usId = do
  lift $ logDebug hLog "Checking in DB is user author"
  maybeAu <- catchOneSelE hLog $ selectAuthorsForUser  usId
  case maybeAu of
    Nothing -> throwE $ ForbiddenError $ "user_id: " ++ show usId ++ " isn`t author"
    Just author -> return author

isUserAuthorE_ :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ()
isUserAuthorE_ h usId = do
  _ <- isUserAuthorE h usId
  return ()

isNULL :: PostId -> PostIdOrNull
isNULL 0 = PostIdNull
isNULL postId = PostIdExist postId


checkDraftResourse :: (MonadCatch m) => Handle m -> ResourseId -> ExceptT ReqError m DraftId
checkDraftResourse Handle{..} draftIdTxt = do
  iD <- tryReadResourseId "draft_id" draftIdTxt
  isExistResourseE hExist (DraftId iD)
  return iD

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactE (hLog h) . withTransactionDB h

