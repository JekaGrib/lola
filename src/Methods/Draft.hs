{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Draft where

import Api.Request.JSON (DraftRequest (..),checkDraftReqJson)
import Api.Response (AuthorResponse (..), CatResponse (..), DraftResponse (..), DraftsResponse (..), PicIdUrl (pic_idPU), PostIdOrNull (..), PostResponse (..), TagResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Category (fromCatResp)
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutDrafts, deletePicsTagsForDrafts, deletePicsTagsForPost)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Methods.Common.MakeCatResp (makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import Psql.Selecty (Author (..), Draft (..), Tag (..))
import Oops (ReqError(..))
import Api.Request.QueryStr ( GetDrafts (..),checkQStr)
import Types
import Data.Time.Calendar ( Day)
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId(..))
import Network.HTTP.Types (QueryText)
import Api.Request.EndPoint (AppMethod(..))
import Data.ByteString (ByteString)
import Psql.Methods.Draft
import Psql.ToQuery.SelectLimit (OrderBy(..))


data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , selectDrafts :: DraftId -> m [Draft]
  , selectUsersForDraft :: DraftId -> m [UserId]
  , selectTags :: [TagId] -> m [Tag]
  , selectDaysForPost :: PostId -> m [Day]
  , selectLimDraftsForAuthor :: AuthorId -> OrderBy -> Page -> Limit -> m [Draft]
  , selectPicsForDraft :: PostId -> m [PictureId]
  , selectTagsForDraft :: DraftId -> m [Tag]
  , selectPostsForDraft :: DraftId -> m [PostId]
  , selectAuthorsForUser :: UserId -> m [Author]
  , updateDbDraft :: DraftId -> UpdateDbDraft -> m ()
  , updateDbPost :: PostId -> UpdateDbPost -> m ()
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





workWithDrafts :: (MonadCatch m) => Handle m -> QueryText -> AppMethod -> ByteString -> ExceptT ReqError m ResponseInfo
workWithDrafts h@Handle{..} qStr meth json =
  case meth of
    ToPost -> do
      lift $ logInfo hLog "Create new draft command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      body <- checkDraftReqJson hExist json
      createNewDraft h usId body
    ToPostId draftId -> do
      lift $ logInfo hLog "Publish draft command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      isExistResourseE hExist (DraftId draftId)
      publishDraft h usId draftId
    ToGet draftId -> do
      lift $ logInfo hLog "Get draft command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      isExistResourseE hExist (DraftId draftId)
      getDraft h usId draftId
    ToGetAll -> do
      lift $ logInfo hLog "Get drafts command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      checkQStr hExist qStr >>= getDrafts h usId
    ToPut draftId -> do
      lift $ logInfo hLog  "Update draft command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      isExistResourseE hExist (DraftId draftId)
      body <- checkDraftReqJson hExist json
      updateDraft h usId draftId body
    ToDelete draftId -> do
      lift $ logInfo hLog "Delete draft command"
      (usId, _) <- tokenUserAuth hAuth  qStr
      isExistResourseE hExist (DraftId draftId)
      deleteDraft h usId draftId    
    


createNewDraft :: (MonadCatch m) => Handle m -> UserId -> DraftRequest -> ExceptT ReqError m ResponseInfo
createNewDraft h@Handle{..} usId drReq@(DraftRequest nameParam catIdParam txtParam picId picsIds tagsIds) = do
  DraftInfo auResp@(AuthorResponse auId _ _) tagResps catResp <- getDraftInfo h usId drReq
  let insDr = InsertDraft Nothing auId nameParam catIdParam txtParam picId
  draftId <- insertReturnAllDraft h picsIds tagsIds insDr    
  lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " created"
  okHelper $ DraftResponse {draft_id2 = draftId, post_id2 = PostIdNull, author2 = auResp, draft_name2 = nameParam, draft_cat2 = catResp, draft_text2 = txtParam, draft_main_pic_id2 = picId, draft_main_pic_url2 = makeMyPicUrl hConf picId, draft_tags2 = tagResps, draft_pics2 = fmap (inPicIdUrl hConf) picsIds}



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
  draftsResps <- mapM (makeDraftResp h usId auId) drafts
  lift $ logInfo hLog $ "Draft_ids: " ++ show (fmap draft_idD drafts) ++ " sending in response"
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
    updateDbDraft draftId updDr
    insertManyDraftsPics (zip (repeat draftId) picsIds)
    insertManyDraftsTags (zip (repeat draftId) tagsIds)
  lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " updated"
  okHelper $ DraftResponse {draft_id2 = draftId, post_id2 = isNULL postId, author2 = auResp, draft_name2 = nameParam, draft_cat2 = catResp, draft_text2 = txtParam, draft_main_pic_id2 = picId, draft_main_pic_url2 = makeMyPicUrl hConf picId, draft_tags2 = tagResps, draft_pics2 = fmap (inPicIdUrl hConf ) picsIds}

deleteDraft :: (MonadCatch m) => Handle m -> UserId -> DraftId -> ExceptT ReqError m ResponseInfo
deleteDraft h@Handle{..} usId draftId = do
  isUserAuthorE_ h usId
  isDraftAuthor h draftId usId
  withTransactionDBE h $ deleteAllAboutDrafts hDelMany [draftId]
  lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " deleted"
  ok204Helper

publishDraft :: (MonadCatch m) => Handle m -> UserId -> DraftId -> ExceptT ReqError m ResponseInfo
publishDraft h@Handle{..} usId draftId = do
  DraftResponse _ draftPostId auResp@(AuthorResponse auId _ _) draftName catResp draftTxt mPicId mPicUrl picIdUrls tagResps <- selectDraftAndMakeResp h usId draftId
  case draftPostId of
    PostIdNull -> do
      day <- lift getDay
      postId <- withTransactionDBE h $ do
        let insPost = InsertPost auId draftName day (fromCatResp catResp) draftTxt mPicId
        postId <- insertReturnPost insPost
        insertManyPostsPics (zip (repeat postId) (fmap pic_idPU picIdUrls))
        insertManyPostsTags (zip (repeat postId) (fmap tag_idTR tagResps))
        return postId
      lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " published as post_id: " ++ show postId
      okHelper $ PostResponse {post_id = postId, author4 = auResp, post_name = draftName, post_create_date = day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = mPicUrl, post_pics = picIdUrls, post_tags = tagResps}
    PostIdExist postId -> do
      day <- catchOneSelE hLog $ selectDaysForPost postId
      withTransactionDBE h $ do
        let updPost = UpdateDbPost draftName (fromCatResp catResp) draftTxt mPicId 
        updateDbPost postId updPost
        deletePicsTagsForPost hDelMany  postId
        insertManyPostsPics (zip (repeat postId) (fmap pic_idPU picIdUrls))
        insertManyPostsTags (zip (repeat postId) (fmap tag_idTR tagResps))
      lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " published as post_id: " ++ show postId
      okHelper $ PostResponse {post_id = postId, author4 = auResp, post_name = draftName, post_create_date = day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = mPicUrl, post_pics = picIdUrls, post_tags = tagResps}

selectDraftAndMakeResp :: (MonadCatch m) => Handle m -> UserId -> DraftId -> ExceptT ReqError m DraftResponse
selectDraftAndMakeResp h@Handle{..} usId draftId = do
  Author auId _ _ <- isUserAuthorE h usId
  isDraftAuthor h draftId usId
  draft <- catchOneSelE hLog $ selectDrafts  draftId
  makeDraftResp h usId auId draft

makeDraftResp :: (MonadCatch m) => Handle m -> UserId -> AuthorId -> Draft -> ExceptT ReqError m DraftResponse
makeDraftResp Handle{..} usId auId (Draft drId auInfo draftPostId draftName draftCatId draftTxt mPicId) = do
  picsIds <- catchSelE hLog $ selectPicsForDraft drId
  tagS <- catchSelE hLog $ selectTagsForDraft drId
  catResp <- makeCatResp hCatResp  draftCatId
  return DraftResponse {draft_id2 = drId, post_id2 = isNULL draftPostId, author2 = AuthorResponse auId auInfo usId, draft_name2 = draftName, draft_cat2 = catResp, draft_text2 = draftTxt, draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl hConf mPicId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap (inPicIdUrl hConf) picsIds}

data DraftInfo = DraftInfo AuthorResponse [TagResponse] CatResponse

getDraftInfo :: (MonadCatch m) => Handle m -> UserId -> DraftRequest -> ExceptT ReqError m DraftInfo
getDraftInfo h@Handle{..} usId (DraftRequest _ catIdParam _ _ _ tagsIds) = do
  Author auId auInfo _ <- isUserAuthorE h usId
  tagS <- catchSelE hLog  $ selectTags tagsIds
  catResp <- makeCatResp hCatResp catIdParam
  return $ DraftInfo (AuthorResponse auId auInfo usId) (fmap inTagResp tagS) catResp

insertReturnAllDraft :: (MonadCatch m) => Handle m -> [PictureId] -> [TagId] -> InsertDraft -> ExceptT ReqError m DraftId
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
  maybeAu <- catchMaybeOneSelE hLog $ selectAuthorsForUser  usId
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



withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactE (hLog h) . withTransactionDB h

