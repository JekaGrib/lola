{-# LANGUAGE RankNTypes #-}

module Methods.Draft where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.JSON (DraftRequest (..), checkDraftReqJson)
import Api.Request.QueryStr (GetDrafts (..), checkQStr)
import Api.Response
  ( AuthorResponse (..),
    CatResponse (..),
    DraftResponse (..),
    DraftsResponse (..),
    PicIdUrl (picIdPU),
    PostIdOrNull (..),
    TagResponse (..),
  )
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple (withTransaction)
import Error (ReqError (..))
import Logger
import Methods.Category (fromCatResp)
import Methods.Common
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenUserAuth)
import Methods.Common.DeleteMany
  ( deleteAllAboutDrafts,
    deletePicsTagsForDrafts,
    deletePicsTagsForPost,
  )
import qualified Methods.Common.DeleteMany (Handle, makeH)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourceE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Common.MakeCatResp (makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import Network.HTTP.Types (QueryText)
import Psql.Methods.Draft
import Psql.Selecty (Author (..), Draft (..), Tag (..))
import Psql.ToQuery.SelectLimit (OrderBy (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectDrafts :: DraftId -> m [Draft],
    selectUsersForDraft :: DraftId -> m [UserId],
    selectTags :: [TagId] -> m [Tag],
    selectLimDraftsForAuthor :: AuthorId -> OrderBy -> Page -> Limit -> m [Draft],
    selectPicsForDraft :: PostId -> m [PictureId],
    selectTagsForDraft :: DraftId -> m [Tag],
    selectPostsForDraft :: DraftId -> m [PostId],
    selectAuthorsForUser :: UserId -> m [Author],
    updateDbDraft :: DraftId -> UpdateDbDraft -> m (),
    updateDbPost :: PostId -> UpdateDbPost -> m (),
    updateDbPostForDraft :: DraftId -> PostId -> m (),
    insertReturnDraft :: InsertDraft -> m DraftId,
    insertManyDraftsPics :: [(DraftId, PictureId)] -> m (),
    insertManyDraftsTags :: [(DraftId, TagId)] -> m (),
    insertReturnPost :: InsertPost -> m PostId,
    insertManyPostsPics :: [(PostId, PictureId)] -> m (),
    insertManyPostsTags :: [(PostId, TagId)] -> m (),
    getDay :: m Day,
    withTransactionDB :: forall a. m a -> m a,
    hCatResp :: Methods.Common.MakeCatResp.Handle m,
    hDelMany :: Methods.Common.DeleteMany.Handle m,
    hAuth :: Methods.Common.Auth.Handle m,
    hExist :: Methods.Common.Exist.Handle m
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
        (selectLimDraftsForAuthor' conn)
        (selectPicsForDraft' conn)
        (selectTagsForDraft' conn)
        (selectPostsForDraft' conn)
        (selectAuthorsForUser' conn)
        (updateDbDraft' conn)
        (updateDbPost' conn)
        (updateDbPostForDraft' conn)
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

workWithDrafts ::
  (MonadCatch m) =>
  Handle m ->
  QueryText ->
  AppMethod ->
  ByteString ->
  ExceptT ReqError m ResponseInfo
workWithDrafts h@Handle {..} qStr meth json =
  case meth of
    ToPost -> do
      lift $ logInfo hLog "Create new draft command"
      (usId, _) <- tokenUserAuth hAuth qStr
      body <- checkDraftReqJson hExist json
      createNewDraft h usId body
    ToPostId draftId -> do
      lift $ logInfo hLog "Publish draft command"
      (usId, _) <- tokenUserAuth hAuth qStr
      isExistResourceE hExist (DraftId draftId)
      publishDraft h usId draftId
    ToGet draftId -> do
      lift $ logInfo hLog "Get draft command"
      (usId, _) <- tokenUserAuth hAuth qStr
      isExistResourceE hExist (DraftId draftId)
      getDraft h usId draftId
    ToGetAll -> do
      lift $ logInfo hLog "Get drafts command"
      (usId, _) <- tokenUserAuth hAuth qStr
      checkQStr hExist qStr >>= getDrafts h usId
    ToPut draftId -> do
      lift $ logInfo hLog "Update draft command"
      (usId, _) <- tokenUserAuth hAuth qStr
      isExistResourceE hExist (DraftId draftId)
      body <- checkDraftReqJson hExist json
      updateDraft h usId draftId body
    ToDelete draftId -> do
      lift $ logInfo hLog "Delete draft command"
      (usId, _) <- tokenUserAuth hAuth qStr
      isExistResourceE hExist (DraftId draftId)
      deleteDraft h usId draftId

createNewDraft ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  DraftRequest ->
  ExceptT ReqError m ResponseInfo
createNewDraft
  h@Handle {..}
  usId
  (DraftRequest nameParam catIdParam txtParam picId picsIds tagsIds) = do
    Author auId _ _ <- isUserAuthorE h usId
    let insDr = InsertDraft Nothing auId nameParam catIdParam txtParam picId
    draftId <- insertReturnAllDraft h picsIds tagsIds insDr
    lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " created"
    ok201Helper hConf "draft" draftId

getDraft ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  DraftId ->
  ExceptT ReqError m ResponseInfo
getDraft h@Handle {..} usId draftId = do
  resp <- selectDraftAndMakeResp h usId draftId
  lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " sending in response"
  okHelper resp

getDrafts ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  GetDrafts ->
  ExceptT ReqError m ResponseInfo
getDrafts h@Handle {..} usId (GetDrafts pageNum) = do
  Author auId _ _ <- isUserAuthorE h usId
  let orderBy = ByDraftId DESC
  drafts <- catchSelE hLog $ selectLimDraftsForAuthor auId orderBy pageNum (cDraftsLimit hConf)
  draftsResps <- mapM (makeDraftResp h usId auId) drafts
  lift $ logInfo hLog $
    "Draft_ids: " ++ show (fmap draftIdDR drafts)
      ++ " sending in response"
  okHelper $
    DraftsResponse
      { page = pageNum,
        drafts = draftsResps
      }

updateDraft ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  DraftId ->
  DraftRequest ->
  ExceptT ReqError m ResponseInfo
updateDraft
  h@Handle {..}
  usId
  draftId
  drReq@(DraftRequest nameParam catIdParam txtParam picId picsIds tagsIds) = do
    isDraftAuthor h draftId usId
    DraftInfo auResp tagResps catResp <- getDraftInfo h usId drReq
    postId <- catchOneSelectE hLog $ selectPostsForDraft draftId
    withTransactionDBE h $ do
      deletePicsTagsForDrafts hDelMany [draftId]
      let updDr = UpdateDbDraft nameParam catIdParam txtParam picId
      updateDbDraft draftId updDr
      insertManyDraftsPics (zip (repeat draftId) picsIds)
      insertManyDraftsTags (zip (repeat draftId) tagsIds)
    lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " updated"
    okHelper $
      DraftResponse
        { draftIdD = draftId,
          postIdD = isNULL postId,
          authorD = auResp,
          draftNameD = nameParam,
          draftCategoryD = catResp,
          draftTextD = txtParam,
          draftMainPicIdD = picId,
          draftMainPicUrlD = makeMyPicUrl hConf picId,
          draftTagsD = tagResps,
          draftPicsD = fmap (inPicIdUrl hConf) picsIds
        }

deleteDraft ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  DraftId ->
  ExceptT ReqError m ResponseInfo
deleteDraft h@Handle {..} usId draftId = do
  isUserAuthorE_ h usId
  isDraftAuthor h draftId usId
  withTransactionDBE h $ deleteAllAboutDrafts hDelMany [draftId]
  lift $ logInfo hLog $ "Draft_id: " ++ show draftId ++ " deleted"
  ok204Helper

publishDraft ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  DraftId ->
  ExceptT ReqError m ResponseInfo
publishDraft h@Handle {..} usId draftId = do
  DraftResponse _ draftPostId (AuthorResponse auId _ _) draftName catResp draftTxt mPicId _ picIdUrls tagResps <-
    selectDraftAndMakeResp h usId draftId
  case draftPostId of
    PostIdNull -> do
      day <- lift getDay
      postId <- withTransactionDBE h $ do
        let insPost = InsertPost auId draftName day (fromCatResp catResp) draftTxt mPicId
        postId <- insertReturnPost insPost
        insertManyPostsPics (zip (repeat postId) (fmap picIdPU picIdUrls))
        insertManyPostsTags (zip (repeat postId) (fmap tagIdTR tagResps))
        updateDbPostForDraft draftId postId
        return postId
      lift $ logInfo hLog $
        "Draft_id: " ++ show draftId
          ++ " published as post_id: "
          ++ show postId
      okPublishedPostHelper postId
    PostIdExist postId -> do
      withTransactionDBE h $ do
        let updPost = UpdateDbPost draftName (fromCatResp catResp) draftTxt mPicId
        updateDbPost postId updPost
        deletePicsTagsForPost hDelMany postId
        insertManyPostsPics (zip (repeat postId) (fmap picIdPU picIdUrls))
        insertManyPostsTags (zip (repeat postId) (fmap tagIdTR tagResps))
      lift $ logInfo hLog $
        "Draft_id: " ++ show draftId
          ++ " published as post_id: "
          ++ show postId
      okPublishedPostHelper postId

selectDraftAndMakeResp ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  DraftId ->
  ExceptT ReqError m DraftResponse
selectDraftAndMakeResp h@Handle {..} usId draftId = do
  Author auId _ _ <- isUserAuthorE h usId
  isDraftAuthor h draftId usId
  draft <- catchOneSelectE hLog $ selectDrafts draftId
  makeDraftResp h usId auId draft

makeDraftResp ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  AuthorId ->
  Draft ->
  ExceptT ReqError m DraftResponse
makeDraftResp
  Handle {..}
  usId
  auId
  (Draft drId auInfo draftPostId draftName draftCatId draftTxt mPicId) = do
    picsIds <- catchSelE hLog $ selectPicsForDraft drId
    tagS <- catchSelE hLog $ selectTagsForDraft drId
    catResp <- makeCatResp hCatResp draftCatId
    return
      DraftResponse
        { draftIdD = drId,
          postIdD = isNULL draftPostId,
          authorD = AuthorResponse auId auInfo usId,
          draftNameD = draftName,
          draftCategoryD = catResp,
          draftTextD = draftTxt,
          draftMainPicIdD = mPicId,
          draftMainPicUrlD = makeMyPicUrl hConf mPicId,
          draftTagsD = fmap inTagResp tagS,
          draftPicsD = fmap (inPicIdUrl hConf) picsIds
        }

data DraftInfo = DraftInfo AuthorResponse [TagResponse] CatResponse

getDraftInfo ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  DraftRequest ->
  ExceptT ReqError m DraftInfo
getDraftInfo h@Handle {..} usId (DraftRequest _ catIdParam _ _ _ tagsIds) = do
  Author auId auInfo _ <- isUserAuthorE h usId
  tagS <- catchSelE hLog $ selectTags tagsIds
  catResp <- makeCatResp hCatResp catIdParam
  return $ DraftInfo (AuthorResponse auId auInfo usId) (fmap inTagResp tagS) catResp

insertReturnAllDraft ::
  (MonadCatch m) =>
  Handle m ->
  [PictureId] ->
  [TagId] ->
  InsertDraft ->
  ExceptT ReqError m DraftId
insertReturnAllDraft h@Handle {..} picsIds tagsIds insDr = withTransactionDBE h $ do
  draftId <- insertReturnDraft insDr
  insertManyDraftsPics (zip (repeat draftId) picsIds)
  insertManyDraftsTags (zip (repeat draftId) tagsIds)
  return draftId

isDraftAuthor :: (MonadCatch m) => Handle m -> DraftId -> UserId -> ExceptT ReqError m ()
isDraftAuthor Handle {..} draftId usId = do
  usDraftId <- catchOneSelectE hLog $ selectUsersForDraft draftId
  unless (usDraftId == usId)
    $ throwE
    $ ForbiddenError
    $ "user_id: " ++ show usId
      ++ " is not author of draft_id: "
      ++ show draftId

isUserAuthorE :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m Author
isUserAuthorE Handle {..} usId = do
  lift $ logDebug hLog "Checking in DB is user author"
  maybeAu <- catchMaybeOneSelectE hLog $ selectAuthorsForUser usId
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
withTransactionDBE h = catchTransactionE (hLog h) . withTransactionDB h
