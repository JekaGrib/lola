{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Draft where

import Api.Request (DraftRequest (..))
import Api.Response (AuthorResponse (..), CatResponse (..), DraftResponse (..), DraftsResponse (..), OkResponse (..), PicIdUrl (pic_idPU), PostIdOrNull (..), PostResponse (..), TagResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List (intercalate, zip4)
import Data.Text ( pack)
import Data.Time.Calendar (Day, showGregorian)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Category (fromCatResp)
import Methods.Common
import Methods.Common.DeleteMany (deleteAllAboutDrafts, deleteDraftsPicsTags, deletePostsPicsTags)
import qualified Methods.Common.DeleteMany (Handle, makeH)
import Methods.Common.MakeCatResp (makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import Methods.Common.Select (Author (..), Draft (..), PostInfo (..), Tag (..))
import Methods.Post.LimitArg (FilterArg, SortArg)
import Oops
import ParseQueryStr (CreatePostsDraft (..), DeleteDraft (..), GetDraft (..), GetDrafts (..), PublishDraft (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectDays :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Day],
    selectNums :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Id],
    selectTags :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Tag],
    selectAuthors :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Author],
    selectPostInfos :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [PostInfo],
    selectDrafts :: Table -> [DbSelectParamKey] -> Where -> [DbValue]-> m [Draft],
    selectLimitDrafts :: Table -> OrderBy -> Page -> Limit -> [DbSelectParamKey] -> Where -> [DbValue] -> [FilterArg] -> [SortArg] -> m [Draft],
    updateInDb :: Table -> ToUpdate -> Where -> [DbValue] -> m (),
    deleteFromDb :: Table -> Where -> [DbValue] -> m (),
    isExistInDb :: Table -> Where -> DbValue -> m Bool,
    insertReturn :: Table -> DbReturnParamKey -> [DbInsertParamKey] -> [DbValue] -> m Id,
    insertMany :: Table -> [DbInsertParamKey] -> [(Id, Id)] -> m (),
    getDay :: m String,
    withTransactionDB :: forall a. m a -> m a,
    hCatResp :: Methods.Common.MakeCatResp.Handle m,
    hDelMany :: Methods.Common.DeleteMany.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectOnly' conn)
        (selectOnly' conn)
        (select' conn)
        (select' conn)
        (select' conn)
        (select' conn)
        (selectLimit' conn)
        (updateInDb' conn)
        (deleteFromDb' conn)
        (isExistInDb' conn)
        (insertReturn' conn)
        (insertMany' conn)
        getDay'
        (withTransaction conn)
        (Methods.Common.MakeCatResp.makeH conf logH)
        (Methods.Common.DeleteMany.makeH conf)

createNewDraft :: (MonadCatch m) => Handle m -> UserId -> DraftRequest -> ExceptT ReqError m ResponseInfo
createNewDraft h usIdNum drReq@(DraftRequest _ nameParam catIdParam txtParam picId picsIds tagsIds) = do
  DraftInfo auResp@(AuthorResponse auId _ _) tagResps catResp <- getDraftInfo h usIdNum drReq
  draftId <- withTransactionDBE h $ do
    let insNames = ["author_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
    let insValues = [Id auId, Txt nameParam, Id catIdParam, Txt txtParam, Id picId]
    insertReturnDraft h picsIds tagsIds insNames insValues
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " created"
  okHelper $ DraftResponse {draft_id2 = draftId, post_id2 = PostIdNull, author2 = auResp, draft_name2 = nameParam, draft_cat2 = catResp, draft_text2 = txtParam, draft_main_pic_id2 = picId, draft_main_pic_url2 = makeMyPicUrl (hConf h) picId, draft_tags2 = tagResps, draft_pics2 = fmap (inPicIdUrl (hConf h)) picsIds}

createPostsDraft :: (MonadCatch m) => Handle m -> UserId -> CreatePostsDraft -> ExceptT ReqError m ResponseInfo
createPostsDraft h usIdNum (CreatePostsDraft postIdParam) = do
  isUserAuthorE_ h usIdNum
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  let params = ["a.author_id", "author_info", "post_name", "post_category_id", "post_text", "post_main_pic_id"]
  PostInfo auId auInfo postName postCatId postTxt mPicId <- checkOneIfExistE (hLog h) (selectPostInfos h) table params "post_id=?" (Id postIdParam)
  isPostAuthor h postIdParam usIdNum
  picsIds <- checkListE (hLog h) $ selectNums h "postspics" ["pic_id"] "post_id=?" [Id postIdParam]
  tagS <- checkListE (hLog h) $ selectTags h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id", "tag_name"] "post_id=?" [Id postIdParam]
  let tagsIds = fmap tag_idT tagS
  catResp <- makeCatResp (hCatResp h) postCatId
  draftId <- withTransactionDBE h $ do
    let insNames = ["post_id", "author_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
    let insValues = [Id postIdParam, Id auId, Txt postName, Id postCatId, Txt postTxt, Id mPicId]
    insertReturnDraft h picsIds tagsIds insNames insValues
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " created for post_id: " ++ show postIdParam
  okHelper $ DraftResponse {draft_id2 = draftId, post_id2 = PostIdExist postIdParam, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = postName, draft_cat2 = catResp, draft_text2 = postTxt, draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl (hConf h) mPicId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap (inPicIdUrl (hConf h)) picsIds}

getDraft :: (MonadCatch m) => Handle m -> UserId -> GetDraft -> ExceptT ReqError m ResponseInfo
getDraft h usIdNum (GetDraft draftIdParam) = do
  resp <- selectDraftAndMakeResp h usIdNum draftIdParam
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftIdParam ++ " sending in response"
  okHelper resp

getDrafts :: (MonadCatch m) => Handle m -> UserId -> GetDrafts -> ExceptT ReqError m ResponseInfo
getDrafts h usIdNum (GetDrafts pageNum) = do
  Author auId _ _ <- isUserAuthorE h usIdNum
  let table = "drafts JOIN authors ON authors.author_id = drafts.author_id"
  let orderBy = "draft_id DESC"
  let extractParams = ["drafts.draft_id", "author_info", "COALESCE (post_id, '0') AS post_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
  let where' = "drafts.author_id = ?"
  let values = [Id auId]
  drafts <- checkListE (hLog h) $ selectLimitDrafts h table orderBy pageNum (cDraftsLimit . hConf $ h) extractParams where' values [] []
  let alldraftIdsValues = fmap (Id . draft_idD) drafts
  let allCatIdsNum = fmap draft_cat_idD drafts
  manyCatResp <- mapM (makeCatResp (hCatResp h)) allCatIdsNum
  manyDraftPicsIds <- mapM (checkListE (hLog h) . selectNums h "draftspics" ["pic_id"] "draft_id=?") $ fmap (: []) alldraftIdsValues
  tagSMany <- mapM (checkListE (hLog h) . selectTags h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id", "tag_name"] "draft_id=?") $ fmap (: []) alldraftIdsValues
  let allParams = zip4 drafts manyCatResp manyDraftPicsIds tagSMany
  lift $ logInfo (hLog h) $ "Draft_ids: " ++ show (fmap draft_idD drafts) ++ " sending in response"
  okHelper $
    DraftsResponse
      { page9 = pageNum,
        drafts9 = fmap (\(Draft draftId auInfo postId draftName _ draftText draftMainPicId, catResp, pics, tagS) -> DraftResponse {draft_id2 = draftId, post_id2 = isNULL postId, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName, draft_cat2 = catResp, draft_text2 = draftText, draft_main_pic_id2 = draftMainPicId, draft_main_pic_url2 = makeMyPicUrl (hConf h) draftMainPicId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap (inPicIdUrl (hConf h)) pics}) allParams
      }

updateDraft :: (MonadCatch m) => Handle m -> UserId -> DraftId -> DraftRequest -> ExceptT ReqError m ResponseInfo
updateDraft h usIdNum draftIdNum drReq@(DraftRequest _ nameParam catIdParam txtParam picId picsIds tagsIds) = do
  isDraftAuthor h draftIdNum usIdNum
  DraftInfo auResp tagResps catResp <- getDraftInfo h usIdNum drReq
  postId <- checkOneE (hLog h) $ selectNums h "drafts" ["COALESCE (post_id, '0') AS post_id"] "draft_id=?" [Id draftIdNum]
  withTransactionDBE h $ do
    deleteDraftsPicsTags (hDelMany h) [draftIdNum]
    updateInDb h "drafts" "draft_name=?,draft_category_id=?,draft_text=?,draft_main_pic_id=?" "draft_id=?" [Txt nameParam, Id catIdParam, Txt txtParam, Id picId, Id draftIdNum]
    insertMany h "draftspics" ["draft_id", "pic_id"] (zip (repeat draftIdNum) picsIds)
    insertMany h "draftstags" ["draft_id", "tag_id"] (zip (repeat draftIdNum) tagsIds)
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftIdNum ++ " updated"
  okHelper $ DraftResponse {draft_id2 = draftIdNum, post_id2 = isNULL postId, author2 = auResp, draft_name2 = nameParam, draft_cat2 = catResp, draft_text2 = txtParam, draft_main_pic_id2 = picId, draft_main_pic_url2 = makeMyPicUrl (hConf h) picId, draft_tags2 = tagResps, draft_pics2 = fmap (inPicIdUrl (hConf h)) picsIds}

deleteDraft :: (MonadCatch m) => Handle m -> UserId -> DeleteDraft -> ExceptT ReqError m ResponseInfo
deleteDraft h usIdNum (DeleteDraft draftIdParam) = do
  isExistInDbE h "drafts" "draft_id=?" (Id draftIdParam)
  isUserAuthorE_ h usIdNum
  isDraftAuthor h draftIdParam usIdNum
  withTransactionDBE h $ deleteAllAboutDrafts (hDelMany h) [draftIdParam]
  lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftIdParam ++ " deleted"
  okHelper $ OkResponse {ok = True}

publishDraft :: (MonadCatch m) => Handle m -> UserId -> PublishDraft -> ExceptT ReqError m ResponseInfo
publishDraft h usIdNum (PublishDraft draftIdParam) = do
  DraftResponse draftId draftPostId auResp@(AuthorResponse auId _ _) draftName catResp draftTxt mPicId mPicUrl picIdUrls tagResps <- selectDraftAndMakeResp h usIdNum draftIdParam
  case draftPostId of
    PostIdNull -> do
      day <- lift $ getDay h
      postId <- withTransactionDBE h $ do
        let insNames = ["author_id", "post_name", "post_create_date", "post_category_id", "post_text", "post_main_pic_id"]
        let insValues = [Id auId, Txt draftName, Txt (pack day), Id . fromCatResp $ catResp, Txt draftTxt, Id mPicId]
        postId <- insertReturn h "posts" "post_id" insNames insValues
        insertMany h "postspics" ["post_id", "pic_id"] (zip (repeat postId) (fmap pic_idPU picIdUrls))
        insertMany h "poststags" ["post_id", "tag_id"] (zip (repeat postId) (fmap tag_idTR tagResps))
        return postId
      lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " published as post_id: " ++ show postId
      okHelper $ PostResponse {post_id = postId, author4 = auResp, post_name = draftName, post_create_date = pack day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = mPicUrl, post_pics = picIdUrls, post_tags = tagResps}
    PostIdExist postId -> do
      day <- checkOneE (hLog h) $ selectDays h "posts" ["post_create_date"] "post_id=?" [Id postId]
      withTransactionDBE h $ do
        updateInDb h "posts" "post_name=?,post_category_id=?,post_text=?,post_main_pic_id=?" "post_id=?" [Txt draftName, Id . fromCatResp $ catResp, Txt draftTxt, Id mPicId, Id postId]
        deletePostsPicsTags (hDelMany h) [postId]
        insertMany h "postspics" ["post_id", "pic_id"] (zip (repeat postId) (fmap pic_idPU picIdUrls))
        insertMany h "poststags" ["post_id", "tag_id"] (zip (repeat postId) (fmap tag_idTR tagResps))
      lift $ logInfo (hLog h) $ "Draft_id: " ++ show draftId ++ " published as post_id: " ++ show postId
      okHelper $ PostResponse {post_id = postId, author4 = auResp, post_name = draftName, post_create_date = pack . showGregorian $ day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = mPicUrl, post_pics = picIdUrls, post_tags = tagResps}

selectDraftAndMakeResp :: (MonadCatch m) => Handle m -> UserId -> DraftId -> ExceptT ReqError m DraftResponse
selectDraftAndMakeResp h usIdNum draftIdParam = do
  let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
  let params = ["d.draft_id", "author_info", "COALESCE (post_id, '0') AS post_id", "draft_name", "draft_category_id", "draft_text", "draft_main_pic_id"]
  Draft drId auInfo draftPostId draftName draftCatId draftTxt mPicId <- checkOneIfExistE (hLog h) (selectDrafts h) table params "draft_id=?" (Id draftIdParam)
  Author auId _ _ <- isUserAuthorE h usIdNum
  isDraftAuthor h draftIdParam usIdNum
  picsIds <- checkListE (hLog h) $ selectNums h "draftspics" ["pic_id"] "draft_id=?" [Id draftIdParam]
  tagS <- checkListE (hLog h) $ selectTags h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id", "tag_name"] "draft_id=?" [Id draftIdParam]
  catResp <- makeCatResp (hCatResp h) draftCatId
  return DraftResponse {draft_id2 = drId, post_id2 = isNULL draftPostId, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName, draft_cat2 = catResp, draft_text2 = draftTxt, draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl (hConf h) mPicId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap (inPicIdUrl (hConf h)) picsIds}

data DraftInfo = DraftInfo AuthorResponse [TagResponse] CatResponse

getDraftInfo :: (MonadCatch m) => Handle m -> UserId -> DraftRequest -> ExceptT ReqError m DraftInfo
getDraftInfo h usIdNum (DraftRequest _ _ catIdParam _ picId picsIds tagsIds) = do
  isExistInDbE h "categories"  "category_id=?" (Id catIdParam)
  mapM_ (isExistInDbE h "tags"  "tag_id=?") $ fmap Id tagsIds
  mapM_ (isExistInDbE h "pics"  "pic_id=?") $ fmap Id (picId : picsIds)
  (Author auId auInfo usId) <- isUserAuthorE h usIdNum
  let where' = intercalate " OR " . fmap (const "tag_id=?") $ tagsIds
  tagS <- checkListE (hLog h) $ selectTags h "tags" ["tag_id", "tag_name"] where' (fmap Id tagsIds)
  catResp <- makeCatResp (hCatResp h) catIdParam
  return $ DraftInfo (AuthorResponse auId auInfo usId) (fmap inTagResp tagS) catResp

insertReturnDraft :: (MonadCatch m) => Handle m -> [PictureId] -> [TagId] -> [Param] -> [DbValue] -> m DraftId
insertReturnDraft h picsIds tagsIds insNames insValues = do
  draftId <- insertReturn h "drafts" "draft_id" insNames insValues
  insertMany h "draftspics" ["draft_id", "pic_id"] (zip (repeat draftId) picsIds)
  insertMany h "draftstags" ["draft_id", "tag_id"] (zip (repeat draftId) tagsIds)
  return draftId

isDraftAuthor :: (MonadCatch m) => Handle m -> DraftId -> UserId -> ExceptT ReqError m ()
isDraftAuthor h draftIdNum usIdNum = do
  let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
  usDraftId <- checkOneE (hLog h) $ selectNums h table ["user_id"] "draft_id=?" [Id draftIdNum]
  unless (usDraftId == usIdNum)
    $ throwE
    $ SimpleError
    $ "user_id: " ++ show usIdNum ++ " is not author of draft_id: " ++ show draftIdNum

isPostAuthor :: (MonadCatch m) => Handle m -> PostId -> UserId -> ExceptT ReqError m ()
isPostAuthor h postIdParam usIdNum = do
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  usPostId <- checkOneE (hLog h) $ selectNums h table ["user_id"] "post_id=?" [Id postIdParam]
  unless (usPostId == usIdNum)
    $ throwE
    $ SimpleError
    $ "user_id: " ++ show usIdNum ++ " is not author of post_id: " ++ show postIdParam

isUserAuthorE :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m Author
isUserAuthorE h usIdNum = do
  lift $ logDebug (hLog h) "Checking in DB is user author"
  maybeAu <- checkMaybeOneE (hLog h) $ selectAuthors h "authors" ["author_id", "author_info", "user_id"] "user_id=?" [Id usIdNum]
  case maybeAu of
    Nothing -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " isn`t author"
    Just author -> return author

isUserAuthorE_ :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ()
isUserAuthorE_ h usIdNum = do
  _ <- isUserAuthorE h usIdNum
  return ()

isNULL :: PostId -> PostIdOrNull
isNULL 0 = PostIdNull
isNULL postId = PostIdExist postId

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h

isExistInDbE :: (MonadCatch m) => Handle m -> Table -> Where -> DbValue -> ExceptT ReqError m ()
isExistInDbE h = checkIsExistE (hLog h) (isExistInDb h)
