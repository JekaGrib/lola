{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Category where

import Api.Response (CatResponse (..), OkResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.List (intercalate)
import Database.PostgreSQL.Simple (withTransaction)
import Logger
import Methods.Common
import Methods.Common.MakeCatResp (findOneLevelSubCats, makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import Oops
import ParseQueryStr (CreateCategory (..), CreateSubCategory (..), DeleteCategory (..), UpdateCategory (..))
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    updateDbCat :: CatName -> CategoryId -> m (),
    updateDbSubCat :: CatName -> SuperCatId -> CatId -> m (),
    updateDbCatsForPosts :: CategoryId -> [CategoryId] -> m (),
    updateDbCatsForDrafts :: CategoryId -> [CategoryId] -> m (),
    deleteDbCats :: [CategoryId] -> m (),
    isExistCat :: CategoryId -> m Bool,
    insertRetCat :: CatName -> m CategoryId,
    insertRetSubCat :: CatName -> SuperCatId -> m CategoryId,
    withTransactionDB :: forall a. m a -> m a,
    hCatResp :: Methods.Common.MakeCatResp.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (updateDbCat' conn)
        (updateDbSubCat' conn)
        (updateDbCatsForPosts' conn)
        (updateDbCatsForDrafts' conn)
        (deleteDbCats' conn)
        (isExistCat' conn)
        (insertRetCat' conn)
        (insertRetSubCat' conn)
        (withTransaction conn)
        (Methods.Common.MakeCatResp.makeH conf logH)

updateDbCat' conn catName catId = do
  let set = SetPair "category_name=?" (Txt catName)
  let wh = WherePair "category_id=?" (Id catId)
  updateInDb' conn (Update "categories" [set] wh)
updateDbSubCat' conn catName superCatId catId = do
  let set1 = SetPair "category_name=?" (Txt catName)
  let set2 = SetPair "super_category_id=?" (Id superCatId)
  let wh = WherePair "category_id=?" (Id catId)
  updateInDb' conn (Update "categories" [set1,set2] wh)
updateDbCatsForPosts' conn newCatId catIds = do
  let set = SetPair "category_id=?" (Id newCatId)
  let toWhPair catId = WherePair "category_id=?" (Id catId)
  let wh = WhereOr $ map toWhPair catIds
  updateInDb' conn (Update "posts" [set] wh)
updateDbCatsForDrafts' conn newCatId catIds = do
  let set = SetPair "category_id=?" (Id newCatId)
  let toWhPair catId = WherePair "category_id=?" (Id catId)
  let wh = WhereOr $ map toWhPair catIds
  updateInDb' conn (Update "drafts" [set] wh)
deleteDbCats' conn catIds = do
  let toWhPair catId = WherePair "category_id=?" (Id catId)
  let wh = WhereOr $ map toWhPair catIds
  deleteFromDb' conn (Delete "categories" wh)
isExistCat' conn catId = do
  let wh = WherePair "category_id=?" (Id catId)
  isExistInDb' conn (Exists "categories" wh)
insertReturnCat' conn catName = do
  let insPair = InsertPair "cat_name" (Txt catName)
  insertReturn' conn (InsertRet "categories" [insPair] "category_id")
insertReturnSubCat' conn catName = do
  let insPair = InsertPair "cat_name" (Txt catName)
  insertReturn' conn (InsertRet "categories" [insPair] "category_id")

createCategory :: (MonadCatch m) => Handle m -> CreateCategory -> ExceptT ReqError m ResponseInfo
createCategory Handle{..} (CreateCategory catNameParam) = do
  catId <- catchInsRetE hLog $ insertReturnCat catNameParam
  lift $ logInfo hLog $ "Category_id: " ++ show catId ++ " created"
  okHelper $ CatResponse {cat_id = catId, cat_name = catNameParam, one_level_sub_cats = []}

createCategory Handle{..} (CreateSubCategory catNameParam superCatIdParam) = do
  let logpair = ("category_id", superCatIdParam)
  catchExistsE hLog logpair $ isExistCat superCatIdParam
  catId <- catchInsRetE hLog $ insertReturnSubCat catNameParam superCatIdParam
  catResp <- makeCatResp (hCatResp h) catId
  lift $ logInfo (hLog h) $ "Sub_Category_id: " ++ show catId ++ " created"
  okHelper catResp

getCategory :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m ResponseInfo
getCategory Handle{..} catId = do
  let logpair = ("category_id", catId)
  catchExistsE hLog logpair $ isExistCat catId
  catResp <- makeCatResp hCatResp catId
  lift $ logInfo hLog $ "Category_id: " ++ show catId ++ " sending in response"
  okHelper catResp

updateCategory :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m ResponseInfo
updateCategory h@Handle{..} catId (UpdateCategory catIdParam catNameParam maybeSuperCatIdParam) = do
  let logpair = ("category_id", catIdParam)
  catchExistsE hLog logpair $ isExistCat catIdParam
  case maybeSuperCatIdParam of
    Just superCatIdParam -> do
      let logpair = ("category_id", superCatIdParam)
      catchExistsE hLog logpair $ isExistCat superCatIdParam
      checkRelationCats h catIdParam superCatIdParam
      catchUpdE hLog $ updateDbSubCat catNameParam superCatIdParam catIdParam
    Nothing -> do
      catchUpdE hLog $ updateDbCat catNameParam superCatIdParam catIdParam
  catResp <- makeCatResp hCatResp catIdParam
  lift $ logInfo (hLog h) $ "Category_id: " ++ show catIdParam ++ " updated."
  okHelper catResp

deleteCategory :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m ResponseInfo
deleteCategory h@Handle{..} catId = do
  let logpair = ("category_id", catId)
  catchExistsE hLog logpair $ isExistCat catId
  allSubCats <- findAllSubCats h catId
  let allCats = catId : allSubCats
  let updatePos = catchUpdE hLog $ updateDbCatsForPosts (cDefCatId hConf) allCats
  let updateDr = catchUpdE hLog $ updateDbCatsForDrafts (cDefCatId hConf) allCats
  let deleteCat = deleteDbCats allCats
  withTransactionDBE h (updatePos >> updateDr >> deleteCat)
  lift $ logInfo (hLog h) $ "Category_id: " ++ show catId ++ " deleted."
  okHelper $ OkResponse {ok = True}

checkRelationCats :: (MonadCatch m) => Handle m -> CategoryId -> CategoryId -> ExceptT ReqError m ()
checkRelationCats h catId superCatId
  | catId == superCatId = throwE $ SimpleError $ "super_category_id: " ++ show superCatId ++ " equal to category_id."
  | otherwise = do
    allSubCats <- findAllSubCats h catId
    when (superCatId `elem` allSubCats)
      $ throwE
      $ SimpleError
      $ "super_category_id: " ++ show superCatId ++ " is subCategory of category_id: " ++ show catId

findAllSubCats :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m [CategoryId]
findAllSubCats h@Handle{..} catId = do
  catsIds <- findOneLevelSubCats hCatResp catId
  case catsIds of
    [] -> return []
    _ -> do
      subCatsIds <- mapM (findAllSubCats h) catsIds
      return $ catsIds ++ Prelude.concat subCatsIds

fromCatResp :: CatResponse -> CategoryId
fromCatResp (SubCatResponse a _ _ _) = a
fromCatResp (CatResponse a _ _) = a


withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h
