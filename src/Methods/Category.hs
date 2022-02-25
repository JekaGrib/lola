{-# LANGUAGE OverloadedStrings #-}
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
    updateInDb :: Table -> ToUpdate -> Where -> [DbValue] -> m (),
    deleteFromDb :: Table -> Where -> [DbValue] -> m (),
    isExistInDb :: Table -> Where -> DbValue -> m Bool,
    insertReturn :: Table -> DbReturnParamKey -> [DbInsertParamKey] -> [DbValue] -> m Id,
    withTransactionDB :: forall a. m a -> m a,
    hCatResp :: Methods.Common.MakeCatResp.Handle m
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (updateInDb' conn)
        (deleteFromDb' conn)
        (isExistInDb' conn)
        (insertReturn' conn)
        (withTransaction conn)
        (Methods.Common.MakeCatResp.makeH conf logH)

createCategory :: (MonadCatch m) => Handle m -> CreateCategory -> ExceptT ReqError m ResponseInfo
createCategory h (CreateCategory catNameParam) = do
  catId <- insertReturnE h "categories" "category_id" ["category_name"] [Txt catNameParam]
  lift $ logInfo (hLog h) $ "Category_id: " ++ show catId ++ " created"
  okHelper $ CatResponse {cat_id = catId, cat_name = catNameParam, one_level_sub_cats = []}

createSubCategory :: (MonadCatch m) => Handle m -> CreateSubCategory -> ExceptT ReqError m ResponseInfo
createSubCategory h (CreateSubCategory catNameParam superCatIdParam) = do
  isExistInDbE h "categories"  "category_id=?" (Id superCatIdParam)
  catId <- insertReturnE h "categories" "category_id" ["category_name", "super_category_id"] [Txt catNameParam, Id superCatIdParam]
  catResp <- makeCatResp (hCatResp h) catId
  lift $ logInfo (hLog h) $ "Sub_Category_id: " ++ show catId ++ " created"
  okHelper catResp

getCategory :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m ResponseInfo
getCategory h catId = do
  isExistInDbE h "categories"  "category_id=?" (Id catId)
  catResp <- makeCatResp (hCatResp h) catId
  lift $ logInfo (hLog h) $ "Category_id: " ++ show catId ++ " sending in response"
  okHelper catResp

updateCategory :: (MonadCatch m) => Handle m -> UpdateCategory -> ExceptT ReqError m ResponseInfo
updateCategory h (UpdateCategory catIdParam catNameParam maybeSuperCatIdParam) = do
  isExistInDbE h "categories"  "category_id=?" (Id catIdParam)
  case maybeSuperCatIdParam of
    Just superCatIdParam -> do
      isExistInDbE h "categories"  "category_id=?" (Id superCatIdParam)
      checkRelationCats h catIdParam superCatIdParam
      updateInDbE h "categories" "category_name=?,super_category_id=?" "category_id=?" [Txt catNameParam, Id superCatIdParam, Id catIdParam]
    Nothing -> do
      updateInDbE h "categories" "category_name=?" "category_id=?" [Txt catNameParam, Id catIdParam]
  catResp <- makeCatResp (hCatResp h) catIdParam
  lift $ logInfo (hLog h) $ "Category_id: " ++ show catIdParam ++ " updated."
  okHelper catResp

deleteCategory :: (MonadCatch m) => Handle m -> DeleteCategory -> ExceptT ReqError m ResponseInfo
deleteCategory h (DeleteCategory catIdParam) = do
  isExistInDbE h "categories"  "category_id=?" (Id catIdParam)
  allSubCats <- findAllSubCats h catIdParam
  let values = fmap Id (cDefCatId (hConf h) : allSubCats)
  let where' = intercalate " OR " . fmap (const "post_category_id=?") $ allSubCats
  let where'' = intercalate " OR " . fmap (const "draft_category_id=?") $ allSubCats
  let updatePos = updateInDb h "posts" "post_category_id=?" where' values
  let updateDr = updateInDb h "drafts" "draft_category_id=?" where'' values
  let where''' = intercalate " OR " . fmap (const "category_id=?") $ allSubCats
  let deleteCat = deleteFromDb h "categories" where''' (fmap Id allSubCats)
  withTransactionDBE h (updatePos >> updateDr >> deleteCat)
  lift $ logInfo (hLog h) $ "Category_id: " ++ show catIdParam ++ " deleted."
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
findAllSubCats h catId = do
  catsIds <- findOneLevelSubCats (hCatResp h) catId
  case catsIds of
    [] -> return [catId]
    _ -> do
      subCatsIds <- mapM (findAllSubCats h) catsIds
      return $ catId : Prelude.concat subCatsIds

fromCatResp :: CatResponse -> CategoryId
fromCatResp (SubCatResponse a _ _ _) = a
fromCatResp (CatResponse a _ _) = a

updateInDbE :: (MonadCatch m) => Handle m -> Table -> Set -> Where -> [DbValue] -> ExceptT ReqError m ()
updateInDbE h t s w values = checkUpdE (hLog h) $ updateInDb h t s w values

isExistInDbE :: (MonadCatch m) => Handle m -> Table -> Where -> DbValue -> ExceptT ReqError m ()
isExistInDbE h = checkIsExistE (hLog h) (isExistInDb h)

insertReturnE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> [DbValue] -> ExceptT ReqError m Id
insertReturnE h = checkInsRetE (hLog h) (insertReturn h)

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h
