{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

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
import Api.Request.QueryStr (CreateCategory (..), UpdateCategory (..),checkQStr)
import Types
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
  , updateDbCat :: CatName -> CategoryId -> m ()
  , updateDbSubCat :: CatName -> SuperCatId -> CategoryId -> m ()
  , updateDbCatsForPosts :: CategoryId -> [CategoryId] -> m ()
  , updateDbCatsForDrafts :: CategoryId -> [CategoryId] -> m ()
  , deleteDbCats :: [CategoryId] -> m ()
  , insertReturnCat :: CatName -> m CategoryId
  , insertReturnSubCat :: CatName -> SuperCatId -> m CategoryId
  , withTransactionDB :: forall a. m a -> m a
  , hCatResp :: Methods.Common.MakeCatResp.Handle m
  , hAuth :: Methods.Common.Auth.Handle m
  , hExist :: Methods.Common.Exist.Handle m
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
        (insertReturnCat' conn)
        (insertReturnSubCat' conn)
        (withTransaction conn)
        (Methods.Common.MakeCatResp.makeH conf logH)
        (Methods.Common.Auth.makeH conf logH)
        (Methods.Common.Exist.makeH conf)

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
insertReturnCat' conn catName = do
  let insPair = InsertPair "cat_name" (Txt catName)
  insertReturn' conn (InsertRet "categories" [insPair] "category_id")
insertReturnSubCat' conn catName superCatId = do
  let insPair1 = InsertPair "cat_name" (Txt catName)
  let insPair2 = InsertPair "super_category_id" (Id superCatId)
  insertReturn' conn (InsertRet "categories" [insPair1,insPair2] "category_id")

workWithCats :: (MonadCatch m) => Handle m -> ReqInfo -> ExceptT ReqError m ResponseInfo
workWithCats h@Handle{..} (ReqInfo meth path qStr _) = 
  case (meth,path) of
    (POST,["categories"]) -> do
      lift $ logInfo hLog "Create category command"
      tokenAdminAuth hAuth qStr
      checkQStr hExist qStr >>= createCategory h
    (GET,["categories",catIdTxt]) -> do
      lift $ logInfo hLog "Get category command"
      catId <- checkCatResourse h catIdTxt
      getCategory h catId
    (PUT,["categories",catIdTxt]) -> do
      lift $ logInfo hLog "Update category command"
      tokenAdminAuth hAuth qStr
      catId <- checkCatResourse h catIdTxt
      checkQStr hExist qStr >>= updateCategory h catId
    (DELETE,["categories",catIdTxt]) -> do
      lift $ logInfo hLog "Delete category command"
      tokenAdminAuth hAuth qStr
      catId <- checkCatResourse h catIdTxt
      deleteCategory h catId
    (x,y) -> throwE $ ResourseNotExistError $ "Unknown method-path combination: " ++ show (x,y)
    

createCategory :: (MonadCatch m) => Handle m -> CreateCategory -> ExceptT ReqError m ResponseInfo
createCategory Handle{..} (CreateCategory catNameParam Nothing) = do
  catId <- catchInsRetE hLog $ insertReturnCat catNameParam
  lift $ logInfo hLog $ "Category_id: " ++ show catId ++ " created"
  okHelper $ CatResponse {cat_id = catId, cat_name = catNameParam, one_level_sub_cats = []}
createCategory Handle{..} (CreateCategory catNameParam (Just superCatIdParam)) = do
  catId <- catchInsRetE hLog $ insertReturnSubCat catNameParam superCatIdParam
  catResp <- makeCatResp hCatResp catId
  lift $ logInfo hLog $ "Sub_Category_id: " ++ show catId ++ " created"
  okHelper catResp

getCategory :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m ResponseInfo
getCategory Handle{..} catId = do
  catResp <- makeCatResp hCatResp catId
  lift $ logInfo hLog $ "Category_id: " ++ show catId ++ " sending in response"
  okHelper catResp

updateCategory :: (MonadCatch m) => Handle m -> CategoryId -> UpdateCategory -> ExceptT ReqError m ResponseInfo
updateCategory h@Handle{..} catId (UpdateCategory catNameParam maybeSuperCatIdParam) = do
  case maybeSuperCatIdParam of
    Just superCatIdParam -> do
      checkRelationCats h catId superCatIdParam
      catchUpdE hLog $ updateDbSubCat catNameParam superCatIdParam catId
    Nothing -> do
      catchUpdE hLog $ updateDbCat catNameParam catId
  catResp <- makeCatResp hCatResp catId
  lift $ logInfo hLog $ "Category_id: " ++ show catId ++ " updated."
  okHelper catResp

deleteCategory :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m ResponseInfo
deleteCategory h@Handle{..} catId = do
  allSubCats <- findAllSubCats h catId
  let allCats = catId : allSubCats
  let updatePos = updateDbCatsForPosts (cDefCatId hConf) allCats
  let updateDr = updateDbCatsForDrafts (cDefCatId hConf) allCats
  let deleteCat = deleteDbCats allCats
  withTransactionDBE h (updatePos >> updateDr >> deleteCat)
  lift $ logInfo hLog $ "Category_id: " ++ show catId ++ " deleted."
  okHelper $ OkResponse {ok = True}

checkRelationCats :: (MonadCatch m) => Handle m -> CategoryId -> CategoryId -> ExceptT ReqError m ()
checkRelationCats h catId superCatId
  | catId == superCatId = throwE $ BadReqError $ "super_category_id: " ++ show superCatId ++ " equal to category_id."
  | otherwise = do
    allSubCats <- findAllSubCats h catId
    when (superCatId `elem` allSubCats)
      $ throwE
      $ BadReqError
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

checkCatResourse :: (MonadCatch m) => Handle m -> ResourseId -> ExceptT ReqError m CategoryId
checkCatResourse Handle{..} catIdTxt = do
  iD <- tryReadResourseId "category_id" catIdTxt
  isExistResourseE hExist (CategoryId iD)
  return iD

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactE (hLog h) . withTransactionDB h
