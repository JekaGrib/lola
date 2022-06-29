{-# LANGUAGE RankNTypes #-}

module Methods.Category where

import Api.Request.EndPoint (AppMethod (..))
import Api.Request.QueryStr (CreateCategory (..), UpdateCategory (..), checkQStr)
import Api.Response (CatResponse (..), SubCatResponse (..), SuperCatResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Database.PostgreSQL.Simple (withTransaction)
import Error (ReqError (..))
import Logger
import Methods.Common
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourceE)
import Methods.Common.Exist.UncheckedExId (UncheckedExId (..))
import Methods.Common.MakeCatResp (findOneLevelSubCats, makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle, makeH)
import Network.HTTP.Types (QueryText)
import Psql.Methods.Category
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    updateDbCat :: CatName -> CategoryId -> m (),
    updateDbSubCat :: CatName -> SuperCatId -> CategoryId -> m (),
    updateDbCatsForPosts :: CategoryId -> [CategoryId] -> m (),
    updateDbCatsForDrafts :: CategoryId -> [CategoryId] -> m (),
    deleteDbCats :: [CategoryId] -> m (),
    insertReturnCat :: CatName -> m CategoryId,
    insertReturnSubCat :: CatName -> SuperCatId -> m CategoryId,
    withTransactionDB :: forall a. m a -> m a,
    hCatResp :: Methods.Common.MakeCatResp.Handle m,
    hAuth :: Methods.Common.Auth.Handle m,
    hExist :: Methods.Common.Exist.Handle m
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

workWithCats ::
  (MonadCatch m) =>
  Handle m ->
  QueryText ->
  AppMethod ->
  ExceptT ReqError m ResponseInfo
workWithCats h@Handle {..} qStr meth =
  case meth of
    ToPost -> do
      lift $ logInfo hLog "Create category command"
      tokenAdminAuth hAuth qStr
      checkQStr hExist qStr >>= createCategory h
    ToGet catId -> do
      lift $ logInfo hLog "Get category command"
      isExistResourceE hExist (CategoryId catId)
      getCategory h catId
    ToPut catId -> do
      lift $ logInfo hLog "Update category command"
      tokenAdminAuth hAuth qStr
      isExistResourceE hExist (CategoryId catId)
      checkQStr hExist qStr >>= updateCategory h catId
    ToDelete catId -> do
      lift $ logInfo hLog "Delete category command"
      tokenAdminAuth hAuth qStr
      isExistResourceE hExist (CategoryId catId)
      deleteCategory h catId
    _ ->
      throwE $ ResourceNotExistError $
        "Wrong method for categories resource: " ++ show meth

createCategory ::
  (MonadCatch m) =>
  Handle m ->
  CreateCategory ->
  ExceptT ReqError m ResponseInfo
createCategory Handle {..} (CreateCategory catNameParam Nothing) = do
  catId <- catchInsertReturnE hLog $ insertReturnCat catNameParam
  lift $ logInfo hLog $ "Category_id: " ++ show catId ++ " created"
  ok201Helper hConf "category" catId
createCategory Handle {..} (CreateCategory catNameParam (Just superCatIdParam)) = do
  catId <- catchInsertReturnE hLog $ insertReturnSubCat catNameParam superCatIdParam
  lift $ logInfo hLog $ "Sub_Category_id: " ++ show catId ++ " created"
  ok201Helper hConf "category" catId

getCategory ::
  (MonadCatch m) =>
  Handle m ->
  CategoryId ->
  ExceptT ReqError m ResponseInfo
getCategory Handle {..} catId = do
  catResp <- makeCatResp hCatResp catId
  lift $ logInfo hLog $
    "Category_id: " ++ show catId
      ++ " sending in response"
  okHelper catResp

updateCategory ::
  (MonadCatch m) =>
  Handle m ->
  CategoryId ->
  UpdateCategory ->
  ExceptT ReqError m ResponseInfo
updateCategory h@Handle {..} catId (UpdateCategory catNameParam maybeSuperCatIdParam) = do
  case maybeSuperCatIdParam of
    Just superCatIdParam -> do
      checkRelationCats h catId superCatIdParam
      catchUpdE hLog $ updateDbSubCat catNameParam superCatIdParam catId
    Nothing ->
      catchUpdE hLog $ updateDbCat catNameParam catId
  catResp <- makeCatResp hCatResp catId
  lift $ logInfo hLog $ "Category_id: " ++ show catId ++ " updated."
  okHelper catResp

deleteCategory ::
  (MonadCatch m) =>
  Handle m ->
  CategoryId ->
  ExceptT ReqError m ResponseInfo
deleteCategory h@Handle {..} catId = do
  allSubCats <- findAllSubCats h catId
  let allCats = catId : allSubCats
  let updatePos = updateDbCatsForPosts (cDefCatId hConf) allCats
  let updateDr = updateDbCatsForDrafts (cDefCatId hConf) allCats
  let deleteCat = deleteDbCats allCats
  withTransactionDBE h (updatePos >> updateDr >> deleteCat)
  lift $ logInfo hLog $ "Category_id: " ++ show catId ++ " deleted."
  ok204Helper

checkRelationCats ::
  (MonadCatch m) =>
  Handle m ->
  CategoryId ->
  CategoryId ->
  ExceptT ReqError m ()
checkRelationCats h catId superCatId
  | catId == superCatId =
    throwE $ BadReqError $
      "super_category_id: " ++ show superCatId ++ " equal to category_id."
  | otherwise = do
    allSubCats <- findAllSubCats h catId
    when (superCatId `elem` allSubCats)
      $ throwE
      $ BadReqError
      $ "super_category_id: " ++ show superCatId
        ++ " is subCategory of category_id: "
        ++ show catId

findAllSubCats :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m [CategoryId]
findAllSubCats h@Handle {..} catId = do
  catsIds <- findOneLevelSubCats hCatResp catId
  case catsIds of
    [] -> return []
    _ -> do
      subCatsIds <- mapM (findAllSubCats h) catsIds
      return $ catsIds ++ Prelude.concat subCatsIds

fromCatResp :: CatResponse -> CategoryId
fromCatResp (Sub (SubCatResponse iD _ _ _)) = iD
fromCatResp (Super (SuperCatResponse iD _ _)) = iD

withTransactionDBE :: (MonadCatch m) => Handle m -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchTransactionE (hLog h) . withTransactionDB h
