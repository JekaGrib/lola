{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Methods.Category where
          
import           Api.Response (CatResponse(..),OkResponse(..))
import           Logger
import           Types
import           Oops
import           Methods.Handle
import Methods.Handle.Select (Cat(..))
import ParseQueryStr (CreateCategory(..),CreateSubCategory(..),UpdateCategory(..),DeleteCategory(..))
import Conf (Config(..))
import           Data.Text                      ( pack )
import           Control.Monad.Trans.Except (ExceptT,throwE)
import           Control.Monad.Catch            ( MonadCatch)
import           Data.List                      ( intercalate )
import           Control.Monad.Trans            ( lift )
import           Control.Monad (when)


createCategory :: (MonadCatch m) => Handle m -> CreateCategory -> ExceptT ReqError m ResponseInfo
createCategory h (CreateCategory catNameParam) = do
  catId <-  insertReturnE h "categories" "category_id" ["category_name"] [catNameParam]
  lift $ logInfo (hLog h) $ "Category_id: " ++ show catId ++ " created"
  okHelper $ CatResponse {cat_id = catId, cat_name = catNameParam, one_level_sub_cats = [] , super_cat = "NULL"}

createSubCategory :: (MonadCatch m) => Handle m -> CreateSubCategory -> ExceptT ReqError m ResponseInfo
createSubCategory h (CreateSubCategory catNameParam superCatIdNum) = do
  let superCatIdParam = numToTxt superCatIdNum
  isExistInDbE h "categories" "category_id" "category_id=?" [superCatIdParam] 
  catId <-  insertReturnE h "categories" "category_id" ["category_name","super_category_id"] [catNameParam,superCatIdParam] 
  catResp <- makeCatResp h  catId
  lift $ logInfo (hLog h) $ "Sub_Category_id: " ++ show catId ++ " created"
  okHelper catResp

getCategory :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m ResponseInfo 
getCategory h catIdNum = do
  isExistInDbE h "categories" "category_id" "category_id=?" [numToTxt catIdNum] 
  catResp <- makeCatResp h  catIdNum
  lift $ logInfo (hLog h) $ "Category_id: " ++ show catIdNum ++ " sending in response" 
  okHelper catResp
  
updateCategory :: (MonadCatch m) => Handle m -> UpdateCategory -> ExceptT ReqError m ResponseInfo 
updateCategory h (UpdateCategory catIdNum catNameParam superCatIdNum) = do
  let catIdParam = numToTxt catIdNum
  let superCatIdParam = numToTxt superCatIdNum
  isExistInDbE h "categories" "category_id" "category_id=?" [catIdParam]      
  isExistInDbE h "categories" "category_id" "category_id=?" [superCatIdParam] 
  checkRelationCats h  catIdNum superCatIdNum
  updateInDbE h "categories" "category_name=?,super_category_id=?" "category_id=?" [catNameParam,superCatIdParam,catIdParam]
  catResp <- makeCatResp h  catIdNum
  lift $ logInfo (hLog h) $ "Category_id: " ++ show catIdNum ++ " updated." 
  okHelper catResp

deleteCategory :: (MonadCatch m) => Handle m -> DeleteCategory -> ExceptT ReqError m ResponseInfo 
deleteCategory h (DeleteCategory catIdNum) = do
  let catIdParam = numToTxt catIdNum
  isExistInDbE h "categories" "category_id" "category_id=?" [catIdParam] 
  allSubCats <- findAllSubCats h  catIdNum
  let values = fmap (pack . show) (cDefCatId (hConf h) : allSubCats)
  let where'  = intercalate " OR " . fmap (const "post_category_id=?")  $ allSubCats
  let where'' = intercalate " OR " . fmap (const "draft_category_id=?") $ allSubCats
  let updatePos = updateInDb h "posts"  "post_category_id=?"  where'  values
  let updateDr  = updateInDb h "drafts" "draft_category_id=?" where'' values
  let where''' = intercalate " OR " . fmap (const "category_id=?") $ allSubCats
  let deleteCat = deleteFromDb h "categories" where''' (fmap (pack . show) allSubCats)
  withTransactionDBE h (updatePos >> updateDr >> deleteCat)
  lift $ logInfo (hLog h) $ "Category_id: " ++ show catIdNum ++ " deleted." 
  okHelper $ OkResponse {ok = True}

checkRelationCats :: (MonadCatch m) => Handle m -> CategoryId -> CategoryId -> ExceptT ReqError m ()
checkRelationCats h  catIdNum superCatIdNum 
  |catIdNum == superCatIdNum = throwE $ SimpleError $ "super_category_id: " ++ show superCatIdNum ++ " equal to category_id."
  |otherwise                 = do
    allSubCats <- findAllSubCats h  catIdNum
    when (superCatIdNum `elem` allSubCats) $
      throwE $ SimpleError $ "super_category_id: " ++ show superCatIdNum ++ " is subCategory of category_id: " ++ show catIdNum

findAllSubCats :: (MonadCatch m) => Handle m  -> CategoryId -> ExceptT ReqError m [Integer]
findAllSubCats h  catId = do
  catsIds <- findOneLevelSubCats h catId 
  case catsIds of
    [] -> return [catId]
    _  -> do       
      subCatsIds <- mapM (findAllSubCats h ) catsIds
      return $ catId : Prelude.concat  subCatsIds

findOneLevelSubCats :: (MonadCatch m) => Handle m  -> CategoryId -> ExceptT ReqError m [Integer]
findOneLevelSubCats h catId = do
    catsIds <- checkListE h $ selectNum h "categories" ["category_id"] "super_category_id=?" [pack . show $ catId]
    return catsIds  

makeCatResp :: (MonadCatch m) => Handle m  -> CategoryId -> ExceptT ReqError m CatResponse
makeCatResp h catId = do
  Cat catName superCatId <- checkOneE h $ (selectCat h) "categories" ["category_name","COALESCE (super_category_id, '0') AS super_category_id"] "category_id=?" [pack . show $ catId] 
  subCatsIds <- findOneLevelSubCats h catId
  case superCatId of 
    0 -> return $ CatResponse {cat_id = catId, cat_name = catName, one_level_sub_cats = subCatsIds , super_cat = "NULL"}
    _ -> do
      superCatResp <- makeCatResp h superCatId
      return $ SubCatResponse { subCat_id = catId , subCat_name = catName, one_level_sub_categories = subCatsIds , super_category = superCatResp}

fromCatResp :: CatResponse -> CategoryId
fromCatResp (SubCatResponse a _ _ _) = a
fromCatResp (CatResponse a _ _ _) = a
