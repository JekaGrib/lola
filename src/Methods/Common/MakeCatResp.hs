{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Methods.Common.MakeCatResp where

import Api.Response (CatResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT)
import Logger (LogHandle (..))
import Methods.Common
import Methods.Common.ToQuery (Select(..),Where(WherePair))
import Methods.Common.Selecty (Cat (..))
import Oops (ReqError)
import Types

data Handle m = Handle
  { hConf :: Config
  , hLog :: LogHandle m
  , selectCats :: CategoryId -> m [Cat]
  , selectSubCats :: CategoryId -> m [SubCategoryId]
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectCats' conn)
        (selectSubCats' conn)

selectCats' conn catId = select' conn $
    Select 
      ["category_name", "COALESCE (super_category_id, '0') AS super_category_id"] 
      "categories" 
      (WherePair "category_id=?" (Id catId))
selectSubCats' conn catId = do
  let wh = WherePair "super_category_id=?" (Id catId)
  selectOnly' conn $ Select ["category_id"] "categories" wh

makeCatResp :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m CatResponse
makeCatResp h@Handle{..} catId = do
  Cat catName superCatId <- catchOneSelE hLog $ selectCats catId
  subCatsIds <- findOneLevelSubCats h catId
  case superCatId of
    0 -> return $ CatResponse {cat_id = catId, cat_name = catName, one_level_sub_cats = subCatsIds}
    _ -> do
      superCatResp <- makeCatResp h superCatId
      return $ SubCatResponse {subCat_id = catId, subCat_name = catName, one_level_sub_categories = subCatsIds, super_category = superCatResp}

findOneLevelSubCats :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m [CategoryId]
findOneLevelSubCats Handle{..} catId =
  catchSelE hLog $ selectSubCats catId
