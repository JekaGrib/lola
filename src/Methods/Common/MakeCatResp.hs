{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Common.MakeCatResp where

import Api.Response (CatResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT)
import Logger (LogHandle (..))
import Methods.Common
import Methods.Common.Select (Cat (..))
import Oops (ReqError)
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    selectNums :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Id],
    selectCats :: Table -> [DbSelectParamKey] -> Where -> [DbValue] -> m [Cat]
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  let conn = extractConn conf
   in Handle
        conf
        logH
        (selectOnly' conn)
        (select' conn)

makeCatResp :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m CatResponse
makeCatResp h catId = do
  Cat catName superCatId <- checkOneE (hLog h) $ selectCats h "categories" ["category_name", "COALESCE (super_category_id, '0') AS super_category_id"] "category_id=?" [Num catId]
  subCatsIds <- findOneLevelSubCats h catId
  case superCatId of
    0 -> return $ CatResponse {cat_id = catId, cat_name = catName, one_level_sub_cats = subCatsIds}
    _ -> do
      superCatResp <- makeCatResp h superCatId
      return $ SubCatResponse {subCat_id = catId, subCat_name = catName, one_level_sub_categories = subCatsIds, super_category = superCatResp}

findOneLevelSubCats :: (MonadCatch m) => Handle m -> CategoryId -> ExceptT ReqError m [Integer]
findOneLevelSubCats h catId =
  checkListE (hLog h) $ selectNums h "categories" ["category_id"] "super_category_id=?" [Num catId]
