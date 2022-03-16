{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Psql.Selecty (Cat (..))
import Oops (ReqError)
import Types
import Psql.Methods.Common.MakeCatResp

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
