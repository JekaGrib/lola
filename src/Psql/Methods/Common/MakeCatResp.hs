{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Common.MakeCatResp where

import Psql.Selecty (Cat (..))
import Types
import Psql.Methods.Common
import Psql.ToQuery.Select (Select(..),Where(WherePair))
import Database.PostgreSQL.Simple (Connection)

selectCats' :: Connection -> CategoryId -> IO [Cat]
selectCats' conn catId = select' conn $
    Select 
      ["category_name", "COALESCE (super_category_id, '0') AS super_category_id"] 
      "categories" 
      (WherePair "category_id=?" (Id catId))

selectSubCats' :: Connection -> CategoryId -> IO [SubCategoryId]
selectSubCats' conn catId = do
  let wh = WherePair "super_category_id=?" (Id catId)
  selectOnly' conn $ Select ["category_id"] "categories" wh


