{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Common.MakeCatResp where

import Api.Response (CatResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Except (ExceptT)
import Logger (LogHandle (..))
import Methods.Common
import Psql.ToQuery.Select (Select(..),Where(WherePair))
import Psql.Selecty (Cat (..))
import Oops (ReqError)
import Types
import Psql.Methods.Common
import Psql.ToQuery.Delete
import Psql.ToQuery.Exists
import Psql.ToQuery.Insert
import Psql.ToQuery.SelectLimit
import Psql.ToQuery.Select
import Psql.ToQuery.Update
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


