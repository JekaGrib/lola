{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Picture where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Binary (..), Connection)
import Psql.Methods.Common
import Psql.ToQuery.Insert (InsertPair (..), InsertRet (..))
import Psql.ToQuery.Select (Select (..), Where (..))
import Types

selectPicBS' :: Connection -> PictureId -> IO [ByteString]
selectPicBS' conn picId = do
  let wh = WherePair "pic_id=?" (Id picId)
  selectBS' conn (Select ["pic"] "pics" wh)

insertRetPicBS' :: Connection -> ByteString -> IO PictureId
insertRetPicBS' conn sbs = do
  let insPair = InsertPair "pic" (BS (Binary sbs))
  insertReturn' conn (InsertRet "pics" [insPair] "pic_id")
