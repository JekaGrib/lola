{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Common.Auth where

import Database.PostgreSQL.Simple (Connection)
import Psql.Methods.Common
import Psql.ToQuery.Select
import Types

selectTokenKeysForUser' :: Connection -> UserId -> IO [TokenKey]
selectTokenKeysForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  selectOnly' conn $ Select ["token_key"] "users" wh
