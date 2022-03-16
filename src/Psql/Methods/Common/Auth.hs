{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Common.Auth where

import Psql.Methods.Common
import Types
import Psql.ToQuery.Select
import Database.PostgreSQL.Simple (Connection)

selectTokenKeysForUser' :: Connection -> UserId -> IO [TokenKey]
selectTokenKeysForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  selectOnly' conn $ Select ["token_key"] "users" wh

