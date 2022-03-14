{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Common.Auth where

import Api.Response (TokenResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text, pack, unpack)
import Logger
import Psql.Methods.Common
import Psql.Selecty (Auth (..))
import Network.Wai (Request)
import Oops
import Api.Request.QueryStr (LogIn (..), Token (..), parseQueryStr)
import TryRead (tryReadId)
import Types
import Psql.ToQuery
import Network.HTTP.Types.URI (QueryText)
import Psql.Methods.Common
import Psql.ToQuery.Delete
import Psql.ToQuery.Exists
import Psql.ToQuery.Insert
import Psql.ToQuery.SelectLimit
import Psql.ToQuery.Select
import Psql.ToQuery.Update


selectTokenKeysForUser' conn usId = do
  let wh = WherePair "user_id=?" (Id usId)
  selectOnly' conn $ Select ["token_key"] "users" wh

