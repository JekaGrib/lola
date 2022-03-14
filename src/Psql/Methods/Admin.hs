{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Admin where

import Api.Response (UserTokenResponse (..))
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Text (Text, pack, unpack)
import Logger
import Methods.Common
import Oops
import Api.Request.QueryStr (CreateAdmin (..),checkQStr)
import Types
import Data.Time.Calendar ( Day)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE)
import Psql.ToQuery
import Network.HTTP.Types (StdMethod(..),QueryText)
import Api.Request.EndPoint
import Psql.ToQuery.Select
import Psql.ToQuery.Insert
import Psql.Methods.Common
import Database.PostgreSQL.Simple (Connection)


selectKeys' :: Connection -> IO [Key]
selectKeys' conn = 
  selectOnly' conn $ Select ["create_admin_key"] "key" (Where "true")
insertReturnUser' :: Connection -> InsertUser -> IO UserId
insertReturnUser' conn (InsertUser pwd fName lName picId day bool tokenKey) = do
  let insPair1 = InsertPair "password"         (Txt  pwd)
  let insPair2 = InsertPair "first_name"       (Txt  fName)
  let insPair3 = InsertPair "last_name"        (Txt  lName)
  let insPair4 = InsertPair "user_pic_id"      (Id   picId)
  let insPair5 = InsertPair "user_create_date" (Day  day)
  let insPair6 = InsertPair "admin"            (Bool bool)
  let insPair7 = InsertPair "token_key"        (Str  tokenKey)
  let insPairs = [insPair1,insPair2,insPair3,insPair4,insPair5,insPair6,insPair7]
  insertReturn' conn (InsertRet "users" insPairs "user_id")

