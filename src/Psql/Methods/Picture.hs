{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Picture where

import Codec.Picture (decodeImage)
import Conf (Config (..), extractConn)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Data.ByteString (ByteString)
import Data.ByteString.Builder (lazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.String (fromString)
import Data.Text (Text, unpack)
import Logger
import Methods.Common
import qualified Network.HTTP.Simple as HT
import Network.HTTP.Types (status200,StdMethod(..),QueryText)
import Database.PostgreSQL.Simple (Binary(..))
import Oops
import Api.Request.QueryStr (LoadPicture (..),checkQStr)
import Types
import qualified Methods.Common.Auth (Handle, makeH)
import Methods.Common.Auth (tokenAdminAuth,tokenUserAuth)
import qualified Methods.Common.Exist (Handle, makeH)
import Methods.Common.Exist (isExistResourseE)
import Psql.ToQuery
import TryRead (tryReadResourseId)
import Api.Request.EndPoint
import Psql.ToQuery.Delete
import Psql.ToQuery.Exists
import Psql.ToQuery.Insert
import Psql.ToQuery.Select
import Psql.ToQuery.Update
import Psql.Methods.Common

selectPicBS' conn picId = do
  let wh = WherePair "pic_id=?" (Id picId)
  selectBS' conn (Select ["pic"] "pics" wh)
insertRetPicBS' conn sbs = do
  let insPair = InsertPair "pic" (BS (Binary sbs))
  insertReturn' conn (InsertRet "pics" [insPair] "pic_id")

