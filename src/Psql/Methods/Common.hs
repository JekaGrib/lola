{-# LANGUAGE OverloadedStrings #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Psql.Methods.Common where

import Conf (Config (..))
import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, lazyByteString,toLazyByteString)
import Data.List ((\\), intercalate)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (getZonedTime, localDay ,zonedTimeToLocalTime)
import Database.PostgreSQL.Simple (Binary (..), Connection, Only (..), execute, executeMany, query)
import Database.PostgreSQL.Simple.FromField (FromField)
import Logger
import Network.HTTP.Types (ResponseHeaders, Status, status200,QueryText)
import Oops
import System.Random (getStdGen, newStdGen, randomRs)
import Types
import Network.HTTP.Types (StdMethod(..))
import Psql.ToQuery 
import Psql.ToQuery.Delete
import Psql.ToQuery.Exists
import Psql.ToQuery.Insert
import Psql.ToQuery.SelectLimit
import Psql.ToQuery.Select
import Psql.ToQuery.Update
import Psql.Selecty (Comment (..), Selecty, Tag (..))



select' :: (Selecty a) => Connection -> Select -> IO [a]
select' conn sel =
  query conn (toQ sel) (toVal sel)

selectOnly' :: (FromField a) => Connection -> Select -> IO [a]
selectOnly' conn sel = do
  xs <- query conn (toQ sel) (toVal sel)
  return $ fmap fromOnly xs

selectBS' :: Connection -> Select -> IO [ByteString]
selectBS' conn sel = do
  xs <- query conn (toQ sel) (toVal sel)
  return $ fmap (fromBinary . fromOnly) xs

selectLimit' :: (Selecty a) => Connection -> SelectLim -> IO [a]
selectLimit' conn sel = 
  query conn (toQ sel) (toVal sel)

updateInDb' :: Connection -> Update -> IO ()
updateInDb' conn upd = do
  _ <- execute conn (toQ upd) (toVal upd)
  return ()

deleteFromDb' :: Connection -> Delete -> IO ()
deleteFromDb' conn del = do
  _ <- execute conn (toQ del) (toVal del)
  return ()

isExistInDb' :: Connection -> Exists -> IO Bool
isExistInDb' conn exi = do
  onlyChecks <- query conn (toQ exi) (toVal exi)
  Only isExist <- checkOneM onlyChecks
  return isExist

insertReturn' :: Connection -> InsertRet -> IO Id
insertReturn' conn insRet = do
  onlyXs <- query conn (toQ insRet) (toVal insRet)
  Only num <- checkOneM onlyXs
  return num


insertMany' :: Connection -> InsertMany -> IO ()
insertMany' conn insMany@(InsertMany t pair) = do
  _ <- executeMany conn (toQ insMany) (insManyVal pair)
  return ()

checkOneM :: (MonadCatch m) => [a] -> m a
checkOneM xs = case xs of
  [] -> throwM UnexpectedEmptyDbOutPutException
  [x] -> return x
  _ -> throwM UnexpectedMultipleDbOutPutException