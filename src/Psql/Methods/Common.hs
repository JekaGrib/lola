module Psql.Methods.Common where

import Control.Monad.Catch (MonadCatch, throwM)
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
  ( Binary (..),
    Connection,
    Only (..),
    execute,
    executeMany,
    query,
  )
import Database.PostgreSQL.Simple.FromField (FromField)
import Error (UnexpectedDbOutPutException (..))
import Psql.Selecty (Selecty)
import Psql.ToQuery (toQ, toVal)
import Psql.ToQuery.Delete (Delete)
import Psql.ToQuery.Exists (Exists)
import Psql.ToQuery.Insert (InsertMany (..), InsertRet, insManyVal)
import Psql.ToQuery.Select (Select)
import Psql.ToQuery.SelectLimit (SelectLim)
import Psql.ToQuery.Update (Update)
import Types

select' :: (Selecty a) => Connection -> Select -> IO [a]
select' conn select =
  query conn (toQ select) (toVal select)

selectOnly' :: (FromField a) => Connection -> Select -> IO [a]
selectOnly' conn select = do
  xs <- query conn (toQ select) (toVal select)
  return $ fmap fromOnly xs

selectBS' :: Connection -> Select -> IO [ByteString]
selectBS' conn select = do
  xs <- query conn (toQ select) (toVal select)
  return $ fmap (fromBinary . fromOnly) xs

selectLimit' :: (Selecty a) => Connection -> SelectLim -> IO [a]
selectLimit' conn select =
  query conn (toQ select) (toVal select)

updateInDb' :: Connection -> Update -> IO ()
updateInDb' conn upd = do
  _ <- execute conn (toQ upd) (toVal upd)
  return ()

deleteFromDb' :: Connection -> Delete -> IO ()
deleteFromDb' conn del = do
  _ <- execute conn (toQ del) (toVal del)
  return ()

isExistInDb' :: Connection -> Exists -> IO Bool
isExistInDb' conn exist = do
  onlyChecks <- query conn (toQ exist) (toVal exist)
  Only isExist <- checkOneM onlyChecks
  return isExist

insertReturn' :: Connection -> InsertRet -> IO Id
insertReturn' conn insertReturn = do
  onlyXs <- query conn (toQ insertReturn) (toVal insertReturn)
  Only num <- checkOneM onlyXs
  return num

insertMany' :: Connection -> InsertMany -> IO ()
insertMany' conn insertMany@(InsertMany _ pair) = do
  _ <- executeMany conn (toQ insertMany) (insManyVal pair)
  return ()

checkOneM :: (MonadCatch m) => [a] -> m a
checkOneM xs = case xs of
  [] -> throwM UnexpectedEmptyDbOutPutException
  [x] -> return x
  _ -> throwM UnexpectedMultipleDbOutPutException
