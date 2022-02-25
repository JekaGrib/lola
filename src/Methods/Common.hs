{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Common where

import Api.Response (CommentIdTextUserResponse (..), PicIdUrl (..), TagResponse (..))
import Conf (Config (..))
import Control.Monad.Catch (MonadCatch, throwM)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, lazyByteString)
import Data.List ((\\), intercalate)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (getZonedTime, localDay ,zonedTimeToLocalTime)
import Database.PostgreSQL.Simple (Binary (..), Connection, Only (..), execute, executeMany, query)
import Database.PostgreSQL.Simple.FromField (FromField)
import Logger
import Methods.Common.Selecty (Comment (..), Selecty, Tag (..))
import Methods.Common.ToQuery (toDelQ, toExQ, toInsManyQ, toInsRetQ, toSelLimQ, toSelQ, toUpdQ)
import Methods.Post.LimitArg
import Network.HTTP.Types (ResponseHeaders, Status, status200)
import Oops
import System.Random (getStdGen, newStdGen, randomRs)
import Types

-- common logic functions:

data ResponseInfo = ResponseInfo {resStatus :: Status, resHeaders :: ResponseHeaders, resBuilder :: Builder}

okHelper :: (MonadCatch m, ToJSON a) => a -> ExceptT ReqError m ResponseInfo
okHelper toJ = return $ ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")] (lazyByteString . encode $ toJ)

checkOneE :: (MonadCatch m, Show a) => LogHandle m -> m [a] -> ExceptT ReqError m a
checkOneE logH m = do
  lift $ logDebug logH "Select data from DB."
  xs <- catchDbErr $ lift m
  case xs of
    [] -> throwE $ DatabaseError "Empty output"
    [x] -> do
      lift $ logInfo logH "Data received from DB"
      return x
    _ -> throwE $ DatabaseError $ "Output not single" ++ show xs

checkMaybeOneE :: (MonadCatch m, Show a) => LogHandle m -> m [a] -> ExceptT ReqError m (Maybe a)
checkMaybeOneE logH m = do
  lift $ logDebug logH "Select data from DB."
  xs <- catchDbErr $ lift m
  case xs of
    [] -> do
      lift $ logInfo logH "Received empty data from DB"
      return Nothing
    [x] -> do
      lift $ logInfo logH "Data received from DB"
      return (Just x)
    _ -> throwE $ DatabaseError $ "Output not single" ++ show xs

checkOneIfExistE ::
  (MonadCatch m, Show a) =>
  LogHandle m ->
  (Table -> [Param] -> Where -> [DbValue] -> m [a]) ->
  Table ->
  [Param] ->
  Where ->
  DbValue ->
  ExceptT ReqError m a
checkOneIfExistE logH func table params where' value = do
  lift $ logDebug logH $ "Select data from DB. Table: " ++ table
  xs <- catchDbErr $ lift $ func table params where' [value]
  case xs of
    [] -> throwE $ SimpleError $ (where' \\ "???") ++ show value ++ " doesn`t exist"
    [x] -> do
      lift $ logInfo logH "Data received from DB"
      return x
    _ -> throwE $ DatabaseError $ "Output not single" ++ show xs

checkListE :: (MonadCatch m, Show a) => LogHandle m -> m [a] -> ExceptT ReqError m [a]
checkListE logH m = do
  lift $ logDebug logH "Select data from DB."
  xs <- catchDbErr $ lift m
  lift $ logInfo logH "Data received from DB"
  return xs

catchDbErrE :: (MonadCatch m) => m a -> ExceptT ReqError m a
catchDbErrE = catchDbErr . lift

checkUpdE :: (MonadCatch m) => LogHandle m -> m () -> ExceptT ReqError m ()
checkUpdE logH m = do
  lift $ logDebug logH "Update data in DB."
  catchDbErrE m
  lift $ logInfo logH "Data updated in DB"

checkIsExistE ::
  (MonadCatch m) =>
  LogHandle m ->
  (Table -> Where -> DbValue -> m Bool) ->
  Table ->
  Where ->
  DbValue ->
  ExceptT ReqError m ()
checkIsExistE logH func table where' value = do
  lift $ logDebug logH $ "Checking existence " ++ where' ++ show value ++ " in the DB"
  isExist <- catchDbErrE $ func table where' value
  if isExist
    then
      ( do
          lift $ logInfo logH $ "Entity (" ++ where' ++ show value ++ ") exist"
          return ()
      )
    else
      throwE
        $ SimpleError
        $ (where' \\ "=?") 
          ++ ": "
          ++ show value
          ++ " doesn`t exist."

checkInsRetE ::
  (MonadCatch m) =>
  LogHandle m ->
  (Table -> String -> [String] -> a -> m Id) ->
  Table ->
  String ->
  [String] ->
  a ->
  ExceptT ReqError m Id
checkInsRetE logH func table returnName insNames insValues = do
  lift $ logDebug logH "Insert data in the DB"
  i <- catchDbErrE $ func table returnName insNames insValues
  lift $ logInfo logH $ "DB return " ++ returnName ++ ": " ++ show i
  return i

checkTransactE :: (MonadCatch m) => LogHandle m -> m a -> ExceptT ReqError m a
checkTransactE logH m = do
  lift $ logDebug logH "Open transaction in DB to do several actions"
  a <- catchDbErrE m
  lift $ logInfo logH "Transaction closed. Several actions in DB finished."
  return a

checkOneM :: (MonadCatch m) => [a] -> m a
checkOneM xs = case xs of
  [] -> throwM UnexpectedEmptyDbOutPutException
  [x] -> return x
  _ -> throwM UnexpectedMultipleDbOutPutException

-- common IO handle functions:

getDay' :: IO Day
getDay' = do
  time <- getZonedTime
  let day = localDay . zonedTimeToLocalTime $ time
  return day

select' :: (Selecty a) => Connection -> Table -> [Param] -> Where -> [DbValue] -> IO [a]
select' conn table params where' =
  query conn (toSelQ table params where')

selectOnly' :: (FromField a) => Connection -> Table -> [Param] -> Where -> [DbValue] -> IO [a]
selectOnly' conn table params where' values = do
  xs <- query conn (toSelQ table params where') values
  return $ fmap fromOnly xs

selectBS' :: Connection -> Table -> [Param] -> Where -> [DbValue] -> IO [ByteString]
selectBS' conn table params where' values = do
  xs <- query conn (toSelQ table params where') values
  return $ fmap (fromBinary . fromOnly) xs

selectLimit' :: (Selecty a) => Connection -> Table -> String -> Page -> Limit -> [String] -> String -> [DbValue] -> [FilterArg] -> [SortArg] -> IO [a]
selectLimit' conn defTable defOrderBy page limitNumber params defWhere defValues filterArgs sortArgs = do
  let table = unwords $ [defTable] ++ fmap tableFil filterArgs ++ fmap tableSort sortArgs
  let where' = intercalate " AND " $ defWhere : fmap whereFil filterArgs
  let orderBy = intercalate "," $ fmap orderBySort sortArgs ++ [defOrderBy]
  let values = (concatMap fst . fmap valuesFil $ filterArgs) ++ defValues ++ (concatMap snd . fmap valuesFil $ filterArgs)
  query conn (toSelLimQ table orderBy page limitNumber params where') values

updateInDb' :: Connection -> String -> String -> String -> [DbValue] -> IO ()
updateInDb' conn table set where' values = do
  _ <- execute conn (toUpdQ table set where') values
  return ()

deleteFromDb' :: Connection -> Table -> Where -> [DbValue] -> IO ()
deleteFromDb' conn table where' values = do
  _ <- execute conn (toDelQ table where') values
  return ()

isExistInDb' :: Connection -> Table -> Where -> DbValue -> IO Bool
isExistInDb' conn table where' value = do
  onlyChecks <- query conn (toExQ table where') [value]
  Only isExist <- checkOneM onlyChecks
  return isExist

insertReturn' :: Connection -> String -> String -> [String] -> [DbValue] -> IO Id
insertReturn' conn table returnName insNames insValues = do
  onlyXs <- query conn (toInsRetQ table returnName insNames) insValues
  Only num <- checkOneM onlyXs
  return num

insertByteaInDb' :: Connection -> String -> String -> [String] -> ByteString -> IO Id
insertByteaInDb' conn table returnName insNames bs = do
  onlyXs <- query conn (toInsRetQ table returnName insNames) [Binary bs]
  Only num <- checkOneM onlyXs
  return num

insertMany' :: Connection -> Table -> [DbInsertParamKey] -> [(Id, Id)] -> IO ()
insertMany' conn table insNames insValues = do
  _ <- executeMany conn (toInsManyQ table insNames) insValues
  return ()

getTokenKey' :: IO String
getTokenKey' = do
  gen <- getStdGen
  _ <- newStdGen
  return . take 6 $ randomRs ('a', 'z') gen

-- common clear functions:

sha1 :: ByteString -> Digest SHA1
sha1 = hash

strSha1 :: String -> String
strSha1 = show . sha1 . fromString

txtSha1 :: Text -> Text
txtSha1 = pack . strSha1 . unpack

inCommResp :: Comment -> CommentIdTextUserResponse
inCommResp (Comment idCom usId txt) = CommentIdTextUserResponse idCom txt usId

inTagResp :: Tag -> TagResponse
inTagResp (Tag tagId tagName) = TagResponse tagId tagName

makeMyPicUrl :: Config -> PictureId -> Text
makeMyPicUrl conf picId = pack $ "http://" ++ cServHost conf ++ ":" ++ show (cServPort conf) ++ "/picture/" ++ show picId

inPicIdUrl :: Config -> PictureId -> PicIdUrl
inPicIdUrl conf picId = PicIdUrl picId (makeMyPicUrl conf picId)

numToTxt :: Id -> Text
numToTxt = pack . show
