{-# LANGUAGE OverloadedStrings #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

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
import Data.ByteString.Builder (Builder, lazyByteString,toLazyByteString)
import Data.List ((\\), intercalate)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (getZonedTime, localDay ,zonedTimeToLocalTime)
import Database.PostgreSQL.Simple (Binary (..), Connection, Only (..), execute, executeMany, query)
import Database.PostgreSQL.Simple.FromField (FromField)
import Logger
import Methods.Common.Selecty (Comment (..), Selecty, Tag (..))
import Methods.Common.ToQuery
import Network.HTTP.Types (ResponseHeaders, Status, status200,QueryText)
import Oops
import System.Random (getStdGen, newStdGen, randomRs)
import Types
import Methods.Common.ToQuery
import Network.HTTP.Types (StdMethod(..))


-- common logic functions:

data ResponseInfo = ResponseInfo {resStatus :: Status, resHeaders :: ResponseHeaders, resBuilder :: Builder}

instance Show  ResponseInfo where
  show (ResponseInfo s h b) = "ResponseInfo Status: " ++ show s ++ ". Headers: " ++ show h ++ ". Builder: " ++ (show $ toLazyByteString b)

data ReqInfo = ReqInfo StdMethod [Text] QueryText (Maybe ByteString)

okHelper :: (MonadCatch m, ToJSON a) => a -> ExceptT ReqError m ResponseInfo
okHelper toJ = return $ ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")] (lazyByteString . encode $ toJ)

catchSelE :: (MonadCatch m) => LogHandle m -> m [a] -> ExceptT ReqError m [a]
catchSelE logH m = do
  lift $ logDebug logH "Select data from DB."
  xs <- catchDbErr $ lift m
  lift $ logInfo logH "Data received from DB"
  return xs

checkOneE :: (MonadCatch m, Show a) => [a] -> ExceptT ReqError m a
checkOneE xs = case xs of
  [] -> throwE $ DatabaseError "Empty output"
  [x] -> return x
  _ -> throwE $ DatabaseError $ "Output not single" ++ show xs  

checkMaybeOneE :: (MonadCatch m, Show a) =>  [a] -> ExceptT ReqError m (Maybe a)
checkMaybeOneE xs = case xs of
  [] -> return Nothing
  [x] -> return (Just x)
  _ -> throwE $ DatabaseError $ "Output not single" ++ show xs

{-checkOneIfExistE :: (MonadCatch m, Show a) => LogPair -> [a] -> ExceptT ReqError m a
checkOneIfExistE (k,v) xs = case xs of
  [] -> throwE $ SimpleError $ k ++ ": " ++ v ++ " doesn`t exist"
  [x] -> return x
  _ -> throwE $ DatabaseError $ "Output not single" ++ show xs

checkOneIfResExistE :: (MonadCatch m, Show a) => LogPair -> [a] -> ExceptT ReqError m a
checkOneIfResExistE (k,v) xs = case xs of
  [] -> throwE $ SimpleError $ k ++ ": " ++ v ++ " doesn`t exist"
  [x] -> return x
  _ -> throwE $ DatabaseError $ "Output not single" ++ show xs-}

catchOneSelE :: (MonadCatch m,Show a) => LogHandle m -> m [a] -> ExceptT ReqError m a
catchOneSelE logH m = 
  catchSelE logH m >>= checkOneE

catchMaybeOneSelE :: (MonadCatch m,Show a) => LogHandle m -> m [a] -> ExceptT ReqError m (Maybe a)
catchMaybeOneSelE logH m = 
  catchSelE logH m >>= checkMaybeOneE

{-catchOneSelIfExistsE :: (MonadCatch m,Show a) => LogHandle m -> LogPair -> m [a] -> ExceptT ReqError m a
catchOneSelIfExistsE (k,v) logH m = 
  catchSelE logH m >>= checkOneIfExistE (k,v)

catchOneSelIfResExistsE :: (MonadCatch m,Show a) => LogHandle m -> LogPair -> m [a] -> ExceptT ReqError m a
catchOneSelIfResExistsE logH m = 
  catchSelE logH m >>= checkOneIfExistE (k,v)-}




catchDbErrE :: (MonadCatch m) => m a -> ExceptT ReqError m a
catchDbErrE = catchDbErr . lift

catchUpdE :: (MonadCatch m) => LogHandle m -> m () -> ExceptT ReqError m ()
catchUpdE logH m = do
  lift $ logDebug logH "Update data in DB."
  catchDbErrE m
  lift $ logInfo logH "Data updated in DB"

{-
catchExistE ::
  (MonadCatch m,Show a) => LogHandle m -> (LogKey,a) -> m Bool -> ExceptT ReqError m ()
catchExistE logH (k,v) m = do
  lift $ logDebug logH $ "Checking existence " ++ k ++ ": " ++ show v ++ " in the DB"
  isExist <- catchDbErrE m 
  checkExistE (k,v) isExist

checkExistE :: 
  (MonadCatch m,Show a) => LogHandle m -> (LogKey,a) -> Bool -> ExceptT ReqError m ()
checkExistE (k,v) isExist = 
  if isExist
    then lift $ logInfo logH $ "Entity (" ++ k ++ ": " ++ show v ++ ") exist"
    else
      throwE
        $ SimpleError
        $ k ++ ": " ++ show v
          ++ " doesn`t exist."

checkResE logH path

catchResExistE ::
  (MonadCatch m,Show a) => LogHandle m -> (UncheckedExId -> m Bool) -> UncheckedExId -> ExceptT ReqError m ()
catchResExistE logH func id = do
  lift $ logDebug logH $ "Checking existence request entity in the DB"
  isEx <- catchDbErrE $ func id
  checkResExistE isEx id

checkResExistE :: 
  (MonadCatch m,Show a) => LogHandle m -> Bool -> UncheckedExId -> ExceptT ReqError m ()
checkResExistE isEx id = 
  unless isEx . throwE $ ResourseEntityNotExistError id
-}


catchInsRetE ::
  (MonadCatch m) =>
  LogHandle m -> m Id ->
  ExceptT ReqError m Id
catchInsRetE logH m = do
  lift $ logDebug logH "Insert data in the DB"
  i <- catchDbErrE $ m
  lift $ logInfo logH $ "DB return id: " ++ show i
  return i

catchTransactE :: (MonadCatch m) => LogHandle m -> m a -> ExceptT ReqError m a
catchTransactE logH m = do
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

--insertByteaInDb' :: Connection -> InsertRet -> IO Id
--insertByteaInDb' conn table returnName insNames bs = do
  --onlyXs <- query conn (toInsRetQ table returnName insNames) [Binary bs]
  --Only num <- checkOneM onlyXs
  --return num

insertMany' :: Connection -> InsertMany -> IO ()
insertMany' conn insMany@(InsertMany t pair) = do
  _ <- executeMany conn (toQ insMany) (insManyVal pair)
  return ()

getTokenKey' :: IO TokenKey
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
