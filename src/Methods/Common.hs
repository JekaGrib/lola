--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}




module Methods.Common where
          
import           Api.Response
import           Api.Request
import           Logger
import           Types
import Methods.Common.Select
import           Oops
import  Conf (Config(..),extractConn)
import ParseQueryStr 
import CheckJsonReq (checkDraftReqJson)
import Methods.Common.ToQuery (toSelQ,toSelLimQ,toUpdQ,toDelQ,toExQ,toInsRetQ,toInsManyQ)
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Network.HTTP.Types             ( status200, status404, Status, ResponseHeaders )
import           Data.Aeson (ToJSON,encode)
import           Data.Text                      ( pack, unpack, Text )
import           Data.ByteString.Builder        ( lazyByteString, Builder, toLazyByteString )
import           Database.PostgreSQL.Simple (query, withTransaction, execute, executeMany,Connection,Only(..),Binary(..))
import           Database.PostgreSQL.Simple.FromField (FromField)
import qualified Network.HTTP.Simple            as HT
import           Data.Time.LocalTime
import           Data.Time.Calendar             ( showGregorian,Day)
import           Data.String                    ( fromString )
import           Data.List                      ( intercalate, zip4, nub, (\\) )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Codec.Picture                  ( decodeImage )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy           as BSL
import           Control.Monad.Catch            ( catch, throwM, MonadCatch)
import           Crypto.Hash                    (hash,Digest)
import Crypto.Hash.Algorithms (SHA1)
import System.Random (getStdGen,newStdGen,randomRs)
import Methods.Post.LimitArg


-- common logic functions:

data ResponseInfo = ResponseInfo {resStatus :: Status, resHeaders :: ResponseHeaders, resBuilder :: Builder}

okHelper :: (MonadCatch m, ToJSON a) => a -> ExceptT ReqError m ResponseInfo
okHelper toJ = return $ ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")]  (lazyByteString . encode $ toJ)


checkOneE :: (MonadCatch m,Show a) => LogHandle m -> m [a] -> ExceptT ReqError m a
checkOneE logH m = do
  lift $ logDebug logH $ "Select data from DB." 
  xs <- catchDbErr $ lift m
  case xs of
    []           -> throwE $ DatabaseError "Empty output"
    [x] -> do
      lift $ logInfo logH $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "Output not single" ++ show xs

checkMaybeOneE :: (MonadCatch m,Show a) => LogHandle m -> m [a] -> ExceptT ReqError m (Maybe a)
checkMaybeOneE logH m = do
  lift $ logDebug logH $ "Select data from DB." 
  xs <- catchDbErr $ lift m
  case xs of
    []           -> do
      lift $ logInfo logH $ "Received empty data from DB"
      return Nothing
    [x] -> do
      lift $ logInfo logH $ "Data received from DB"
      return (Just x)
    _            -> throwE $ DatabaseError $ "Output not single" ++ show xs

checkOneIfExistE :: (MonadCatch m,Show a) => LogHandle m -> 
  (Table -> [Param] -> Where -> [Text] -> m [a]) ->
  Table -> [Param] -> Where -> Text -> ExceptT ReqError m a
checkOneIfExistE logH func table params where' value = do
  lift $ logDebug logH $ "Select data from DB. Table: " ++ table 
  xs <- catchDbErr $ lift $ func table params where' [value]
  case xs of
    []           -> throwE $ SimpleError $ (where' \\ "???") ++ unpack value ++ " doesn`t exist"
    (x:[]) -> do
      lift $ logInfo logH $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "Output not single" ++ show xs

checkListE :: (MonadCatch m,Show a) => LogHandle m -> m [a] -> ExceptT ReqError m [a]
checkListE logH m = do
  lift $ logDebug logH "Select data from DB."
  xs <- catchDbErr $ lift m
  lift $ logInfo logH "Data received from DB"
  return xs

checkE :: (MonadCatch m) => m a -> ExceptT ReqError m a
checkE = catchDbErr . lift

checkUpdE :: (MonadCatch m) => LogHandle m -> m () -> ExceptT ReqError m ()
checkUpdE logH m = do
  lift $ logDebug logH "Update data in DB."
  checkE m
  lift $ logInfo logH "Data updated in DB"

checkDelE :: (MonadCatch m) => LogHandle m -> m () -> ExceptT ReqError m ()
checkDelE logH m = do
  lift $ logDebug logH "Delete data from DB."
  checkE m
  lift $ logInfo logH "Data deleted from DB"

checkIsExistE :: (MonadCatch m) => LogHandle m ->
  (String -> String -> String -> [Text] -> m Bool) -> 
   String -> String -> String -> [Text] -> ExceptT ReqError m ()
checkIsExistE logH func table checkName where' values = do
  lift $ logDebug logH $ "Checking existence entity (" ++ checkName ++ ") in the DB"
  isExist  <- checkE $ func table checkName where' values 
  case isExist of
    True -> do
      lift $ logInfo logH $ "Entity (" ++ checkName ++ ") exist"
      return ()
    False -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " doesn`t exist."

checkInsRetE :: (MonadCatch m) => LogHandle m ->
  (Table -> String -> [String] -> a -> m Integer) -> 
   Table -> String -> [String] -> a -> ExceptT ReqError m Integer
checkInsRetE logH func table returnName insNames insValues = do
  lift $ logDebug logH $ "Insert data in the DB"
  i <- checkE $ func table returnName insNames insValues
  lift $ logInfo logH $ "DB return " ++ returnName ++ ": " ++ show i
  return i

checkTransactE :: (MonadCatch m) => LogHandle m -> m a -> ExceptT ReqError m a
checkTransactE logH m = do
  lift $ logDebug logH "Open transaction in DB to do several actions"
  a <- checkE m
  lift $ logInfo logH "Transaction closed. Several actions in DB finished."
  return a

checkOneM :: (MonadCatch m) => [a] -> m a
checkOneM xs = case xs of
  [] -> throwM UnexpectedEmptyDbOutPutException
  (x:[]) -> return x
  _ -> throwM UnexpectedMultipleDbOutPutException

-- common IO handle functions:

getDay' :: IO String
getDay' = do
  time    <- getZonedTime
  let day = showGregorian . localDay . zonedTimeToLocalTime $ time
  return day

select' :: (Select a) => Connection -> Table -> [Param] -> Where -> [Text] -> IO [a]
select' conn table params where' values = do
  xs <- query conn (toSelQ table params where') values
  return xs

selectOnly' :: (FromField a) => Connection -> Table -> [Param] -> Where -> [Text] -> IO [a]
selectOnly' conn table params where' values = do
  xs <- query conn (toSelQ table params where') values
  return $ fmap fromOnly xs

selectBS' :: Connection -> Table -> [Param] -> Where -> [Text] -> IO [ByteString]
selectBS' conn table params where' values = do
  xs <- query conn (toSelQ table params where') values
  return $ fmap (fromBinary . fromOnly) xs  

selectLimit' :: (Select a) => Connection -> Table -> String -> Integer -> Integer -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> IO [a]
selectLimit' conn defTable defOrderBy page limitNumber params defWhere defValues filterArgs sortArgs = do
  let table   = intercalate " "     $ [defTable] ++ fmap tableFil filterArgs ++ fmap tableSort sortArgs
  let where'  = intercalate " AND " $ [defWhere] ++ fmap whereFil filterArgs
  let orderBy = intercalate ","     $ fmap orderBySort sortArgs ++ [defOrderBy]
  let values  = (concatMap fst . fmap valuesFil $ filterArgs) ++ defValues ++ (concatMap snd . fmap valuesFil $ filterArgs)
  xs <- query conn (toSelLimQ table orderBy page limitNumber params where') values
  return xs

updateInDb' :: Connection -> String -> String -> String -> [Text] -> IO ()
updateInDb' conn table set where' values = do
  _ <- execute conn (toUpdQ table set where') values
  return ()

deleteFromDb' :: Connection -> Table -> Where -> [Text] -> IO ()
deleteFromDb' conn table where' values = do
  _ <- execute conn (toDelQ table where') values
  return ()

isExistInDb' :: Connection -> String -> String -> String -> [Text] -> IO Bool
isExistInDb' conn table checkName where' values = do
  onlyChecks  <- query conn (toExQ table checkName where') values
  Only isExist <- checkOneM onlyChecks
  return isExist

insertReturn' :: Connection -> String -> String -> [String] -> [Text] -> IO Id
insertReturn' conn table returnName insNames insValues = do
  onlyXs <- query conn ( toInsRetQ table returnName insNames ) insValues
  Only num <- checkOneM onlyXs
  return num

insertByteaInDb' :: Connection -> String -> String -> [String] -> ByteString -> IO Id
insertByteaInDb' conn table returnName insNames bs = do
  onlyXs <- query conn ( toInsRetQ table returnName insNames ) [Binary bs]
  Only num <- checkOneM onlyXs
  return num

insertMany' :: Connection -> String -> [String] -> [(Integer,Integer)] -> IO ()
insertMany' conn table insNames insValues = do
  _ <- executeMany conn ( toInsManyQ table insNames ) insValues
  return ()

getTokenKey' :: IO String
getTokenKey' = do
  gen <- getStdGen
  _ <- newStdGen
  return . take 6 $ randomRs ('a','z') gen

 

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


makeMyPicUrl :: PictureId -> Text
makeMyPicUrl picId = pack $ "http://localhost:3000/picture/" ++ show picId

inPicIdUrl :: PictureId -> PicIdUrl
inPicIdUrl picId    = PicIdUrl picId (makeMyPicUrl picId)

fromTwoIdsToPair :: TwoIds -> (Integer,Integer)
fromTwoIdsToPair (TwoIds a b) = (a,b)

numToTxt :: Id -> Text
numToTxt = pack . show