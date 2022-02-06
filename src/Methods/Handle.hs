--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}




module Methods.Handle where
          
import           Api.Response
import           Api.Request
import           Logger
import           Types
import Methods.Handle.Select
import           Oops
import  Conf (Config(..)) 
import ParseQueryStr 
import Methods.Handle.ToQuery (toSelQ,toSelLimQ,toUpdQ,toDelQ,toExQ,toInsRetQ,toInsManyQ)
import CheckJsonReq (checkDraftReqJson)
import ConnectDB  (tryConnect,ConnDB(..))
import Methods.Handle.ToQuery (toSelQ,toSelLimQ,toUpdQ,toDelQ,toExQ,toInsRetQ,toInsManyQ)
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Network.HTTP.Types             ( status200, status404, Status, ResponseHeaders )
import           Data.Aeson (ToJSON,encode)
import           Data.Text                      ( pack, unpack, Text )
import           Data.ByteString.Builder        ( lazyByteString, Builder, toLazyByteString )
import           Database.PostgreSQL.Simple (query, withTransaction, execute, executeMany,Connection,Only(..),Binary(Binary))
import qualified Network.HTTP.Simple            as HT
import           Data.Time.LocalTime
import           Data.Time.Calendar             ( showGregorian)
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



data MethodsHandle m = MethodsHandle 
  { hConf             :: Config,
    hLog              :: LogHandle m ,
    select      :: forall a. (Select a) => String -> [String] -> String -> [Text] -> m [a],
    selectLimit :: forall a. (Select a) => String -> String -> Integer -> Integer -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> m [a],
    updateInDb        :: String -> String -> String -> [Text] -> m (),
    deleteFromDb      :: String -> String -> [Text] -> m (),
    isExistInDb       :: String -> String -> String -> [Text] -> m Bool,
    insertReturn  :: String -> String -> [String] -> [Text] -> m [Integer],
    insertByteaInDb   :: String -> String -> [String] -> ByteString -> m [Integer],
    insertMany    :: String -> [String] -> [(Integer,Integer)] -> m (),
    httpAction        :: HT.Request -> m (HT.Response BSL.ByteString),
    getDay            :: m String,
    getTokenKey       :: m String,
    withTransactionDB :: forall a. m a -> m a
    }

makeMethodsHWithConn :: Config -> LogHandle IO -> Connection -> MethodsHandle IO
makeMethodsHWithConn conf hLog conn =
  MethodsHandle conf hLog (select' conn) (selectLimit' conn) (updateInDb' conn) (deleteFromDb' conn) (isExistInDb' conn) (insertReturn' conn) (insertByteaInDb' conn) (insertMany' conn) HT.httpLBS getDay'   getTokenKey' (withTransaction conn)

data ResponseInfo = ResponseInfo {resStatus :: Status, resHeaders :: ResponseHeaders, resBuilder :: Builder}

okHelper :: (Monad m, MonadCatch m, ToJSON a) => a -> ExceptT ReqError m ResponseInfo
okHelper toJ = return $ ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")]  (lazyByteString . encode $ toJ)



selectOneE :: (Monad m, MonadCatch m, Select b) => MethodsHandle m -> Table -> [Param] -> Where -> [Text] -> ExceptT ReqError m b
selectOneE h table params where' values = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table 
  xs <- catchDbErr $ lift $ select h table params where' values
  case xs of
    []           -> throwE $ DatabaseError "DatabaseError.Empty output"
    [x] -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs

selectOneIfExistE :: (Monad m, MonadCatch m, Select b) => MethodsHandle m -> Table -> [Param] -> Where -> Text -> ExceptT ReqError m b
selectOneIfExistE h table params where' value = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table 
  xs <- catchDbErr $ lift $ select h table params where' [value]
  case xs of
    []           -> throwE $ SimpleError $ (where' \\ "???") ++ unpack value ++ " doesn`t exist"
    [x] -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs

selectMaybeOneE :: (Monad m, MonadCatch m, Select b) => MethodsHandle m  -> String -> [String] -> String -> [Text] -> ExceptT ReqError m (Maybe b)
selectMaybeOneE h table params where' values = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table 
  xs <- catchDbErr $ lift $ select h table params where' values
  case xs of
    []           -> do
      lift $ logInfo (hLog h) $ "Received empty data from DB"
      return Nothing
    [x] -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return (Just x)
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs

selectListFromDbE :: (Monad m, MonadCatch m,Select b) => MethodsHandle m  -> String -> [String] -> String -> [Text] -> ExceptT ReqError m [b]
selectListFromDbE h table params where' values = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table
  xs <- catchDbErr $ lift $ select h table params where' values
  lift $ logInfo (hLog h) $ "Data received from DB"
  return xs 

selectListLimitFromDbE :: (Monad m, MonadCatch m,Select b) => MethodsHandle m  -> String -> String -> Integer -> Integer -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> ExceptT ReqError m [b]
selectListLimitFromDbE h defTable defOrderBy page limitNumber params defWhere defValues filterArgs sortArgs =  do
  lift $ logDebug (hLog h) $ "Select data from DB."
  xs <- catchDbErr $ lift $ selectLimit h defTable defOrderBy page limitNumber params defWhere defValues filterArgs sortArgs
  lift $ logInfo (hLog h) $ "Data received from DB"
  return xs

updateInDbE :: (Monad m, MonadCatch m) => MethodsHandle m -> Table -> Set -> Where -> [Text] -> ExceptT ReqError m ()
updateInDbE h table set where' values = catchDbErr $ do
  lift $ updateInDb h table set where' values

deleteFromDbE :: (Monad m, MonadCatch m) => MethodsHandle m -> Table -> Where -> [Text] -> ExceptT ReqError m ()
deleteFromDbE h table where' values = catchDbErr $ do
  lift $ deleteFromDb h table where' values   

isExistInDbE :: (Monad m, MonadCatch m) => MethodsHandle m  -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
isExistInDbE h table checkName where' values = do
  lift $ logDebug (hLog h) $ "Checking existence entity (" ++ checkName ++ ") in the DB"
  check  <- catchDbErr $ lift $ isExistInDb h table checkName where' values 
  case check of
    True -> do
      lift $ logInfo (hLog h) $ "Entity (" ++ checkName ++ ") exist"
      return ()
    False -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " doesn`t exist."
  
ifExistInDbThrowE :: (Monad m, MonadCatch m) => MethodsHandle m  -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
ifExistInDbThrowE h table checkName where' values = do
  lift $ logDebug (hLog h) $ "Checking existence entity (" ++ checkName ++ ") in the DB"
  check  <- catchDbErr $ lift $ isExistInDb h table checkName where' values 
  case check of
    True -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " already exist in " ++ table
    False -> do
      lift $ logInfo (hLog h) $ "Entity (" ++ checkName ++ ") doesn`t exist"
      return ()

insertReturnE :: (Monad m, MonadCatch m) => MethodsHandle m  -> String -> String -> [String] -> [Text] -> ExceptT ReqError m Integer
insertReturnE h table returnName insNames insValues =  do
  xs <- catchDbErr $ lift $ insertReturn h table returnName insNames insValues
  case xs of
    []           -> throwE $ DatabaseError "DatabaseError.Empty output"
    [x] -> do 
      lift $ logInfo (hLog h) $ "Data received from DB. " ++ returnName ++ ": " ++ show x
      return x
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs

insertByteaInDbE :: (Monad m, MonadCatch m) => MethodsHandle m  -> String -> String -> [String] -> ByteString -> ExceptT ReqError m Integer
insertByteaInDbE h table returnName insNames bs =  do
  xs <- catchDbErr $ lift $ insertByteaInDb h table returnName insNames bs
  case xs of
    []           -> throwE $ DatabaseError "DatabaseError.Empty output"
    [x] -> do 
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs

insertManyE :: (Monad m, MonadCatch m) => MethodsHandle m  -> String -> [String] -> [(Integer,Integer)] -> ExceptT ReqError m ()
insertManyE h table insNames insValues = catchDbErr $ do
  lift $ insertMany h table insNames insValues

withTransactionDBE :: (Monad m, MonadCatch m) => MethodsHandle m  -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchDbErr . lift . withTransactionDB h

checkSingleOutPut :: (MonadCatch m) => [a] -> m a
checkSingleOutPut xs = case xs of
  [] -> throwM UnexpectedEmptyDbOutPutException
  (x:[]) -> return x
  _ -> throwM UnexpectedMultipleDbOutPutException

-- IO handle functions:

getDay' :: IO String
getDay' = do
  time    <- getZonedTime
  let day = showGregorian . localDay . zonedTimeToLocalTime $ time
  return day

select' :: (Select a) => Connection -> String -> [String] -> String -> [Text] -> IO [a]
select' conn table params where' values = do
  xs <- query conn (toSelQ table params where') values
  return xs

selectLimit' :: (Select a) => Connection -> String -> String -> Integer -> Integer -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> IO [a]
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
  Only check <- checkSingleOutPut onlyChecks
  return check

insertReturn' :: Connection -> String -> String -> [String] -> [Text] -> IO [Integer]
insertReturn' conn table returnName insNames insValues = do
  xs <- query conn ( toInsRetQ table returnName insNames ) insValues 
  return (fmap fromOnly xs)

insertByteaInDb' :: Connection -> String -> String -> [String] -> ByteString -> IO [Integer]
insertByteaInDb' conn table returnName insNames bs = do
  xs <- query conn ( toInsRetQ table returnName insNames ) [Binary bs] 
  return (fmap fromOnly xs)

insertMany' :: Connection -> String -> [String] -> [(Integer,Integer)] -> IO ()
insertMany' conn table insNames insValues = do
  _ <- executeMany conn ( toInsManyQ table insNames ) insValues
  return ()

getTokenKey' :: IO String
getTokenKey' = do
  gen <- getStdGen
  _ <- newStdGen
  return . take 6 $ randomRs ('a','z') gen

 

-- clear functions:

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