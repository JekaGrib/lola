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
import ConnectDB  (ConnDB(..))
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



data MethodsHandle m = MethodsHandle 
  { hConf              :: Config,
    hLog               :: LogHandle m ,
    selectTxt          :: Table -> [String] -> String -> [Text] -> m [Only Text],
    selectDay          :: Table -> [String] -> String -> [Text] -> m [Only Day],
    selectBS           :: Table -> [String] -> String -> [Text] -> m [Only (Binary ByteString)],
    selectNum          :: Table -> [String] -> String -> [Text] -> m [Only Id],
    selectTwoIds       :: Table -> [String] -> String -> [Text] -> m [TwoIds],
    selectAuth         :: Table -> [String] -> String -> [Text] -> m [Auth],
    selectCat          :: Table -> [String] -> String -> [Text] -> m [Cat],
    selectTag          :: Table -> [String] -> String -> [Text] -> m [Tag],
    selectAuthor       :: Table -> [String] -> String -> [Text] -> m [Author],
    selectComment      :: Table -> [String] -> String -> [Text] -> m [Comment],
    selectUser         :: Table -> [String] -> String -> [Text] -> m [User],
    selectPostInfo     :: Table -> [String] -> String -> [Text] -> m [PostInfo],
    selectDraft        :: Table -> [String] -> String -> [Text] -> m [Draft],
    selectPost         :: Table -> [String] -> String -> [Text] -> m [Post],
    selectLimitComment :: Table -> String -> Integer -> Integer -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> m [Comment],
    selectLimitDraft   :: Table -> String -> Integer -> Integer -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> m [Draft],
    selectLimitPost    :: Table -> String -> Integer -> Integer -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> m [Post],
    updateInDb         :: Table -> String -> String -> [Text] -> m (),
    deleteFromDb       :: Table -> String -> [Text] -> m (),
    isExistInDb        :: Table -> String -> String -> [Text] -> m Bool,
    insertReturn       :: Table -> String -> [String] -> [Text] -> m Integer,
    insertByteaInDb    :: Table -> String -> [String] -> ByteString -> m Integer,
    insertMany         :: Table -> [String] -> [(Integer,Integer)] -> m (),
    httpAction         :: HT.Request -> m (HT.Response BSL.ByteString),
    getDay             :: m String,
    getTokenKey        :: m String,
    withTransactionDB  :: forall a. m a -> m a
    }

makeMethodsH :: Config -> LogHandle IO -> MethodsHandle IO
makeMethodsH conf hLog = let ConnDB conn _ = cConnDB conf in
  MethodsHandle 
    conf 
    hLog 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (select' conn) 
    (selectLimit' conn) 
    (selectLimit' conn) 
    (selectLimit' conn) 
    (updateInDb' conn) 
    (deleteFromDb' conn) 
    (isExistInDb' conn) 
    (insertReturn' conn) 
    (insertByteaInDb' conn) 
    (insertMany' conn) 
    HT.httpLBS 
    getDay'   
    getTokenKey' 
    (withTransaction conn)

data ResponseInfo = ResponseInfo {resStatus :: Status, resHeaders :: ResponseHeaders, resBuilder :: Builder}

okHelper :: (MonadCatch m, ToJSON a) => a -> ExceptT ReqError m ResponseInfo
okHelper toJ = return $ ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")]  (lazyByteString . encode $ toJ)


checkOneE :: (MonadCatch m,Show a) => MethodsHandle m -> m [a] -> ExceptT ReqError m a
checkOneE h m = do
  lift $ logDebug (hLog h) $ "Select data from DB." 
  xs <- catchDbErr $ lift m
  case xs of
    []           -> throwE $ DatabaseError "Empty output"
    [x] -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "Output not single" ++ show xs

checkMaybeOneE :: (MonadCatch m,Show a) => MethodsHandle m -> m [a] -> ExceptT ReqError m (Maybe a)
checkMaybeOneE h m = do
  lift $ logDebug (hLog h) $ "Select data from DB." 
  xs <- catchDbErr $ lift m
  case xs of
    []           -> do
      lift $ logInfo (hLog h) $ "Received empty data from DB"
      return Nothing
    [x] -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return (Just x)
    _            -> throwE $ DatabaseError $ "Output not single" ++ show xs

checkOneIfExistE :: (MonadCatch m,Show a) => MethodsHandle m -> 
  (Table -> [Param] -> Where -> [Text] -> m [a]) ->
  Table -> [Param] -> Where -> Text -> ExceptT ReqError m a
checkOneIfExistE h func table params where' value = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table 
  xs <- catchDbErr $ lift $ func table params where' [value]
  case xs of
    []           -> throwE $ SimpleError $ (where' \\ "???") ++ unpack value ++ " doesn`t exist"
    (x:[]) -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "Output not single" ++ show xs

checkListE :: (MonadCatch m,Show a) => MethodsHandle m -> m [a] -> ExceptT ReqError m [a]
checkListE h m = do
  lift $ logDebug (hLog h) $ "Select data from DB."
  xs <- catchDbErr $ lift m
  lift $ logInfo (hLog h) $ "Data received from DB"
  return xs

updateInDbE :: (MonadCatch m) => MethodsHandle m -> Table -> Set -> Where -> [Text] -> ExceptT ReqError m ()
updateInDbE h table set where' values = catchDbErr $ do
  lift $ updateInDb h table set where' values

deleteFromDbE :: (MonadCatch m) => MethodsHandle m -> Table -> Where -> [Text] -> ExceptT ReqError m ()
deleteFromDbE h table where' values = catchDbErr $ do
  lift $ deleteFromDb h table where' values   

isExistInDbE :: (MonadCatch m) => MethodsHandle m  -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
isExistInDbE h table checkName where' values = do
  lift $ logDebug (hLog h) $ "Checking existence entity (" ++ checkName ++ ") in the DB"
  check  <- catchDbErr $ lift $ isExistInDb h table checkName where' values 
  case check of
    True -> do
      lift $ logInfo (hLog h) $ "Entity (" ++ checkName ++ ") exist"
      return ()
    False -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " doesn`t exist."
  
ifExistInDbThrowE :: (MonadCatch m) => MethodsHandle m  -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
ifExistInDbThrowE h table checkName where' values = do
  lift $ logDebug (hLog h) $ "Checking existence entity (" ++ checkName ++ ") in the DB"
  check  <- catchDbErr $ lift $ isExistInDb h table checkName where' values 
  case check of
    True -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " already exist in " ++ table
    False -> do
      lift $ logInfo (hLog h) $ "Entity (" ++ checkName ++ ") doesn`t exist"
      return ()

insertReturnE :: (MonadCatch m) => MethodsHandle m  -> Table -> String -> [String] -> [Text] -> ExceptT ReqError m Integer
insertReturnE h table returnName insNames insValues =  catchDbErr $
  lift $ insertReturn h table returnName insNames insValues


insertByteaInDbE :: (MonadCatch m) => MethodsHandle m  -> Table -> String -> [String] -> ByteString -> ExceptT ReqError m Integer
insertByteaInDbE h table returnName insNames bs =  catchDbErr 
  $ lift $ insertByteaInDb h table returnName insNames bs


insertManyE :: (MonadCatch m) => MethodsHandle m  -> Table -> [String] -> [(Integer,Integer)] -> ExceptT ReqError m ()
insertManyE h table insNames insValues = catchDbErr $ do
  lift $ insertMany h table insNames insValues

withTransactionDBE :: (MonadCatch m) => MethodsHandle m  -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchDbErr . lift . withTransactionDB h

checkOneM :: (MonadCatch m) => [a] -> m a
checkOneM xs = case xs of
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

{-selectOnly' :: (Only a) => Connection -> String -> [String] -> String -> [Text] -> IO [a]
selectOnly' conn table params where' values = do
  xs <- query conn (toSelQ table params where') values
  return (fmap fromOnly xs)-}

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
  Only check <- checkOneM onlyChecks
  return check

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