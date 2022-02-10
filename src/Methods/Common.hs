{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Methods.Common where
          
import           Api.Response (TagResponse(..), CommentIdTextUserResponse(..),PicIdUrl(..))
import           Logger
import           Types
import Methods.Common.Select (Select,Comment(..),Tag(..))
import           Oops
import Methods.Common.ToQuery (toSelQ,toSelLimQ,toUpdQ,toDelQ,toExQ,toInsRetQ,toInsManyQ)
import           Network.HTTP.Types             ( status200, Status, ResponseHeaders )
import           Data.Aeson (ToJSON,encode)
import           Data.Text                      ( pack, unpack, Text )
import           Data.ByteString.Builder        ( lazyByteString, Builder)
import           Database.PostgreSQL.Simple (query, execute, executeMany,Connection,Only(..),Binary(..))
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Data.Time.LocalTime
import           Data.Time.Calendar             ( showGregorian)
import           Data.String                    ( fromString )
import           Data.List                      ( intercalate, (\\) )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Data.ByteString                ( ByteString )
import           Control.Monad.Catch            ( throwM, MonadCatch)
import           Crypto.Hash                    (hash,Digest)
import Crypto.Hash.Algorithms (SHA1)
import System.Random (getStdGen,newStdGen,randomRs)
import Methods.Post.LimitArg
import  Conf (Config(..))


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

catchDbErrE :: (MonadCatch m) => m a -> ExceptT ReqError m a
catchDbErrE = catchDbErr . lift

checkUpdE :: (MonadCatch m) => LogHandle m -> m () -> ExceptT ReqError m ()
checkUpdE logH m = do
  lift $ logDebug logH "Update data in DB."
  catchDbErrE m
  lift $ logInfo logH "Data updated in DB"


checkIsExistE :: (MonadCatch m) => LogHandle m ->
  (String -> String -> String -> [Text] -> m Bool) -> 
   String -> String -> String -> [Text] -> ExceptT ReqError m ()
checkIsExistE logH func table checkName where' values = do
  lift $ logDebug logH $ "Checking existence entity (" ++ checkName ++ ") in the DB"
  isExist  <- catchDbErrE $ func table checkName where' values 
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

makeMyPicUrl :: Config -> PictureId -> Text
makeMyPicUrl conf picId = pack $ "http://" ++ cServHost conf ++ ":" ++ show (cServPort conf) ++ "/picture/" ++ show picId

inPicIdUrl :: Config -> PictureId -> PicIdUrl
inPicIdUrl conf picId = PicIdUrl picId (makeMyPicUrl conf picId)

numToTxt :: Id -> Text
numToTxt = pack . show