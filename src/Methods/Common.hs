module Methods.Common where

import Api.Response (CommentIdTextUserResponse (..), PicIdUrl (..), TagResponse (..),Created(..),CreatedUser(..))
import Conf (Config (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA1)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (getZonedTime, localDay, zonedTimeToLocalTime)
import Logger
import Network.HTTP.Types (Header, ResponseHeaders, Status, hLocation, status200, status201, status204)
import Error (ReqError (..), catchDbErr)
import Psql.Selecty (Comment (..), Tag (..))
import System.Random (getStdGen, newStdGen, randomRs)
import Types

-- common logic functions:

data ResponseInfo = ResponseInfo {resStatus :: Status, resHeaders :: ResponseHeaders, resBS :: BSL.ByteString}
  deriving (Eq)

instance Show ResponseInfo where
  show (ResponseInfo s h b) = "ResponseInfo Status: " ++ show s ++ ". Headers: " ++ show h ++ ". ByteString: " ++ show b

jsonHeader :: Header
jsonHeader = ("Content-Type", "application/json; charset=utf-8")

textHeader :: Header
textHeader = ("Content-Type", "text/html")

okHelper :: (MonadCatch m, ToJSON a) => a -> ExceptT ReqError m ResponseInfo
okHelper toJ = return $ ResponseInfo status200 [jsonHeader] $ encode toJ

ok201Helper :: (MonadCatch m) => Config -> String -> Id -> ExceptT ReqError m ResponseInfo
ok201Helper conf entity iD = 
  return $ ResponseInfo status201 [jsonHeader, (hLocation, url entity iD)] $ encode $ Created iD entity
    where
      url name number = makeMyUrl conf (toPlural name ++ "/" ++ show number)

ok201UserHelper :: (MonadCatch m) => Config -> Text -> Id -> ExceptT ReqError m ResponseInfo
ok201UserHelper conf token iD = 
  return $ ResponseInfo status201 [jsonHeader, (hLocation, url iD)] $ encode $ CreatedUser iD token
    where
      url number = makeMyUrl conf ("users/" ++ show number)


ok204Helper :: (MonadCatch m) => ExceptT ReqError m ResponseInfo
ok204Helper = return $ ResponseInfo status204 [textHeader] "Status 204 No data"

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

checkMaybeOneE :: (MonadCatch m, Show a) => [a] -> ExceptT ReqError m (Maybe a)
checkMaybeOneE xs = case xs of
  [] -> return Nothing
  [x] -> return (Just x)
  _ -> throwE $ DatabaseError $ "Output not single" ++ show xs

catchOneSelE :: (MonadCatch m, Show a) => LogHandle m -> m [a] -> ExceptT ReqError m a
catchOneSelE logH m =
  catchSelE logH m >>= checkOneE

catchMaybeOneSelE :: (MonadCatch m, Show a) => LogHandle m -> m [a] -> ExceptT ReqError m (Maybe a)
catchMaybeOneSelE logH m =
  catchSelE logH m >>= checkMaybeOneE

catchDbErrE :: (MonadCatch m) => m a -> ExceptT ReqError m a
catchDbErrE = catchDbErr . lift

catchUpdE :: (MonadCatch m) => LogHandle m -> m () -> ExceptT ReqError m ()
catchUpdE logH m = do
  lift $ logDebug logH "Update data in DB."
  catchDbErrE m
  lift $ logInfo logH "Data updated in DB"

catchInsRetE ::
  (MonadCatch m) =>
  LogHandle m ->
  m Id ->
  ExceptT ReqError m Id
catchInsRetE logH m = do
  lift $ logDebug logH "Insert data in the DB"
  i <- catchDbErrE m
  lift $ logInfo logH $ "DB return id: " ++ show i
  return i

catchTransactE :: (MonadCatch m) => LogHandle m -> m a -> ExceptT ReqError m a
catchTransactE logH m = do
  lift $ logDebug logH "Open transaction in DB to do several actions"
  a <- catchDbErrE m
  lift $ logInfo logH "Transaction closed. Several actions in DB finished."
  return a

-- common IO handle functions:

getDay' :: IO Day
getDay' = do
  time <- getZonedTime
  let day = localDay . zonedTimeToLocalTime $ time
  return day

generateTokenKey' :: IO TokenKey
generateTokenKey' = do
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
inCommResp (Comment idCom usId txt _) = CommentIdTextUserResponse idCom txt usId

inTagResp :: Tag -> TagResponse
inTagResp (Tag tagId tagName) = TagResponse tagId tagName

makeMyPicUrl :: Config -> PictureId -> Text
makeMyPicUrl conf picId = pack $ "http://" ++ cServHost conf ++ ":" ++ show (cServPort conf) ++ "/pictures/" ++ show picId

inPicIdUrl :: Config -> PictureId -> PicIdUrl
inPicIdUrl conf picId = PicIdUrl picId (makeMyPicUrl conf picId)

numToTxt :: Id -> Text
numToTxt = pack . show

makeMyUrl :: Config -> String -> ByteString
makeMyUrl conf str = fromString $ "http://" ++ cServHost conf ++ ":" ++ show (cServPort conf) ++ "/" ++ str

toPlural :: String -> String
toPlural "category" = "categories"
toPlural entity = entity ++ "s"
 
