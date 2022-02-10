{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}





module Methods.User where
          
import           Api.Response (UserTokenResponse(..),UserResponse(..),OkResponse(..))
import           Logger
import           Types
import           Oops (ReqError)
import           Methods.Common
import Methods.Common.Select (User(..))
import ParseQueryStr (CreateUser(..),DeleteUser(..))
import           Data.Text                      ( pack,Text )
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( MonadCatch)
import Methods.Common.DeleteMany (deleteAllAboutDrafts)
import qualified Methods.Common.DeleteMany (Handle,makeH)
import           Data.Time.Calendar             ( showGregorian)
import  Conf (Config(..),extractConn)
import           Database.PostgreSQL.Simple (withTransaction)




data Handle m = Handle 
  { hConf              :: Config,
    hLog               :: LogHandle m ,
    selectNum          :: Table -> [Param] -> Where -> [Text] -> m [Id],
    selectUser         :: Table -> [Param] -> Where -> [Text] -> m [User],
    updateInDb         :: Table -> String -> String -> [Text] -> m (),
    deleteFromDb       :: Table -> String -> [Text] -> m (),
    isExistInDb        :: Table -> String -> String -> [Text] -> m Bool,
    insertReturn       :: Table -> String -> [String] -> [Text] -> m Integer,
    getDay             :: m String,
    getTokenKey        :: m String,
    withTransactionDB  :: forall a. m a -> m a,
    hDelMany :: Methods.Common.DeleteMany.Handle m
    }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH = let conn = extractConn conf in
  Handle 
    conf 
    logH 
    (selectOnly' conn) 
    (select' conn) 
    (updateInDb' conn) 
    (deleteFromDb' conn) 
    (isExistInDb' conn) 
    (insertReturn' conn) 
    getDay'   
    getTokenKey' 
    (withTransaction conn)
    (Methods.Common.DeleteMany.makeH conf )


createUser :: (MonadCatch m) => Handle m -> CreateUser -> ExceptT ReqError m ResponseInfo
createUser h (CreateUser pwdParam fNameParam lNameParam picIdNum) = do
  let picIdParam = numToTxt picIdNum
  day <- lift $ getDay h
  tokenKey <- lift $ getTokenKey h
  let hashPwdParam = txtSha1 pwdParam
  let insNames  = ["password"    ,"first_name","last_name","user_pic_id"  ,"user_create_date","admin","token_key"]
  let insValues = [ hashPwdParam ,fNameParam  ,lNameParam ,picIdParam   ,pack day          ,"FALSE",pack tokenKey]
  usId <-  insertReturnE h "users" "user_id" insNames insValues
  lift $ logDebug (hLog h) $ "DB return user_id:" ++ show usId ++ "and token key"
  lift $ logInfo (hLog h) $ "User_id: " ++ show usId ++ " created"
  let usToken = pack $ show usId ++ "." ++ strSha1 tokenKey ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
  okHelper $ UserTokenResponse {tokenUTR = usToken, user_idUTR = usId, first_nameUTR = fNameParam, last_nameUTR = lNameParam, user_pic_idUTR = picIdNum, user_pic_urlUTR = makeMyPicUrl (hConf h) picIdNum, user_create_dateUTR = pack day}
  
getUser :: (MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m ResponseInfo 
getUser h usIdNum = do
  let selectParams = ["first_name","last_name","user_pic_id","user_create_date"]
  User fName lName picId usCreateDate <- checkOneIfExistE (hLog h) (selectUser h) "users" selectParams "user_id=?" (numToTxt usIdNum)
  okHelper $ UserResponse {user_id = usIdNum, first_name = fName, last_name = lName, user_pic_id = picId, user_pic_url = makeMyPicUrl (hConf h) picId, user_create_date = pack . showGregorian $ usCreateDate}

deleteUser :: (MonadCatch m) => Handle m -> DeleteUser -> ExceptT ReqError m ResponseInfo 
deleteUser h (DeleteUser usIdNum) = do
  let usIdParam = pack . show $ usIdNum
  isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
  let updateCom = updateInDb h "comments" "user_id=?" "user_id=?" [pack . show $ cDefUsId (hConf h),usIdParam]
  let deleteUs = deleteFromDb h "users" "user_id=?" [usIdParam]
  maybeAuId <- checkMaybeOneE (hLog h) $ selectNum h "authors" ["author_id"] "user_id=?" [usIdParam]
  case maybeAuId of
    Just authorId -> do
      let updatePost = updateInDb h "posts" "author_id=?" "author_id=?" [pack . show $ cDefAuthId  (hConf h),pack . show $ (authorId :: Integer)]
      draftsIds <- checkListE (hLog h) $ selectNum h "drafts" ["draft_id"] "author_id=?" [pack . show $ authorId]  
      let deleteDr = deleteAllAboutDrafts (hDelMany h) draftsIds
      let deleteAu = deleteFromDb h "authors" "author_id=?" [pack . show $ authorId]
      withTransactionDBE h (updateCom >> updatePost >> deleteDr >> deleteAu >> deleteUs)
    Nothing -> 
      withTransactionDBE h (updateCom >> deleteUs)
  lift $ logInfo (hLog h) $ "User_id: " ++ show usIdNum ++ " deleted"
  okHelper $ OkResponse {ok = True}

isExistInDbE :: (MonadCatch m) => Handle m  -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
isExistInDbE h = checkIsExistE (hLog h) (isExistInDb h)

insertReturnE :: (MonadCatch m) => Handle m -> Table -> String -> [String] -> [Text] -> ExceptT ReqError m Integer
insertReturnE h = checkInsRetE (hLog h) (insertReturn h)

withTransactionDBE :: (MonadCatch m) => Handle m  -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h