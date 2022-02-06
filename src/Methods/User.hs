--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Methods.User where
          
import           Api.Response (UserTokenResponse(..),UserResponse(..),OkResponse(..))
import           Logger
import           Types
import           Oops (ReqError)
import           Methods.Handle
import Methods.Handle.Select (User(..))
import ParseQueryStr (CreateUser(..),DeleteUser(..))
import Conf (Config(..))
import           Network.Wai (Request,ResponseReceived,Response,responseBuilder,strictRequestBody,pathInfo)
import           Data.Text                      ( pack )
import           Database.PostgreSQL.Simple (Only(..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( catch, throwM, MonadCatch)
import Methods.Post (deleteAllAboutDrafts)
import           Data.Time.Calendar             ( showGregorian)


createUser :: (MonadCatch m) => MethodsHandle m -> CreateUser -> ExceptT ReqError m ResponseInfo
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
  okHelper $ UserTokenResponse {tokenUTR = usToken, user_idUTR = usId, first_nameUTR = fNameParam, last_nameUTR = lNameParam, user_pic_idUTR = picIdNum, user_pic_urlUTR = makeMyPicUrl picIdNum, user_create_dateUTR = pack day}
  
getUser :: (MonadCatch m) => MethodsHandle m -> UserId -> ExceptT ReqError m ResponseInfo 
getUser h usIdNum = do
  let selectParams = ["first_name","last_name","user_pic_id","user_create_date"]
  User fName lName picId usCreateDate <- selectOneIfExistE h "users" selectParams "user_id=?" (numToTxt usIdNum)
  okHelper $ UserResponse {user_id = usIdNum, first_name = fName, last_name = lName, user_pic_id = picId, user_pic_url = makeMyPicUrl picId, user_create_date = pack . showGregorian $ usCreateDate}

deleteUser :: (MonadCatch m) => MethodsHandle m -> DeleteUser -> ExceptT ReqError m ResponseInfo 
deleteUser h (DeleteUser usIdNum) = do
  let usIdParam = pack . show $ usIdNum
  isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
  let updateCom = updateInDb h "comments" "user_id=?" "user_id=?" [pack . show $ (cDefUsId $ hConf h),usIdParam]
  let deleteUs = deleteFromDb h "users" "user_id=?" [usIdParam]
  maybeAuId <- selectMaybeOneE h "authors" ["author_id"] "user_id=?" [usIdParam]
  case maybeAuId of
    Just (Only authorId) -> do
      let updatePost = updateInDb h "posts" "author_id=?" "author_id=?" [pack . show $ (cDefAuthId $ hConf h),pack . show $ (authorId :: Integer)]
      onlyDraftsIds <- selectListFromDbE h "drafts" ["draft_id"] "author_id=?" [pack . show $ authorId]  
      let draftsIds = fmap fromOnly onlyDraftsIds
      let deleteDr = deleteAllAboutDrafts h $ draftsIds
      let deleteAu = deleteFromDb h "authors" "author_id=?" [pack . show $ authorId]
      withTransactionDBE h (updateCom >> updatePost >> deleteDr >> deleteAu >> deleteUs)
    Nothing -> 
      withTransactionDBE h (updateCom >> deleteUs)
  lift $ logInfo (hLog h) $ "User_id: " ++ show usIdNum ++ " deleted"
  okHelper $ OkResponse {ok = True}
