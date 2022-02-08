{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}




module Methods.Tag where
          
import           Api.Response (TagResponse(..),OkResponse(..))
import           Logger
import           Types
import           Oops
import           Methods.Handle
import ParseQueryStr (CreateTag(..),UpdateTag(..),DeleteTag(..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( MonadCatch)


createTag :: (Monad m,MonadCatch m) => Handle m -> CreateTag -> ExceptT ReqError m ResponseInfo
createTag h (CreateTag tagNameParam) = do
  tagId <-  insertReturnE h "tags" "tag_id" ["tag_name"] [tagNameParam]
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagId ++ " created"
  okHelper $ TagResponse tagId tagNameParam
  
getTag :: (Monad m,MonadCatch m) => Handle m -> TagId -> ExceptT ReqError m ResponseInfo 
getTag h tagIdNum = do
  tagName <- checkOneIfExistE h (selectTxt h) "tags" ["tag_name"] "tag_id=?" (numToTxt tagIdNum)
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " sending in response"
  okHelper $ TagResponse tagIdNum tagName
  
updateTag :: (Monad m,MonadCatch m) => Handle m -> UpdateTag -> ExceptT ReqError m ResponseInfo 
updateTag h (UpdateTag tagIdNum tagNameParam) = do
  let tagIdParam = numToTxt tagIdNum
  isExistInDbE h "tags" "tag_id" "tag_id=?" [tagIdParam] 
  updateInDbE h "tags" "tag_name=?" "tag_id=?" [tagNameParam,tagIdParam]
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " updated"
  okHelper $ TagResponse tagIdNum tagNameParam

deleteTag :: (Monad m,MonadCatch m) => Handle m -> DeleteTag -> ExceptT ReqError m ResponseInfo 
deleteTag h (DeleteTag tagIdNum) = do
  let tagIdParam = numToTxt tagIdNum
  isExistInDbE h "tags" "tag_id" "tag_id=?" [tagIdParam]
  let deleteDrTg = deleteFromDb h "draftstags" "tag_id=?" [tagIdParam] 
  let deletePosTg = deleteFromDb h "poststags" "tag_id=?" [tagIdParam] 
  let deleteTg = deleteFromDb h "tags" "tag_id=?" [tagIdParam]
  withTransactionDBE h (deleteDrTg >> deletePosTg >> deleteTg)
  lift $ logInfo (hLog h) $ "Tag_id: " ++ show tagIdNum ++ " deleted"
  okHelper $ OkResponse {ok = True}

