--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}




module App where
          
import           Api.Response
import           Api.Request
import           Logger
import           Types
import           Oops
import           LimitArg
import ParseQueryStr hiding (tryReadNum)
import ToQuery (toSelQ,toSelLimQ,toUpdQ,toDelQ,toExQ,toInsRetQ,toInsManyQ)
import CheckJsonReq (checkDraftReqJson)
import ConnectDB  (tryConnect,ConnDB(..))
import           Network.Wai
import           Network.HTTP.Types             ( status200, status404, status301, movedPermanently301, http11, Status, ResponseHeaders )
import           Network.HTTP.Types.URI         ( queryToQueryText )
import           Network.Wai.Handler.Warp       ( run )
import           Data.Aeson
import           Data.Text                      ( pack, unpack, Text, concat, toUpper, stripPrefix, isPrefixOf )
import           Data.ByteString.Builder        ( lazyByteString, Builder, toLazyByteString )
import           Database.PostgreSQL.Simple
import qualified Network.HTTP.Simple            as HT
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar             ( showGregorian, Day, fromGregorian, fromGregorianValid )
import           Database.PostgreSQL.Simple.Time
import           Data.String                    ( fromString )
import           Data.List                      ( intercalate, zip4, nub, delete, (\\) )
import           Control.Monad                  ( when )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Codec.Picture                  ( decodeImage )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy           as BSL
import           Control.Monad.Catch            ( catch, throwM, MonadCatch, Exception)
import           Data.HashMap.Strict            ( toList, fromList )
import qualified Data.Vector                    as V
import           Data.Int                       ( Int64 )
import qualified Database.PostgreSQL.Simple.FromField as FF
import qualified Data.Map                       as M
import           Database.PostgreSQL.Simple.FromRow
import           Control.Applicative
import qualified Control.Exception              as E
import           Crypto.Hash                    (hash,Digest)
import Crypto.Hash.Algorithms (SHA1)
import System.Random (getStdGen,newStdGen,randomRs)

data Handle m = Handle 
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
    getBody           :: Request -> m BSL.ByteString,
    printh            :: Request -> m (),
    getTokenKey       :: m String,
    withTransactionDB :: forall a. m a -> m a
    }

data Config = Config 
  { cConnDB      :: ConnDB,    
    cDefPicId    :: Integer,
    cDefUsId     :: UserId,
    cDefAuthId   :: Integer,
    cDefCatId    :: Integer,
    cCommLimit   :: Integer,
    cDraftsLimit :: Integer,
    cPostsLimit  :: Integer
    }

data ResponseInfo = ResponseInfo {resStatus :: Status, resHeaders :: ResponseHeaders, resBuilder :: Builder}

responseBuilderFromInfo :: ResponseInfo -> Response
responseBuilderFromInfo (ResponseInfo s h b) = responseBuilder s h b


application :: Config -> LogHandle IO -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application config handleLog req send = do
  let connDB = cConnDB config
  (conn,_) <- tryConnect connDB
  let h = Handle config handleLog (select' conn) (selectLimit' conn) (updateInDb' conn) (deleteFromDb' conn) (isExistInDb' conn) (insertReturn' conn) (insertByteaInDb' conn) (insertMany' conn) HT.httpLBS getDay' strictRequestBody print getTokenKey' (withTransaction conn)
  logDebug (hLog h) "Connect to DB"
  respE <- runExceptT $ logOnErr (hLog h) $ chooseRespEx h req
  let resInfo = fromE respE 
  logDebug (hLog h) $ "Output response: " ++ (show . toLazyByteString . resBuilder $ resInfo)
  send ( responseBuilderFromInfo resInfo )


fromE :: Either ReqError ResponseInfo -> ResponseInfo
fromE respE = case respE of
  Right a                  -> a
  Left (SimpleError str)   -> ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")]
                                (lazyByteString . encode $ OkInfoResponse {ok7 = False, info7 = pack str})
  Left (SecretError str)   -> ResponseInfo status404 [] "Status 404 Not Found"
  Left (DatabaseError str) -> ResponseInfo status200 [] "Internal server error"
  Left (DatabaseAndUnrollError str) -> ResponseInfo status200 [] "Internal server error"
  

okHelper :: (Monad m, MonadCatch m, MonadFail m, ToJSON a) => a -> ExceptT ReqError m ResponseInfo
okHelper toJ = return $ ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")]  (lazyByteString . encode $ toJ)


chooseRespEx :: (Monad m, MonadCatch m,MonadFail m) => Handle m  -> Request -> ExceptT ReqError m ResponseInfo
chooseRespEx h req = do
  lift $ logDebug (hLog h) $ "Incoming request: " ++ show req
  case pathInfo req of
    ["logIn"] -> do
      lift $ logInfo (hLog h) $ "Login command"
      LogIn usIdNum pwdParam <- parseQuery req
      let usIdParam = numToTxt usIdNum
      lift $ logInfo (hLog h) $ "All logIn parameters parsed"
      Auth pwd admBool <- selectOneIfExistE h "users" ["password","admin"] "user_id=?" usIdParam 
      case admBool of
        False -> do
          checkPwd pwdParam pwd
          tokenKey <- lift $ getTokenKey h
          updateInDbE h "users" "token_key=?" "user_id=?" [pack tokenKey,usIdParam]
          let usToken = pack $ show usIdNum ++ "." ++ strSha1 tokenKey ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
          okHelper $ TokenResponse {tokenTR = usToken}          
        True -> do 
          checkPwd pwdParam pwd
          tokenKey <- lift $ getTokenKey h
          updateInDbE h "users" "token_key=?" "user_id=?" [pack tokenKey,usIdParam]
          let usToken = pack $ show usIdNum ++ "." ++ strSha1 tokenKey ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
          okHelper $ TokenResponse {tokenTR = usToken}  
    ["createUser"] -> do
      lift $ logInfo (hLog h) $ "Create user command" 
      CreateUser pwdParam fNameParam lNameParam picIdNum <- parseQuery req
      let picIdParam = numToTxt picIdNum
      lift $ logInfo (hLog h) $ "All parameters parsed"
      day <- lift $ getDay h
      let hashPwdParam = pack . strSha1 . fromString . unpack $ pwdParam
      tokenKey <- lift $ getTokenKey h
      let insNames  = ["password"    ,"first_name","last_name","user_pic_id"  ,"user_create_date","admin","token_key"]
      let insValues = [ hashPwdParam ,fNameParam  ,lNameParam ,picIdParam   ,pack day          ,"FALSE",pack tokenKey]
      usId <-  insertReturnE h "users" "user_id" insNames insValues
      lift $ logDebug (hLog h) $ "DB return user_id:" ++ show usId ++ "and token key"
      lift $ logInfo (hLog h) $ "User_id: " ++ show usId ++ " created"
      let usToken = pack $ show usId ++ "." ++ strSha1 tokenKey ++ ".stu." ++ strSha1 ("stu" ++ tokenKey)
      okHelper $ UserTokenResponse {tokenUTR = usToken, user_idUTR = usId, first_nameUTR = fNameParam, last_nameUTR = lNameParam, user_pic_idUTR = picIdNum, user_pic_urlUTR = makeMyPicUrl picIdNum, user_create_dateUTR = pack day}
    ["getUser", usId] -> do
      lift $ logInfo (hLog h) $ "Get user command"
      usIdNum <- tryReadNum usId
      lift $ logInfo (hLog h) $ "All parameters parsed"
      let selectParams = ["first_name","last_name","user_pic_id","user_create_date"]
      User fName lName picId usCreateDate <- selectOneIfExistE h "users" selectParams "user_id=?" usId
      okHelper $ UserResponse {user_id = usIdNum, first_name = fName, last_name = lName, user_pic_id = picId, user_pic_url = makeMyPicUrl picId, user_create_date = pack . showGregorian $ usCreateDate}
    ["deleteUser"] -> do
      lift $ logInfo (hLog h) $ "Delete user command"
      tokenAdminAuth h  req
      DeleteUser usIdNum <- parseQuery req
      let usIdParam = pack . show $ usIdNum
      lift $ logInfo (hLog h) $ "All parameters parsed"
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
    ["createAdmin"]        -> do
      lift $ logInfo (hLog h) $ "Create admin command"
      CreateAdmin keyParam pwdParam fNameParam lNameParam picIdNum <- parseQuery req 
      let picIdParam = numToTxt picIdNum
      onlyKeys <- selectListFromDbE h "key" ["create_admin_key"] "true" ([]::[Text])  
      let keys = fmap fromOnly onlyKeys
      checkEmptyList keys
      checkKeyE keyParam (last keys)
      day   <- lift $ getDay h
      let hashPwdParam = pack . strSha1 . unpack $ pwdParam
      tokenKey <- lift $ getTokenKey h
      let insNames  = ["password","first_name","last_name","user_pic_id"    ,"user_create_date","admin","token_key"]
      let insValues = [hashPwdParam  ,fNameParam  ,lNameParam ,picIdParam,pack day          ,"TRUE",pack tokenKey ]
      admId <-  insertReturnE h "users" "user_id" insNames insValues 
      lift $ logDebug (hLog h) $ "DB return user_id" ++ show admId
      lift $ logInfo (hLog h) $ "User_id: " ++ show admId ++ " created as admin"
      let usToken = pack $ show admId ++ "." ++ strSha1 tokenKey ++ ".hij." ++ strSha1 ("hij" ++ tokenKey)
      okHelper $ UserTokenResponse {tokenUTR = usToken, user_idUTR = admId, first_nameUTR = fNameParam, last_nameUTR = lNameParam, user_pic_idUTR = picIdNum, user_pic_urlUTR = makeMyPicUrl picIdNum, user_create_dateUTR = pack day }
    ["createAuthor"]        -> do
      lift $ logInfo (hLog h) $ "Create author command"
      tokenAdminAuth h  req
      CreateAuthor usIdNum auInfoParam <- parseQuery req  
      let usIdParam = numToTxt usIdNum
      isExistInDbE h  "users" "user_id"  "user_id=?" [usIdParam] 
      ifExistInDbThrowE h "authors" "user_id" "user_id=?" [usIdParam] 
      auId <- insertReturnE h "authors" "author_id" ["user_id","author_info"] [usIdParam,auInfoParam]
      lift $ logDebug (hLog h) $ "DB return author_id" ++ show auId
      lift $ logInfo (hLog h) $ "Author_id: " ++ show auId ++ " created"
      okHelper $ AuthorResponse {author_id = auId, auth_user_id = usIdNum, author_info = auInfoParam}
    ["getAuthor"]        -> do
      lift $ logInfo (hLog h) $ "Get author command"
      tokenAdminAuth h  req
      GetAuthor auIdNum <- parseQuery req
      let auIdParam = numToTxt auIdNum
      Author auId auInfo usId <- selectOneIfExistE h "authors" ["author_id","author_info","user_id"] "author_id=?" auIdParam
      okHelper $ AuthorResponse {author_id = auIdNum, auth_user_id = usId, author_info = auInfo}
    ["updateAuthor"]        -> do
      lift $ logInfo (hLog h) $ "Update author command"
      tokenAdminAuth h  req
      UpdateAuthor auIdNum usIdNum auInfoParam <- parseQuery req
      let usIdParam = numToTxt usIdNum
      let auIdParam = numToTxt auIdNum
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      isExistInDbE h "authors" "author_id" "author_id=?" [auIdParam] 
      isntUserOtherAuthor h usIdNum auIdNum
      updateInDbE h "authors" "author_info=?,user_id=?" "author_id=?" [auInfoParam,usIdParam,auIdParam]
      okHelper $ AuthorResponse {author_id = auIdNum, auth_user_id = usIdNum, author_info = auInfoParam}
    ["deleteAuthor"]   -> do
      lift $ logInfo (hLog h) $ "Delete author command"
      tokenAdminAuth h  req
      DeleteAuthor auIdNum <- parseQuery req
      let auIdParam = numToTxt auIdNum
      isExistInDbE h "authors" "author_id" "author_id=?" [auIdParam] 
      let updatePos = updateInDb h "posts" "author_id=?" "author_id=?" [pack . show $ (cDefAuthId $ hConf h),auIdParam]
      onlyDraftsIds <- selectListFromDbE h "drafts" ["draft_id"] "author_id=?" [auIdParam]
      let draftsIds = fmap fromOnly onlyDraftsIds
      let deleteDr = deleteAllAboutDrafts h  draftsIds
      let deleteAu = deleteFromDb h "authors" "author_id=?" [auIdParam]
      withTransactionDBE h (updatePos >> deleteDr >> deleteAu)
      okHelper $ OkResponse {ok = True}
    ["createCategory"]        -> do
      lift $ logInfo (hLog h) $ "Create category command"
      tokenAdminAuth h  req
      CreateCategory catNameParam <- parseQuery req
      catId <-  insertReturnE h "categories" "category_id" ["category_name"] [catNameParam] 
      okHelper $ CatResponse {cat_id = catId, cat_name = catNameParam, one_level_sub_cats = [] , super_cat = "NULL"}
    ["createSubCategory"]        -> do
      lift $ logInfo (hLog h) $ "Create sub category command"
      tokenAdminAuth h  req 
      CreateSubCategory catNameParam superCatIdNum <- parseQuery req
      let superCatIdParam = numToTxt superCatIdNum
      isExistInDbE h "categories" "category_id" "category_id=?" [superCatIdParam] 
      catId <-  insertReturnE h "categories" "category_id" ["category_name","super_category_id"] [catNameParam,superCatIdParam] 
      catResp <- makeCatResp h  catId
      okHelper catResp
    ["getCategory", catId] -> do
      lift $ logInfo (hLog h) $ "Get category command"
      catIdNum <- tryReadNum catId
      isExistInDbE h "categories" "category_id" "category_id=?" [catId] 
      catResp <- makeCatResp h  catIdNum
      okHelper catResp
    ["updateCategory"] -> do
      lift $ logInfo (hLog h) $ "Update category command"
      tokenAdminAuth h  req    
      UpdateCategory catIdNum catNameParam superCatIdNum <- parseQuery req
      let catIdParam = numToTxt catIdNum
      let superCatIdParam = numToTxt superCatIdNum
      isExistInDbE h "categories" "category_id" "category_id=?" [catIdParam]      
      isExistInDbE h "categories" "category_id" "category_id=?" [superCatIdParam] 
      checkRelationCats h  catIdNum superCatIdNum
      updateInDbE h "categories" "category_name=?,super_category_id=?" "category_id=?" [catNameParam,superCatIdParam,catIdParam]
      catResp <- makeCatResp h  catIdNum
      okHelper catResp
    ["deleteCategory"] -> do
      lift $ logInfo (hLog h) $ "Delete category command"
      tokenAdminAuth h  req
      DeleteCategory catIdNum <- parseQuery req
      let catIdParam = numToTxt catIdNum
      isExistInDbE h "categories" "category_id" "category_id=?" [catIdParam] 
      allSubCats <- findAllSubCats h  catIdNum
      let values = fmap (pack . show) ((cDefCatId $ hConf h):allSubCats)
      let where'  = intercalate " OR " . fmap (const "post_category_id=?")  $ allSubCats
      let where'' = intercalate " OR " . fmap (const "draft_category_id=?") $ allSubCats
      let updatePos = updateInDb h "posts"  "post_category_id=?"  where'  values
      let updateDr  = updateInDb h "drafts" "draft_category_id=?" where'' values
      let where''' = intercalate " OR " . fmap (const "category_id=?") $ allSubCats
      let deleteCat = deleteFromDb h "categories" where''' (fmap (pack . show) allSubCats)
      withTransactionDBE h (updatePos >> updateDr >> deleteCat)
      okHelper $ OkResponse {ok = True}
    ["createTag"]  -> do
      lift $ logInfo (hLog h) $ "Create tag command"
      tokenAdminAuth h  req
      CreateTag tagNameParam <- parseQuery req
      tagId <-  insertReturnE h "tags" "tag_id" ["tag_name"] [tagNameParam] 
      okHelper $ TagResponse tagId tagNameParam
    ["getTag",tagId]  -> do
      lift $ logInfo (hLog h) $ "Get tag command"
      tagIdNum <- tryReadNum tagId
      Only tagName <- selectOneIfExistE h "tags" ["tag_name"] "tag_id=?" tagId
      okHelper $ TagResponse tagIdNum tagName
    ["updateTag"]        -> do
      lift $ logInfo (hLog h) $ "Update tag command"
      tokenAdminAuth h  req
      UpdateTag tagIdNum tagNameParam <- parseQuery req
      let tagIdParam = numToTxt tagIdNum
      isExistInDbE h "tags" "tag_id" "tag_id=?" [tagIdParam] 
      updateInDbE h "tags" "tag_name=?" "tag_id=?" [tagNameParam,tagIdParam]
      okHelper $ TagResponse tagIdNum tagNameParam
    ["deleteTag"]        -> do
      lift $ logInfo (hLog h) $ "Delete tag command"
      tokenAdminAuth h  req
      DeleteTag tagIdNum <- parseQuery req
      let tagIdParam = numToTxt tagIdNum
      isExistInDbE h "tags" "tag_id" "tag_id=?" [tagIdParam]
      let deleteDrTg = deleteFromDb h "draftstags" "tag_id=?" [tagIdParam] 
      let deletePosTg = deleteFromDb h "poststags" "tag_id=?" [tagIdParam] 
      let deleteTg = deleteFromDb h "tags" "tag_id=?" [tagIdParam]
      withTransactionDBE h (deleteDrTg >> deletePosTg >> deleteTg)
      okHelper $ OkResponse {ok = True}
    ["createNewDraft"]  -> do
      lift $ logInfo (hLog h) $ "Create new draft command"
      json <- lift $ getBody h req
      body <- checkDraftReqJson json
      let tokenParam   = tokenDR    body
      let nameParam    = draft_name   body
      let catIdParam   = draft_cat_id body
      let txtParam     = draft_textDR  body
      let picId        = draft_main_pic_id body
      let tagsIds      = nub . draft_tags_ids $ body
      let picsIds      = draft_pics_ids $ body
      (usIdNum,_) <- checkUserTokenParam h tokenParam
      isExistInDbE h "categories" "category_id" "category_id=?" [pack . show $ catIdParam] 
      mapM (isExistInDbE h "tags" "tag_id" "tag_id=?") $ fmap ( (:[]) . pack . show) tagsIds
      Author auId auInfo usId <- isUserAuthorE h  usIdNum 
      let where' = intercalate " OR " . fmap (const "tag_id=?") $ tagsIds
      tagS <- selectListFromDbE h "tags" ["tag_id","tag_name"] where' (fmap (pack . show) tagsIds)
      catResp <- makeCatResp h  catIdParam
      let insNames  = ["author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      let insValues = [pack . show $ auId,nameParam,pack . show $ catIdParam,txtParam,pack . show $ picId]
      draftId <- withTransactionDBE h $ do  
        draftIds <-  insertReturn h "drafts" "draft_id" insNames insValues           
        draftId <- checkSingleOutPut draftIds
        insertMany h "draftspics" ["draft_id","pic_id"] (zip (repeat draftId) picsIds)
        insertMany h "draftstags" ["draft_id","tag_id"] (zip (repeat draftId) tagsIds)
        return draftId  
      okHelper $ DraftResponse { draft_id2 = draftId, post_id2 = PostIdNull , author2 = AuthorResponse auId auInfo usId, draft_name2 = nameParam , draft_cat2 = catResp , draft_text2 = txtParam , draft_main_pic_id2 =  picId , draft_main_pic_url2 = makeMyPicUrl picId , draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}
    ["createPostsDraft"]  -> do
      lift $ logInfo (hLog h) $ "Create post`s draft command"
      (usIdNum,_) <- tokenUserAuth h req 
      CreatePostsDraft postIdNum <- parseQuery req
      let postIdParam = numToTxt postIdNum
      isUserAuthorE h  usIdNum 
      let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
      let params = ["a.author_id","author_info","post_name","post_category_id","post_text","post_main_pic_id"]
      PostInfo auId auInfo postName postCatId postTxt mPicId <- selectOneIfExistE h table params "post_id=?" postIdParam
      isPostAuthor h  postIdParam usIdNum          
      onlyPicsIds <- selectListFromDbE h "postspics" ["pic_id"] "post_id=?" [postIdParam] 
      let picsIds = fmap fromOnly onlyPicsIds
      tagS <- selectListFromDbE h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?" [postIdParam]
      let tagsIds = fmap tag_idT tagS
      catResp <- makeCatResp h  postCatId
      let insNames  = ["post_id","author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      let insValues = [postIdParam,pack . show $ auId,postName,pack . show $ postCatId,postTxt,pack . show $ mPicId]
      draftId <-  insertReturnE h "drafts" "draft_id" insNames insValues
      insertManyE h "draftspics" ["draft_id","pic_id"] (zip (repeat draftId) picsIds)
      insertManyE h "draftstags" ["draft_id","tag_id"] (zip (repeat draftId) tagsIds) 
      okHelper $ DraftResponse {draft_id2 = draftId, post_id2 = PostIdExist postIdNum, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = postName , draft_cat2 = catResp, draft_text2 = postTxt, draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl mPicId , draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}
    ["getDraft"]  -> do
      lift $ logInfo (hLog h) $ "Get draft command"
      (usIdNum,_) <- tokenUserAuth h req 
      GetDraft draftIdNum <- parseQuery req
      let draftIdParam = numToTxt draftIdNum
      let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
      let params = ["d.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      Draft drId auInfo postId draftName draftCatId draftTxt mPicId <- selectOneIfExistE h table params "draft_id=?" draftIdParam         
      Author auId _ _ <- isUserAuthorE h  usIdNum  
      isDraftAuthor h  draftIdParam usIdNum
      onlyPicsIds <- selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?" [draftIdParam]
      let picsIds = fmap fromOnly onlyPicsIds
      tagS <- selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam] 
      catResp <- makeCatResp h  draftCatId
      okHelper $ DraftResponse { draft_id2 = draftIdNum, post_id2 = isNULL postId, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName , draft_cat2 = catResp, draft_text2 = draftTxt , draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl mPicId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}
    ["getDrafts"]  -> do
      lift $ logInfo (hLog h) $ "Get drafts command"
      (usIdNum,_) <- tokenUserAuth h req  
      GetDrafts pageNum <- parseQuery req
      Author auId _ _ <- isUserAuthorE h  usIdNum  
      let table = "drafts JOIN authors ON authors.author_id = drafts.author_id"
      let orderBy = "draft_id DESC"
      let extractParams = ["drafts.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      let where' = "drafts.author_id = ?"
      let values = [pack . show $ auId]
      drafts <- selectListLimitFromDbE h table orderBy pageNum (cDraftsLimit . hConf $ h) extractParams where' values [] []
      let alldraftIdsText = fmap (pack . show . draft_idD) drafts
      let allCatIdsNum = fmap draft_cat_idD drafts
      manyCatResp <- mapM (makeCatResp h ) allCatIdsNum
      manyOnlyDraftPicsIds   <- mapM (selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?") $ fmap (:[]) alldraftIdsText  
      let manyDraftPicsIds = (fmap . fmap) fromOnly manyOnlyDraftPicsIds
      tagSMany <- mapM (selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" ) $ fmap (:[]) alldraftIdsText
      let allParams = zip4 drafts manyCatResp manyDraftPicsIds tagSMany
      okHelper $ DraftsResponse 
        { page9 = pageNum
        , drafts9 = fmap (\(( Draft draftId auInfo postId draftName draftCat draftText draftMainPicId ),catResp,pics,tagS) -> DraftResponse { draft_id2 = draftId, post_id2 = isNULL postId , author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName , draft_cat2 = catResp, draft_text2 = draftText, draft_main_pic_id2 =  draftMainPicId, draft_main_pic_url2 = makeMyPicUrl draftMainPicId , draft_tags2 = fmap inTagResp tagS, draft_pics2 =  fmap inPicIdUrl pics}) allParams }
    ["updateDraft",draftId]  -> do
      lift $ logInfo (hLog h) $ "Update draft command"
      draftIdNum <- tryReadNum draftId
      json <- lift $ getBody h req
      body <- checkDraftReqJson json
      let tokenParam   = tokenDR   body
      let nameParam    = draft_name   body
      let catIdParam   = draft_cat_id body
      let txtParam     = draft_textDR  body
      let picId        = draft_main_pic_id body
      let tagsIds      = nub . draft_tags_ids $ body
      let picsIds      = draft_pics_ids $ body
      (usIdNum,_) <- checkUserTokenParam h tokenParam
      isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftId] 
      isExistInDbE h "categories" "category_id" "category_id=?" [pack . show $ catIdParam] 
      mapM (isExistInDbE h "tags" "tag_id" "tag_id=?" ) $ fmap ( (:[]) . pack . show) tagsIds
      Author auId auInfo usId <- isUserAuthorE h  usIdNum  
      Only postId <- selectOneE h "drafts" ["COALESCE (post_id, '0') AS post_id"] "draft_id=?" [draftId] 
      let where' = intercalate " OR " . fmap (const "tag_id=?") $ tagsIds
      tagS <- selectListFromDbE h "tags" ["tag_id","tag_name"] where' (fmap (pack . show) tagsIds)
      catResp <- makeCatResp h  catIdParam  
      withTransactionDBE h $ do
        deleteDraftsPicsTags h [draftIdNum]
        updateInDb h "drafts" "draft_name=?,draft_category_id=?,draft_text=?,draft_main_pic_id=?" "draft_id=?" [nameParam,pack . show $ catIdParam,txtParam,pack . show $ picId,draftId]
        insertMany h "draftspics" ["draft_id","pic_id"] (zip (repeat draftIdNum) picsIds)
        insertMany h "draftstags" ["draft_id","tag_id"] (zip (repeat draftIdNum) tagsIds)
      okHelper $ DraftResponse {draft_id2 = draftIdNum, post_id2 = isNULL postId, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = nameParam, draft_cat2 = catResp, draft_text2 = txtParam, draft_main_pic_id2 =  picId, draft_main_pic_url2 = makeMyPicUrl picId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}
    ["deleteDraft"]  -> do
      lift $ logInfo (hLog h) $ "Delete draft command"
      (usIdNum,_) <- tokenUserAuth h req 
      DeleteDraft draftIdNum <- parseQuery req
      let draftIdParam = numToTxt draftIdNum
      isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftIdParam] 
      isUserAuthorE h  usIdNum  
      isDraftAuthor h  draftIdParam usIdNum
      withTransactionDBE h $  deleteAllAboutDrafts h  [draftIdNum]
      okHelper $ OkResponse { ok = True }
    ["publishDraft"]  -> do
      lift $ logInfo (hLog h) $ "Publish draft command"
      (usIdNum,_) <- tokenUserAuth h req 
      PublishDraft draftIdNum <- parseQuery req
      let draftIdParam = numToTxt draftIdNum       
      let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
      let params = ["d.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      Draft drId auInfo draftPostId draftName draftCatId draftTxt mPicId <- selectOneIfExistE h table params "draft_id=?" draftIdParam
      Author auId _ _ <- isUserAuthorE h  usIdNum  
      isDraftAuthor h  draftIdParam usIdNum
      case draftPostId of
        0 -> do    
          onlyPicsIds <- selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?" [draftIdParam] 
          let picsIds = fmap fromOnly onlyPicsIds
          tagS <- selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam]
          day <- lift $ getDay h 
          let insNames  = ["author_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
          let insValues = [pack . show $ auId,draftName,pack day,pack . show $ draftCatId,draftTxt,pack . show $ mPicId]          
          postId <-  insertReturnE h "posts" "post_id" insNames insValues          
          insertManyE h "postspics" ["post_id","pic_id"] (zip (repeat postId) picsIds)
          insertManyE h "poststags" ["post_id","tag_id"] (zip (repeat postId) (fmap tag_idT tagS))
          catResp <- makeCatResp h  draftCatId 
          okHelper $ PostResponse {post_id = postId, author4 = AuthorResponse auId auInfo usIdNum, post_name = draftName , post_create_date = pack day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = makeMyPicUrl mPicId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}
        _ -> do       
          onlyPicsIds <- selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?" [draftIdParam] 
          let picsIds = fmap fromOnly onlyPicsIds
          catResp <- makeCatResp h  draftCatId
          Only day <- selectOneE h "posts" ["post_create_date"] "post_id=?" [pack . show $ draftPostId]    
          tagS <- selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam] 
          withTransactionDBE h $ do
            updateInDb h "posts" "post_name=?,post_category_id=?,post_text=?,post_main_pic_id=?" "post_id=?" [draftName,pack . show $ draftCatId,draftTxt,pack . show $ mPicId,pack . show $ draftPostId]
            deletePostsPicsTags h [draftPostId]
            insertMany h "postspics" ["post_id","pic_id"] (zip (repeat draftPostId) picsIds)
            insertMany h "poststags" ["post_id","tag_id"] (zip (repeat draftPostId) (fmap tag_idT tagS))
          okHelper $ PostResponse {post_id = draftPostId, author4 = AuthorResponse auId auInfo usIdNum, post_name = draftName , post_create_date = pack . showGregorian $ day, post_cat = catResp, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = makeMyPicUrl mPicId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}
    ["getPost",postId]  -> do
      lift $ logInfo (hLog h) $ "Get post command"
      postIdNum <- tryReadNum postId
      Post pId auId auInfo usId pName pDate pCatId pText picId <- selectOneIfExistE h "posts JOIN authors ON authors.author_id = posts.author_id " ["posts.post_id","posts.author_id","author_info","user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"] "post_id=?" postId 
      onlyPicsIds <- selectListFromDbE h "postspics" ["pic_id"] "post_id=?" [postId] 
      let picsIds = fmap fromOnly onlyPicsIds
      tagS <- selectListFromDbE h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?" [postId] 
      catResp <- makeCatResp h  pCatId
      okHelper $ PostResponse {post_id = postIdNum, author4 = AuthorResponse auId auInfo usId, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}
    ["getPosts", page] -> do
      lift $ logInfo (hLog h) $ "Get posts command"
      pageNum <- tryReadNum page
      let extractParams = ["posts.post_id","posts.author_id","author_info","authors.user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
      LimitArg filterArgs sortArgs <- chooseArgs req 
      let defTable = "posts JOIN authors ON authors.author_id = posts.author_id"
      let defOrderBy = if isDateASC sortArgs then "post_create_date ASC, post_id ASC" else "post_create_date DESC, post_id DESC"
      let defWhere = "true"
      let defValues = []
      params <- selectListLimitFromDbE h defTable defOrderBy pageNum (cPostsLimit . hConf $ h) extractParams defWhere defValues filterArgs sortArgs 
      let postIdsText = fmap (pack . show . post_idP) params
      let postCatsIds = fmap post_cat_idP params 
      manyCatResp <- mapM (makeCatResp h) postCatsIds
      manyOnlyPostPicsIds <- mapM (selectListFromDbE h "postspics" ["pic_id"] "post_id=?") $ fmap (:[]) postIdsText  
      let manyPostPicsIds = (fmap . fmap) fromOnly manyOnlyPostPicsIds
      tagSMany <- mapM (selectListFromDbE h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?") $ fmap (:[]) postIdsText  
      let allParams = zip4 params manyCatResp manyPostPicsIds tagSMany
      okHelper $ PostsResponse {page10 = pageNum , posts10 = fmap (\((Post pId auId auInfo usId pName pDate pCat pText picId),catResp,pics,tagS) -> PostResponse {post_id = pId, author4 = AuthorResponse auId auInfo usId, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl pics, post_tags = fmap inTagResp tagS}) allParams}
    ["deletePost"]  -> do
      lift $ logInfo (hLog h) $ "Delete post command"
      tokenAdminAuth h  req
      DeletePost postIdNum <- parseQuery req
      let postIdParam = numToTxt postIdNum
      isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
      withTransactionDBE h $ deleteAllAboutPost h postIdNum
      okHelper $ OkResponse { ok = True }
    ["createComment"]  -> do
      lift $ logInfo (hLog h) $ "Create comment command"
      (usIdNum,_) <- tokenUserAuth h req
      CreateComment postIdNum txtParam <- parseQuery req
      let postIdParam = numToTxt postIdNum
      isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
      commId <- insertReturnE h "comments" "comment_id" ["comment_text","post_id","user_id"] [txtParam,postIdParam,(pack . show $ usIdNum)] 
      okHelper $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postIdNum, user_id6 = usIdNum}
    ["getComments"] -> do
      lift $ logInfo (hLog h) $ "Get comments command"
      GetComments postIdNum pageNum <- parseQuery req
      let postIdParam = numToTxt postIdNum
      isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
      comms <- selectListLimitFromDbE h "comments" "comment_id DESC" pageNum (cCommLimit . hConf $ h) ["comment_id","user_id","comment_text"] "post_id=?" [postIdParam] [] []
      okHelper $ CommentsResponse {page = pageNum, post_id9 = postIdNum, comments = fmap inCommResp comms}
    ["updateComment"]  -> do
      lift $ logInfo (hLog h) $ "Update comment command"
      (usIdNum,_) <- tokenUserAuth h req 
      UpdateComment commIdNum txtParam <- parseQuery req
      let commIdParam = numToTxt commIdNum 
      isCommAuthorIfExist h  commIdParam usIdNum
      updateInDbE h "comments" "comment_text=?" "comment_id=?" [txtParam,commIdParam]
      Only postId <- selectOneE h "comments" ["post_id"] "comment_id=?" [commIdParam] 
      okHelper $ CommentResponse {comment_id = commIdNum, comment_text = txtParam, post_id6 = postId, user_id6 = usIdNum}
    ["deleteComment"]  -> do
      lift $ logInfo (hLog h) $ "Delete comment command"
      (usIdNum,accessMode) <- tokenUserAuth h req 
      DeleteComment commIdNum <- parseQuery req
      let commIdParam = numToTxt commIdNum
      isExistInDbE h "comments" "comment_id" "comment_id=?" [commIdParam] 
      case accessMode of
        AdminMode -> do
          deleteFromDbE h "comments" "comment_id=?" [commIdParam]
          okHelper $ OkResponse { ok = True }
        UserMode -> do
          Only postId <- selectOneE h "comments" ["post_id"] "comment_id=?" [commIdParam]  
          isCommOrPostAuthor h commIdNum postId usIdNum 
          deleteFromDbE h "comments" "comment_id=?" [commIdParam]
          okHelper $ OkResponse {ok = True}      
    ["browsePicture"] -> do
      lift $ logInfo (hLog h) $ "browsePicture command"
      (usIdNum,_) <- tokenUserAuth h req 
      BrowsePicture picUrlParam <- parseQuery req
      lbs <- checkPicUrlGetPic h picUrlParam
      let sbs = BSL.toStrict lbs
      picId <- insertByteaInDbE h "pics" "pic_id" ["pic"] sbs
      okHelper $ inPicIdUrl picId 
    ["picture",picId]  -> do
      lift $ logInfo (hLog h) $ "Picture command"
      picIdNum <- tryReadNum picId 
      Only (Binary bs) <- selectOneIfExistE h "pics" ["pic"] "pic_id=?" picId 
      let lbs = BSL.fromStrict bs
      lift $ logInfo (hLog h) $ "Sending picture"
      return $ ResponseInfo 
        status200 
        [("Content-Type", "image/jpeg")] 
        (lazyByteString $ lbs)
    ["test",usId] -> do
      lift $ logInfo (hLog h) $ "Test command"
      usIdNum <- tryReadNum usId
      isExistInDbE h "pics" "pic_id" "pic_id=?" [usId]
      okHelper $ OkResponse {ok = True}
    ["req"] -> do
      lift $ logInfo (hLog h) $ "Test command"
      lift $ printh h $ req
      okHelper $ OkResponse {ok = True}
    _ -> throwE $ SecretError "Unknown response"  

type UserAccessMode = (UserId,AccessMode)
data AccessMode = UserMode | AdminMode

tokenAdminAuth :: (Monad m,MonadCatch m,MonadFail m) => Handle m -> Request -> ExceptT ReqError m ()
tokenAdminAuth h req = do
  Token tokenParam <- parseQuery req
  lift $ logInfo (hLog h) $ "Token parsed"
  hideErr $ checkAdminTokenParam h tokenParam

checkAdminTokenParam :: (Monad m,MonadCatch m) => Handle m -> Text -> ExceptT ReqError m ()  
checkAdminTokenParam h tokenParam =
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, _:xs) -> case break (== '.') xs of
      (tokenKeyParam, '.':'h':'i':'j':'.':ys) -> do
        usIdNum <- tryReadNum (pack usIdParam)
        maybeTokenKey <- selectMaybeOneE h "users" ["token_key"] "user_id=?" [pack usIdParam] 
        case maybeTokenKey of
          Just (Only tokenKey) ->  
            if strSha1 (unpack tokenKey) == tokenKeyParam 
                 && strSha1 ("hij" ++ (unpack tokenKey)) == ys
              then do
                lift $ logInfo (hLog h) $ "Token valid, user in AdminAccessMode. Admin_id: " ++ show usIdNum
                return $ ()
              else throwE . SimpleError $ "INVALID token"
          Nothing -> throwE . SimpleError $ "INVALID token"
      _ -> throwE . SimpleError $ "INVALID token"
    _        -> throwE . SimpleError $ "INVALID token"

tokenUserAuth :: (Monad m,MonadCatch m,MonadFail m) => Handle m -> Request -> ExceptT ReqError m UserAccessMode
tokenUserAuth h req = do
  Token tokenParam <- parseQuery req
  lift $ logInfo (hLog h) $ "Token parsed"
  checkUserTokenParam h tokenParam

checkUserTokenParam :: (Monad m,MonadCatch m) => Handle m -> Text -> ExceptT ReqError m UserAccessMode    
checkUserTokenParam h tokenParam =
  case break (== '.') . unpack $ tokenParam of
    (usIdParam, _:xs) -> case break (== '.') xs of
      (tokenKeyParam, '.':'s':'t':'u':'.':ys) -> do
        usIdNum <- tryReadNum (pack usIdParam)
        maybeTokenKey <- selectMaybeOneE h "users" ["token_key"] "user_id=?" [pack usIdParam] 
        case maybeTokenKey of
          Just (Only tokenKey) ->  
            if strSha1 (unpack tokenKey) == tokenKeyParam 
                 && strSha1 ("stu" ++ (unpack tokenKey)) == ys 
              then do
                lift $ logInfo (hLog h) $ "Token valid, user in UserAccessMode"
                return $ (usIdNum, UserMode)
              else throwE . SimpleError $ "INVALID token"
          Nothing -> throwE . SimpleError $ "INVALID token"
      (tokenKeyParam, '.':'h':'i':'j':'.':ys) -> do
        usIdNum <- tryReadNum (pack usIdParam)
        maybeTokenKey <- selectMaybeOneE h "users" ["token_key"] "user_id=?" [pack usIdParam] 
        case maybeTokenKey of
          Just (Only tokenKey) ->  
            if strSha1 (unpack tokenKey) == tokenKeyParam 
                 && strSha1 ("hij" ++ (unpack tokenKey)) == ys
              then do
                lift $ logInfo (hLog h) $ "Token valid, user in AdminAccessMode"
                return $ (usIdNum, AdminMode)
              else throwE . SimpleError $ "INVALID token"
          Nothing -> throwE . SimpleError $ "INVALID token"  
      _ -> throwE . SimpleError $ "INVALID token"
    _        -> throwE . SimpleError $ "INVALID token"


tryReadNum :: (Monad m) => Text -> ExceptT ReqError m Integer
tryReadNum "" = throwE $ SimpleError "Can`t parse parameter. Empty input."
tryReadNum xs = case reads . unpack $ xs of
  [(a,"")] -> return a
  _        -> throwE $ SimpleError $ "Can`t parse value: " ++ unpack xs ++ ". It must be number"


selectOneE :: (Monad m, MonadCatch m, Select b) => Handle m -> Table -> [Param] -> Where -> [Text] -> ExceptT ReqError m b
selectOneE h table params where' values = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table 
  xs <- catchDbErr $ lift $ select h table params where' values
  case xs of
    []           -> throwE $ DatabaseError "DatabaseError.Empty output"
    [x] -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs

selectOneIfExistE :: (Monad m, MonadCatch m, Select b) => Handle m -> Table -> [Param] -> Where -> Text -> ExceptT ReqError m b
selectOneIfExistE h table params where' value = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table 
  xs <- catchDbErr $ lift $ select h table params where' [value]
  case xs of
    []           -> throwE $ SimpleError $ (where' \\ "???") ++ unpack value ++ " doesn`t exist"
    [x] -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs

selectMaybeOneE :: (Monad m, MonadCatch m, Select b) => Handle m  -> String -> [String] -> String -> [Text] -> ExceptT ReqError m (Maybe b)
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

selectListFromDbE :: (Monad m, MonadCatch m,Select b) => Handle m  -> String -> [String] -> String -> [Text] -> ExceptT ReqError m [b]
selectListFromDbE h table params where' values = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table
  xs <- catchDbErr $ lift $ select h table params where' values
  lift $ logInfo (hLog h) $ "Data received from DB"
  return xs 

selectListLimitFromDbE :: (Monad m, MonadCatch m,Select b) => Handle m  -> String -> String -> Integer -> Integer -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> ExceptT ReqError m [b]
selectListLimitFromDbE h defTable defOrderBy page limitNumber params defWhere defValues filterArgs sortArgs =  do
  lift $ logDebug (hLog h) $ "Select data from DB."
  xs <- catchDbErr $ lift $ selectLimit h defTable defOrderBy page limitNumber params defWhere defValues filterArgs sortArgs
  lift $ logInfo (hLog h) $ "Data received from DB"
  return xs

updateInDbE :: (Monad m, MonadCatch m) => Handle m -> Table -> Set -> Where -> [Text] -> ExceptT ReqError m ()
updateInDbE h table set where' values = catchDbErr $ do
  lift $ updateInDb h table set where' values

deleteFromDbE :: (Monad m, MonadCatch m) => Handle m -> Table -> Where -> [Text] -> ExceptT ReqError m ()
deleteFromDbE h table where' values = catchDbErr $ do
  lift $ deleteFromDb h table where' values   

isExistInDbE :: (Monad m, MonadCatch m,MonadFail m) => Handle m  -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
isExistInDbE h table checkName where' values = do
  lift $ logDebug (hLog h) $ "Checking existence entity (" ++ checkName ++ ") in the DB"
  check  <- catchDbErr $ lift $ isExistInDb h table checkName where' values 
  case check of
    True -> do
      lift $ logInfo (hLog h) $ "Entity (" ++ checkName ++ ") exist"
      return ()
    False -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " doesn`t exist."
  
ifExistInDbThrowE :: (Monad m, MonadCatch m,MonadFail m) => Handle m  -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
ifExistInDbThrowE h table checkName where' values = do
  lift $ logDebug (hLog h) $ "Checking existence entity (" ++ checkName ++ ") in the DB"
  check  <- catchDbErr $ lift $ isExistInDb h table checkName where' values 
  case check of
    True -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " already exist in " ++ table
    False -> do
      lift $ logInfo (hLog h) $ "Entity (" ++ checkName ++ ") doesn`t exist"
      return ()

insertReturnE :: (Monad m, MonadCatch m, MonadFail m) => Handle m  -> String -> String -> [String] -> [Text] -> ExceptT ReqError m Integer
insertReturnE h table returnName insNames insValues =  do
  xs <- catchDbErr $ lift $ insertReturn h table returnName insNames insValues
  case xs of
    []           -> throwE $ DatabaseError "DatabaseError.Empty output"
    [x] -> do 
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs

insertByteaInDbE :: (Monad m, MonadCatch m, MonadFail m) => Handle m  -> String -> String -> [String] -> ByteString -> ExceptT ReqError m Integer
insertByteaInDbE h table returnName insNames bs =  do
  xs <- catchDbErr $ lift $ insertByteaInDb h table returnName insNames bs
  case xs of
    []           -> throwE $ DatabaseError "DatabaseError.Empty output"
    [x] -> do 
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs

insertManyE :: (Monad m, MonadCatch m, MonadFail m) => Handle m  -> String -> [String] -> [(Integer,Integer)] -> ExceptT ReqError m ()
insertManyE h table insNames insValues = catchDbErr $ do
  lift $ insertMany h table insNames insValues

withTransactionDBE :: (Monad m, MonadCatch m) => Handle m  -> m a -> ExceptT ReqError m a
withTransactionDBE h = catchDbErr . lift . withTransactionDB h

deleteAllAboutDrafts :: (Monad m, MonadCatch m) => Handle m  -> [DraftId] -> m ()
deleteAllAboutDrafts h [] = return ()
deleteAllAboutDrafts h draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteDraftsPicsTags h draftsIds
  deleteFromDb h "drafts" where' values

deleteDraftsPicsTags :: (Monad m, MonadCatch m) => Handle m  -> [DraftId] -> m ()
deleteDraftsPicsTags h [] = return ()
deleteDraftsPicsTags h draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteFromDb h "draftspics" where' values
  deleteFromDb h "draftstags" where' values

deleteAllAboutPost :: (Monad m, MonadCatch m) => Handle m  -> PostId -> m ()
deleteAllAboutPost h postId = do
  let postIdTxt = pack . show $ postId
  deletePostsPicsTags h [postId]
  deleteFromDb h "comments" "post_id=?" [postIdTxt]
  onlyDraftsIds <- select h "drafts" ["draft_id"] "post_id=?" [postIdTxt]  
  deleteAllAboutDrafts h $ fmap fromOnly onlyDraftsIds
  deleteFromDb h "posts" "post_id=?" [postIdTxt]

deletePostsPicsTags :: (Monad m, MonadCatch m) => Handle m  -> [PostId] -> m ()
deletePostsPicsTags h [] = return ()
deletePostsPicsTags h postsIds = do
  let values = fmap (pack . show) postsIds
  let where' = intercalate " OR " . fmap (const "post_id=?") $ postsIds
  deleteFromDb h "postspics" where' values
  deleteFromDb h "poststags" where' values


isCommOrPostAuthor :: (Monad m, MonadCatch m, MonadFail m) => Handle m  -> CommentId -> PostId -> UserId -> ExceptT ReqError m ()
isCommOrPostAuthor h commIdNum postId usIdNum = do
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  Only usPostId <- selectOneE h table ["user_id"] "post_id=?" [pack . show $ postId] 
  Only usComId <- selectOneE h "comments" ["user_id"] "comment_id=?" [pack . show $ commIdNum]
  case usPostId == usIdNum || usComId == usIdNum of
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ show commIdNum ++ "and not author of post_id: " ++ show postId


isCommAuthorIfExist :: (Monad m, MonadCatch m, MonadFail m) => Handle m  -> Text -> UserId -> ExceptT ReqError m ()
isCommAuthorIfExist h  commIdParam usIdNum = do
  Only usId <- selectOneIfExistE h "comments" ["user_id"] "comment_id=?" commIdParam  
  case usId == usIdNum of
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of comment_id: " ++ unpack commIdParam


isDraftAuthor :: (Monad m, MonadCatch m, MonadFail m) => Handle m  -> Text -> UserId -> ExceptT ReqError m ()
isDraftAuthor h  draftIdParam usIdNum = do
  let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
  Only usDraftId <- selectOneE h table ["user_id"] "draft_id=?" [draftIdParam]  
  case usDraftId == usIdNum of
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of draft_id: " ++ unpack draftIdParam

isPostAuthor :: (Monad m, MonadCatch m, MonadFail m) => Handle m  -> Text -> UserId -> ExceptT ReqError m ()
isPostAuthor h  postIdParam usIdNum = do
  let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
  Only usPostId <- selectOneE h table ["user_id"] "post_id=?" [postIdParam]  
  case usPostId  == usIdNum of
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " is not author of post_id: " ++ unpack postIdParam

isUserAuthorE :: (Monad m, MonadCatch m) => Handle m -> UserId -> ExceptT ReqError m Author
isUserAuthorE h  usIdNum = do
  lift $ logDebug (hLog h) $ "Checking in DB is user author"  
  maybeAu <- selectMaybeOneE h "authors" ["author_id","author_info","user_id"] "user_id=?" [pack . show $ usIdNum] 
  case maybeAu of
    Nothing -> throwE $ SimpleError $ "user_id: " ++ show usIdNum ++ " isn`t author"
    Just author -> return author
    
isntUserOtherAuthor :: (Monad m, MonadCatch m) => Handle m -> UserId -> AuthorId -> ExceptT ReqError m ()
isntUserOtherAuthor h usIdNum auIdNum = do
  let usIdParam = pack . show $ usIdNum
  let auIdParam = pack . show $ auIdNum
  maybeAuId <- selectMaybeOneE h "authors" ["author_id"] "user_id=?" [usIdParam]
  case maybeAuId of
    Just (Only auId) -> if auId == auIdNum 
      then return ()
      else throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is already author"
    Nothing -> return ()

checkRelationCats :: (Monad m, MonadCatch m) => Handle m -> CategoryId -> CategoryId -> ExceptT ReqError m ()
checkRelationCats h  catIdNum superCatIdNum 
  |catIdNum == superCatIdNum = throwE $ SimpleError $ "super_category_id: " ++ show superCatIdNum ++ " equal to category_id."
  |otherwise                 = do
    allSubCats <- findAllSubCats h  catIdNum
    if superCatIdNum `elem` allSubCats
      then throwE $ SimpleError $ "super_category_id: " ++ show superCatIdNum ++ " is subCategory of category_id: " ++ show catIdNum
      else return ()

checkPicUrlGetPic :: (Monad m,MonadCatch m) => Handle m  -> Text -> ExceptT ReqError m BSL.ByteString
checkPicUrlGetPic h url = do
  res <- (lift $ (httpAction h) . fromString . unpack $ url) `catch` ( (\e -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url ++ ". " ++ (show (e :: HT.HttpException))) )
  let lbs = HT.getResponseBody res
  let sbs = BSL.toStrict lbs
  case decodeImage sbs of
    Right _ -> return lbs
    Left _  -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url

checkPwd :: (Monad m) => Text -> Text -> ExceptT ReqError m ()
checkPwd pwdParam pwd 
  | pwd == hashPwdParam = return ()
  | otherwise       = throwE . SimpleError $ "INVALID password"
    where
      hashPwdParam = txtSha1 pwdParam

checkKeyE :: (Monad m) => Text -> Text -> ExceptT ReqError m ()
checkKeyE keyParam key 
  | keyParam == key = return ()
  | otherwise       = throwE $ SimpleError "Invalid create_admin_key"

checkEmptyList :: (Monad m) => [Text] -> ExceptT ReqError m ()
checkEmptyList [] = throwE $ SimpleError "DatabaseError.Empty output"
checkEmptyList _  = return ()

findAllSubCats :: (Monad m, MonadCatch m) => Handle m  -> CategoryId -> ExceptT ReqError m [Integer]
findAllSubCats h  catId = do
  catsIds <- findOneLevelSubCats h catId 
  case catsIds of
    [] -> return [catId]
    _  -> do       
      subCatsIds <- mapM (findAllSubCats h ) catsIds
      return $ catId : (Prelude.concat  subCatsIds)

findOneLevelSubCats :: (Monad m, MonadCatch m) => Handle m  -> CategoryId -> ExceptT ReqError m [Integer]
findOneLevelSubCats h catId = do
    catsIds <- selectListFromDbE h "categories" ["category_id"] "super_category_id=?" [pack . show $ catId]
    return (fmap fromOnly catsIds)   

makeCatResp :: (Monad m, MonadCatch m) => Handle m  -> CategoryId -> ExceptT ReqError m CatResponse
makeCatResp h catId = do
  Cat catName superCatId <- selectOneE h "categories" ["category_name","COALESCE (super_category_id, '0') AS super_category_id"] "category_id=?" [pack . show $ catId] 
  subCatsIds <- findOneLevelSubCats h catId
  case superCatId of 
    0 -> return $ CatResponse {cat_id = catId, cat_name = catName, one_level_sub_cats = subCatsIds , super_cat = "NULL"}
    _ -> do
      superCatResp <- makeCatResp h superCatId
      return $ SubCatResponse { subCat_id = catId , subCat_name = catName, one_level_sub_categories = subCatsIds , super_category = superCatResp}

checkSingleOutPut :: (Monad m, MonadCatch m) => [a] -> m a
checkSingleOutPut xs = case xs of
  [] -> throwM UnexpectedEmptyDbOutPutException
  (x:[]) -> return x
  _ -> throwM UnexpectedMultipleDbOutPutException

-- IO methods functions:

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
  execute conn (toUpdQ table set where') values
  return ()

deleteFromDb' :: Connection -> Table -> Where -> [Text] -> IO ()
deleteFromDb' conn table where' values = do
  execute conn (toDelQ table where') values
  return ()

isExistInDb' :: Connection -> String -> String -> String -> [Text] -> IO Bool
isExistInDb' conn table checkName where' values = do
  [Only check]  <- query conn (toExQ table checkName where') values
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
  executeMany conn ( toInsManyQ table insNames ) insValues
  return ()

getTokenKey' :: IO String
getTokenKey' = do
  gen <- getStdGen
  newStdGen
  return . take 6 $ randomRs ('a','z') gen

    


-- clear functions:

sha1 :: ByteString -> Digest SHA1
sha1 = hash

strSha1 :: String -> String
strSha1 = show . sha1 . fromString

txtSha1 :: Text -> Text
txtSha1 = pack . strSha1 . unpack

inCommResp :: Comment -> CommentIdTextUserResponse
inCommResp (Comment id usId txt) = CommentIdTextUserResponse id txt usId     

isNULL :: PostId -> PostIdOrNull
isNULL 0      = PostIdNull
isNULL postId = PostIdExist postId

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