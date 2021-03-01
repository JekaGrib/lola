{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}




module App where
          
import           Api
import           Logger
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
import           Data.Time.Calendar             ( showGregorian, Day, fromGregorian )
import           Database.PostgreSQL.Simple.Time
import           Data.String                    ( fromString )
import           Data.List                      ( intercalate, zip4, nub )
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



data Handle m = Handle 
  { hConf             :: Config,
    hLog              :: LogHandle m,
    selectFromDb      :: String -> [String] -> String -> [Text] -> m [SelectType],
    selectLimitFromDb :: String -> String -> Integer -> Integer -> [String] -> String -> [Text] -> m [SelectType],
    updateInDb        :: String -> String -> String -> [Text] -> m (),
    deleteFromDb      :: String -> String -> [Text] -> m (),
    isExistInDb       :: String -> String -> String -> [Text] -> m Bool,
    insertReturnInDb  :: String -> String -> [String] -> [Text] -> m [Integer],
    insertManyInDb    :: String -> [String] -> [(Integer,Integer)] -> m (),
    httpAction        :: HT.Request -> m (HT.Response BSL.ByteString),
    getDay            :: m String,
    getBody           :: Request -> m BSL.ByteString
    }

data Config = Config 
  { cDefPicId   :: Integer,
    cDefUsId    :: Integer,
    cDefAuthId  :: Integer,
    cDefCatId  :: Integer
    }

data SelectType = 
  OnlyInt {fromOnlyInt :: Integer} 
  | OnlyTxt  {fromOnlyTxt :: Text} 
  | OnlyDay  {fromOnlyDay :: Day} 
  | Auth     {pwdAu :: Text, admBoolAu :: Bool}
  | Cat      {cat_nameC :: Text, super_cat_idC :: Integer}
  | Tag      {tag_idT :: Integer, tag_nameT :: Text}
  | Author   {author_idA :: Integer, author_infoA :: Text, user_idA :: Integer}
  | Comment  {comment_idC :: Integer, user_idC :: Integer, comment_textC :: Text}
  | User     {f_nameU :: Text, l_nameU :: Text, pic_idU :: Integer, user_create_dateU :: Day}
  | PostInfo {author_infoPI :: Text, post_namePI :: Text, post_cat_idPI :: Integer, post_textPI :: Text, post_pic_idPI :: Integer}
  | Draft    {draft_idD :: Integer, author_infoD :: Text, post_idD :: Integer, draft_nameD :: Text, draft_cat_idD :: Integer, draft_textD :: Text, draft_pic_idD :: Integer}
  | Post     {post_idP :: Integer, author_idP :: Integer, author_infoP :: Text, user_idP :: Integer, post_nameP :: Text, post_create_dateU :: Day, post_cat_idP :: Integer, post_textP :: Text, post_pic_idP :: Integer}
    deriving (Eq,Show)
    
  
instance FromRow SelectType where
  fromRow = 
    (Post         <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field)
    <|> (Draft    <$> field <*> field <*> field <*> field <*> field <*> field <*> field)
    <|> (PostInfo <$> field <*> field <*> field <*> field <*> field)
    <|> (User    <$> field <*> field <*> field <*> field)
    <|> (Comment <$> field <*> field <*> field)
    <|> (Author  <$> field <*> field <*> field)
    <|> (Tag  <$> field <*> field)
    <|> (Cat  <$> field <*> field)
    <|> (Auth <$> field <*> field)
    <|> (OnlyDay <$> field)
    <|> (OnlyTxt <$> field)
    <|> (OnlyInt <$> field) 



--(cDefUsId $ hConf h) = 1
--defPicId = 1
--(cDefAuthId $ hConf h) = 1
--(cDefCatId $ hConf h) = 1

commentNumberLimit = 20
draftNumberLimit = 5
postNumberLimit = 5


getDay' :: IO String
getDay' = do
  time    <- getZonedTime
  let day = showGregorian . localDay . zonedTimeToLocalTime $ time
  return day



logOnErr h m = m `catchE` (\e -> do
  lift $ logWarning (hLog h) $ show e
  throwE e)

application :: Config -> LogHandle IO -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application config handleLog req send = do
  conn <- connectPostgreSQL (fromString $ "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'")
  let h = Handle config handleLog (selectFromDb' conn) (selectLimitFromDb' conn) (updateInDb' conn) (deleteFromDb' conn) (isExistInDb' conn) (insertReturnInDb' conn) (insertManyInDb' conn) HT.httpLBS getDay' strictRequestBody
  logDebug (hLog h) "Connect to DB"
  ansE <- runExceptT $ logOnErr h $ answerEx h req
  let resInfo = fromE ansE 
  logDebug (hLog h) $ "Output response: " ++ (show . toLazyByteString . resBuilder $ resInfo)
  send ( responseBuilderFromInfo resInfo )

data ResponseInfo = ResponseInfo {resStatus :: Status, resHeaders :: ResponseHeaders, resBuilder :: Builder}
responseBuilderFromInfo (ResponseInfo s h b) = responseBuilder s h b

fromE :: Either ReqError ResponseInfo -> ResponseInfo
fromE ansE = case ansE of
  Right a                -> a
  Left (SimpleError str) -> ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")]
                                (lazyByteString . encode $ OkInfoResponse {ok7 = False, info7 = pack str})
  Left (SecretError str) -> ResponseInfo status404 [] "Status 404 Not Found"

okHelper h x = return $ ResponseInfo status200 [("Content-Type", "application/json; charset=utf-8")]  (lazyByteString . encode $ x)

answerEx :: (Monad m, MonadCatch m,MonadFail m) => Handle m -> Request -> ExceptT ReqError m ResponseInfo
answerEx h req = do
   lift $ logDebug (hLog h) $ "Incoming request: " ++ show req
   case pathInfo req of
    ["createUser"] -> do
      lift $ logInfo (hLog h) $ "Create user command"
      let paramsNames = ["password","first_name","last_name","user_pic_url"]
      [pwdParam,fNameParam,lNameParam,picUrlParam] <- mapM (checkParam req) paramsNames
      lift $ logInfo (hLog h) $ "All parameters parsed"
      picId <- getPicId  h picUrlParam
      day <- lift $ getDay h
      let insNames  = ["password","first_name","last_name","user_pic_id"    ,"user_create_date","admin"]
      let insValues = [pwdParam  ,fNameParam  ,lNameParam ,pack (show picId),pack day          ,"FALSE"]
      usId <-  insertReturnInDbE h "users" "user_id" insNames insValues
      lift $ logDebug (hLog h) $ "DB return user_id" ++ show usId
      lift $ logInfo (hLog h) $ "User_id: " ++ show usId ++ " created"
      okHelper h $ UserResponse {user_id = usId, first_name = fNameParam, last_name = lNameParam, user_pic_id = picId, user_pic_url = makeMyPicUrl picId, user_create_date = pack day}
    ["getUser", usId] -> do
      lift $ logInfo (hLog h) $ "Get user command"
      usIdNum <- tryRead usId
      lift $ logInfo (hLog h) $ "All parameters parsed"
      let selectParams = ["first_name","last_name","user_pic_id","user_create_date"]
      isExistInDbE h "users" "user_id" "user_id=?" [usId] 
      User fName lName picId usCreateDate <- selectOneFromDbE h "users" selectParams "user_id=?" [usId]
      okHelper h $ UserResponse {user_id = usIdNum, first_name = fName, last_name = lName, user_pic_id = picId, user_pic_url = makeMyPicUrl picId, user_create_date = pack . showGregorian $ usCreateDate}
    ["deleteUser"] -> do
      lift $ logInfo (hLog h) $ "Delete user command"
      adminAuthE h  req
      let paramsNames = ["user_id"]
      [usIdParam] <- mapM (checkParam req) paramsNames
      [usIdNum]   <- mapM tryRead [usIdParam]
      lift $ logInfo (hLog h) $ "All parameters parsed"
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      updateInDbE h "comments" "user_id=?" "user_id=?" [pack . show $ (cDefUsId $ hConf h),usIdParam]
      check <- isUserAuthorBool h usIdNum 
      case check of
        True -> do
          OnlyInt authorId <- selectOneFromDbE h "authors" ["author_id"] "user_id=?" [usIdParam]  
          updateInDbE h "posts" "author_id=?" "author_id=?" [pack . show $ (cDefAuthId $ hConf h),pack . show $ authorId]
          draftsIdsSel <- selectListFromDbE h "drafts" ["draft_id"] "author_id=?" [pack . show $ authorId]  
          let draftsIds = fmap fromOnlyInt draftsIdsSel
          deleteAllAboutDrafts h draftsIds
          deleteFromDbE h "authors" "author_id=?" [pack . show $ authorId]
          return () 
        False -> return () 
      deleteFromDbE h "users" "user_id=?" [usIdParam]
      lift $ logInfo (hLog h) $ "User_id: " ++ show usIdNum ++ " deleted"
      okHelper h $ OkResponse {ok = True}
    ["createAdmin"]        -> do
      lift $ logInfo (hLog h) $ "Create admin command"
      let paramsNames = ["create_admin_key","password","first_name","last_name","user_pic_url"]
      [keyParam,pwdParam,fNameParam,lNameParam,picUrlParam] <- mapM (checkParam req) paramsNames
      keysSel <- selectListFromDbE h "key" ["create_admin_key"] "true" ([]::[Text])  
      let keys = fmap fromOnlyTxt keysSel
      checkEmptyList keys
      checkKeyE keyParam (last keys)
      picId <- getPicId  h picUrlParam 
      day <- lift $ getDay h
      let insNames  = ["password","first_name","last_name","user_pic_id"    ,"user_create_date","admin"]
      let insValues = [pwdParam  ,fNameParam  ,lNameParam ,pack (show picId),pack day          ,"TRUE" ]
      admId <-  insertReturnInDbE h "users" "user_id" insNames insValues 
      lift $ logDebug (hLog h) $ "DB return user_id" ++ show admId
      lift $ logInfo (hLog h) $ "User_id: " ++ show admId ++ " created as admin"
      okHelper h $ UserResponse {user_id = admId, first_name = fNameParam, last_name = lNameParam, user_pic_id = picId, user_pic_url = makeMyPicUrl picId, user_create_date = pack day }
    ["createAuthor"]        -> do
      lift $ logInfo (hLog h) $ "Create author command"
      adminAuthE h  req
      let paramsNames = ["user_id","author_info"]
      [usIdParam,auInfoParam] <- mapM (checkParam req) paramsNames  
      [usIdNum]               <- mapM tryRead [usIdParam]  
      isExistInDbE h  "users" "user_id"  "user_id=?" [usIdParam] 
      ifExistInDbThrowE h "authors" "user_id" "user_id=?" [usIdParam] 
      auId <-  insertReturnInDbE h "authors" "user_id" ["user_id","author_info"] [usIdParam,auInfoParam]
      lift $ logDebug (hLog h) $ "DB return author_id" ++ show auId
      lift $ logInfo (hLog h) $ "Author_id: " ++ show auId ++ " created"
      okHelper h $ AuthorResponse {author_id = auId, auth_user_id = usIdNum, author_info = auInfoParam}
    ["getAuthor"]        -> do
      lift $ logInfo (hLog h) $ "Get author command"
      adminAuthE h  req
      let paramsNames = ["author_id"]
      [auIdParam] <- mapM (checkParam req) paramsNames
      [auIdNum]   <- mapM tryRead [auIdParam]
      isExistInDbE h "authors" "author_id" "author_id=?" [auIdParam] 
      Author auId auInfo usId <- selectOneFromDbE h "authors" ["author_id","author_info","user_id"] "author_id=?" [auIdParam] 
      okHelper h $ AuthorResponse {author_id = auIdNum, auth_user_id = usId, author_info = auInfo}
    ["updateAuthor"]        -> do
      lift $ logInfo (hLog h) $ "Update author command"
      adminAuthE h  req
      let paramsNames = ["author_id","user_id","author_info"]
      [auIdParam,usIdParam,auInfoParam] <- mapM (checkParam req) paramsNames
      [auIdNum,usIdNum]                 <- mapM tryRead [auIdParam,usIdParam]
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      isExistInDbE h "authors" "author_id" "author_id=?" [auIdParam] 
      checkRelationUsAu h usIdParam auIdParam
      updateInDbE h "authors" "author_info=?,user_id=?" "author_id=?" [auInfoParam,usIdParam,auIdParam]
      okHelper h $ AuthorResponse {author_id = auIdNum, auth_user_id = usIdNum, author_info = auInfoParam}
    ["deleteAuthor"]   -> do
      lift $ logInfo (hLog h) $ "Delete author command"
      adminAuthE h  req
      let paramsNames = ["author_id"]
      [auIdParam] <- mapM (checkParam req) paramsNames
      [auIdNum]   <- mapM tryRead [auIdParam]
      isExistInDbE h "authors" "author_id" "author_id=?" [auIdParam] 
      updateInDbE h "posts" "author_id=?" "author_id=?" [pack . show $ (cDefAuthId $ hConf h),auIdParam]
      draftsIdsSel <- selectListFromDbE h "drafts" ["draft_id"] "author_id=?" [auIdParam]
      let draftsIds = fmap fromOnlyInt draftsIdsSel 
      deleteAllAboutDrafts h  draftsIds
      deleteFromDbE h "authors" "author_id=?" [auIdParam]
      okHelper h $ OkResponse {ok = True}
    ["createCategory"]        -> do
      lift $ logInfo (hLog h) $ "Create category command"
      adminAuthE h  req
      let paramsNames = ["category_name"]
      [catNameParam] <- mapM (checkParam req) paramsNames
      catId <-  insertReturnInDbE h "categories" "category_id" ["category_name"] [catNameParam] 
      okHelper h $ CatResponse {cat_id = catId, cat_name = catNameParam, one_level_sub_cats = [] , super_cat = "NULL"}
    ["createSubCategory"]        -> do
      lift $ logInfo (hLog h) $ "Create sub category command"
      adminAuthE h  req
      let paramsNames = ["category_name","super_category_id"]
      [catNameParam,superCatIdParam] <- mapM (checkParam req) paramsNames
      [superCatIdNum]                <- mapM tryRead [superCatIdParam] 
      isExistInDbE h "categories" "category_id" "category_id=?" [superCatIdParam] 
      catId <-  insertReturnInDbE h "categories" "category_id" ["category_name","super_category_id"] [catNameParam,superCatIdParam] 
      allSuperCats <- findAllSuperCats h  catId
      okHelper h $ inCatResp allSuperCats
    ["getCategory", catId] -> do
      lift $ logInfo (hLog h) $ "Get category command"
      catIdNum <- tryRead catId
      isExistInDbE h "categories" "category_id" "category_id=?" [catId] 
      allSuperCats <- findAllSuperCats h  catIdNum
      okHelper h $ inCatResp allSuperCats
    ["updateCategory"] -> do
      lift $ logInfo (hLog h) $ "Update category command"
      adminAuthE h  req
      let paramsNames = ["category_id","category_name","super_category_id"]
      [catIdParam,catNameParam,superCatIdParam] <- mapM (checkParam req) paramsNames
      [catIdNum,superCatIdNum]                  <- mapM tryRead [catIdParam,superCatIdParam]     
      isExistInDbE h "categories" "category_id" "category_id=?" [catIdParam]      
      isExistInDbE h "categories" "category_id" "category_id=?" [superCatIdParam] 
      checkRelationCats h  catIdNum superCatIdNum
      updateInDbE h "categories" "category_name=?,super_category_id=?" "category_id=?" [catNameParam,superCatIdParam,catIdParam]
      allSuperCats <- findAllSuperCats h  catIdNum
      okHelper h $ inCatResp allSuperCats 
    ["deleteCategory"] -> do
      lift $ logInfo (hLog h) $ "Delete category command"
      adminAuthE h  req
      let paramsNames = ["category_id"]
      [catIdParam] <- mapM (checkParam req) paramsNames
      [catIdNum]              <- mapM tryRead [catIdParam] 
      isExistInDbE h "categories" "category_id" "category_id=?" [catIdParam] 
      allSubCats <- findAllSubCats h  catIdNum
      let values = fmap (pack . show) ((cDefCatId $ hConf h):allSubCats)
      let where'  = intercalate " OR " . fmap (const "post_category_id=?")  $ allSubCats
      let where'' = intercalate " OR " . fmap (const "draft_category_id=?") $ allSubCats
      updateInDbE h "posts"  "post_category_id=?"  where'  values
      updateInDbE h "drafts" "draft_category_id=?" where'' values
      let where''' = intercalate " OR " . fmap (const "category_id=?") $ allSubCats
      deleteFromDbE h "categories" where''' (fmap (pack . show) allSubCats)
      okHelper h $ OkResponse {ok = True}
    ["createTag"]  -> do
      lift $ logInfo (hLog h) $ "Create tag command"
      adminAuthE h  req
      let paramsNames = ["tag_name"]
      [tagNameParam] <- mapM (checkParam req) paramsNames
      tagId <-  insertReturnInDbE h "tags" "tag_id" ["tag_name"] [tagNameParam] 
      okHelper h $ TagResponse tagId tagNameParam
    ["getTag",tagId]  -> do
      lift $ logInfo (hLog h) $ "Get tag command"
      tagIdNum <- tryRead tagId
      isExistInDbE h "tags" "tag_id" "tag_id=?" [tagId] 
      OnlyTxt tagName <- selectOneFromDbE h "tags" ["tag_name"] "tag_id=?" [tagId] 
      okHelper h $ TagResponse tagIdNum tagName
    ["updateTag"]        -> do
      lift $ logInfo (hLog h) $ "Update tag command"
      adminAuthE h  req
      let paramsNames = ["tag_id","tag_name"]
      [tagIdParam,tagNameParam] <- mapM (checkParam req) paramsNames
      [tagIdNum]                <- mapM tryRead [tagIdParam]
      isExistInDbE h "tags" "tag_id" "tag_id=?" [tagIdParam] 
      updateInDbE h "tags" "tag_name=?" "tag_id=?" [tagNameParam,tagIdParam]
      okHelper h $ TagResponse tagIdNum tagNameParam
    ["deleteTag"]        -> do
      lift $ logInfo (hLog h) $ "Delete tag command"
      adminAuthE h  req
      let paramsNames = ["tag_id"]
      [tagIdParam] <- mapM (checkParam req) paramsNames
      [tagIdNum]              <- mapM tryRead [tagIdParam] 
      deleteFromDbE h "draftstags" "tag_id=?" [tagIdParam]
      deleteFromDbE h "poststags" "tag_id=?" [tagIdParam]
      deleteFromDbE h "tags" "tag_id=?" [tagIdParam]
      okHelper h $ OkResponse {ok = True}
    ["createNewDraft"]  -> do
      lift $ logInfo (hLog h) $ "Create new draft command"
      json <- lift $ getBody h req
      body <- checkDraftReqJson json
      let usIdParam    = user_id1     body
      let pwdParam     = password1    body
      let nameParam    = draft_name   body
      let catIdParam   = draft_cat_id body
      let txtParam     = draft_text1  body
      let mPicUrlParam = draft_main_pic_url body
      let tagsIds      = nub . draft_tags_ids $ body
      let picsUrls     = draft_pics_urls $ body
      isExistInDbE h "users" "user_id" "user_id=?" [pack . show $ usIdParam] 
      OnlyTxt pwd <- selectOneFromDbE h "users" ["password"] "user_id=?" [pack . show $ usIdParam] 
      userAuth pwdParam pwd
      isExistInDbE h "categories" "category_id" "category_id=?" [pack . show $ catIdParam] 
      mapM (isExistInDbE h "tags" "tag_id" "tag_id=?") $ fmap ( (:[]) . pack . show) tagsIds
      isUserAuthorE h  usIdParam 
      Author auId auInfo usId <- selectOneFromDbE h "authors" ["author_id","author_info","user_id"] "user_id=?" [pack . show $ usIdParam] 
      picId <- getPicId  h mPicUrlParam
      picsIds <- mapM (getPicId  h) picsUrls
      let insNames  = ["author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      let insValues = [pack . show $ auId,nameParam,pack . show $ catIdParam,txtParam,pack . show $ picId]
      draftId <-  insertReturnInDbE h "drafts" "draft_id" insNames insValues          
      insertManyInDbE h "draftspics" ["draft_id","pic_id"] (zip (repeat draftId) picsIds)
      insertManyInDbE h "draftstags" ["draft_id","tag_id"] (zip (repeat draftId) tagsIds)
      let where' = intercalate " OR " . fmap (const "tag_id=?") $ tagsIds
      tagS <- selectListFromDbE h "tags" ["tag_id","tag_name"] where' (fmap (pack . show) tagsIds)
      allSuperCats <- findAllSuperCats h  catIdParam  
      okHelper h $ DraftResponse { draft_id2 = draftId, post_id2 = PostText "NULL" , author2 = AuthorResponse auId auInfo usId, draft_name2 = nameParam , draft_cat2 =  inCatResp allSuperCats , draft_text2 = txtParam , draft_main_pic_id2 =  picId , draft_main_pic_url2 = makeMyPicUrl picId , draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}
    ["createPostsDraft"]  -> do
      lift $ logInfo (hLog h) $ "Create post`s draft command"
      let paramsNames = ["post_id","user_id","password"]
      [postIdParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [postIdNum,usIdNum]              <- mapM tryRead [postIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      OnlyTxt pwd <- selectOneFromDbE h "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isUserAuthorE h  usIdNum 
      isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
      auId <- isPostAuthor h  postIdParam usIdParam
      let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
      let params = ["author_info","post_name","post_category_id","post_text","post_main_pic_id"]
      PostInfo auInfo postName postCatId postTxt mPicId <- selectOneFromDbE h table params "post_id=?" [postIdParam]         
      picsIdsSel <- selectListFromDbE h "postspics" ["pic_id"] "post_id=?" [postIdParam] 
      let picsIds = fmap fromOnlyInt picsIdsSel
      tagS <- selectListFromDbE h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?" [postIdParam]
      allSuperCats <- findAllSuperCats h  postCatId
      let insNames  = ["post_id","author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      let insValues = [postIdParam,pack . show $ auId,postName,pack . show $ postCatId,postTxt,pack . show $ mPicId]
      draftId <-  insertReturnInDbE h "drafts" "draft_id" insNames insValues 
      okHelper h $ DraftResponse {draft_id2 = draftId, post_id2 = PostInteger postIdNum, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = postName , draft_cat2 =  inCatResp allSuperCats, draft_text2 = postTxt, draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl mPicId , draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}
    ["getDraft"]  -> do
      lift $ logInfo (hLog h) $ "Get draft command"
      let paramsNames = ["draft_id","user_id","password"]
      [draftIdParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [draftIdNum,usIdNum]              <- mapM tryRead [draftIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      OnlyTxt pwd <- selectOneFromDbE h "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isUserAuthorE h  usIdNum  
      auId <- isDraftAuthor h  draftIdParam usIdParam
      let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
      let params = ["d.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      Draft drId auInfo postId draftName draftCatId draftTxt mPicId <- selectOneFromDbE h table params "draft_id=?" [draftIdParam]        
      picsIdsSel <- selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?" [draftIdParam]
      let picsIds = fmap fromOnlyInt picsIdsSel
      tagS <- selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam] 
      allSuperCats <- findAllSuperCats h  draftCatId
      okHelper h $ DraftResponse { draft_id2 = draftIdNum, post_id2 = isNULL postId, author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName , draft_cat2 =  inCatResp allSuperCats, draft_text2 = draftTxt , draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl mPicId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}
    ["getDrafts"]  -> do
      lift $ logInfo (hLog h) $ "Get drafts command"
      let paramsNames = ["page","user_id","password"]
      [pageParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [pageNum,usIdNum]              <- mapM tryRead [pageParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      OnlyTxt pwd <- selectOneFromDbE h "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isUserAuthorE h  usIdNum  
      OnlyInt auId <- selectOneFromDbE h "authors" ["author_id"] "user_id=?" [usIdParam]  
      let table = "drafts JOIN authors ON authors.author_id = drafts.author_id"
      let orderBy = "draft_id DESC"
      let extractParams = ["drafts.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      let where' = "drafts.author_id = ?"
      let values = [pack . show $ auId]
      params <- selectListLimitFromDbE h table orderBy pageNum draftNumberLimit extractParams where' values 
      let alldraftIdsText = fmap (pack . show . draft_idD) params
      let allCatIdsNum = fmap draft_cat_idD params
      manyAllSuperCats <- mapM (findAllSuperCats h ) allCatIdsNum
      manyDraftPicsIdsSel <- mapM (selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?") $ fmap (:[]) alldraftIdsText  
      let manyDraftPicsIds = (fmap . fmap) fromOnlyInt manyDraftPicsIdsSel
      tagSMany <- mapM (selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" ) $ fmap (:[]) alldraftIdsText
      let allParams = zip4 params manyAllSuperCats manyDraftPicsIds tagSMany
      okHelper h $ DraftsResponse 
        { page9 = pageNum
        , drafts9 = fmap (\(( Draft draftId auInfo postId draftName draftCat draftText draftMainPicId ),cats,pics,tagS) -> DraftResponse { draft_id2 = draftId, post_id2 = isNULL postId , author2 = AuthorResponse auId auInfo usIdNum, draft_name2 = draftName , draft_cat2 =  inCatResp cats, draft_text2 = draftText, draft_main_pic_id2 =  draftMainPicId, draft_main_pic_url2 = makeMyPicUrl draftMainPicId , draft_tags2 = fmap inTagResp tagS, draft_pics2 =  fmap inPicIdUrl pics}) allParams }
    ["updateDraft",draftId]  -> do
      lift $ logInfo (hLog h) $ "Update draft command"
      draftIdNum <- tryRead draftId
      json <- lift $ getBody h req
      body <- checkDraftReqJson json
      let usIdParam    = user_id1     body
      let pwdParam     = password1    body
      let nameParam    = draft_name   body
      let catIdParam   = draft_cat_id body
      let txtParam     = draft_text1  body
      let mPicUrlParam = draft_main_pic_url body
      let tagsIds      = nub . draft_tags_ids $ body
      let picsUrls     = draft_pics_urls $ body
      isExistInDbE h "users" "user_id" "user_id=?" [pack . show $ usIdParam] 
      OnlyTxt pwd <- selectOneFromDbE h "users" ["password"] "user_id=?" [pack . show $ usIdParam] 
      userAuth pwdParam pwd
      isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftId] 
      isExistInDbE h "categories" "category_id" "category_id=?" [pack . show $ catIdParam] 
      mapM (isExistInDbE h "tags" "tag_id" "tag_id=?" ) $ fmap ( (:[]) . pack . show) tagsIds
      isUserAuthorE h  usIdParam  
      Author auId auInfo usId <- selectOneFromDbE h "authors" ["author_id","author_info","user_id"] "user_id=?" [pack . show $ usIdParam] 
      OnlyInt postId <- selectOneFromDbE h "drafts" ["COALESCE (post_id, '0') AS post_id"] "draft_id=?" [draftId] 
      picId <- getPicId  h mPicUrlParam
      picsIds <- mapM (getPicId  h) picsUrls
      deleteDraftsPicsTags h [draftIdNum]
      updateInDbE h "drafts" "draft_name=?,draft_category_id=?,draft_text=?,draft_main_pic_id=?" "draft_id=?" [nameParam,pack . show $ catIdParam,txtParam,pack . show $ picId,draftId]
      insertManyInDbE h "draftspics" ["draft_id","pic_id"] (zip (repeat draftIdNum) picsIds)
      insertManyInDbE h "draftstags" ["draft_id","tag_id"] (zip (repeat draftIdNum) tagsIds)
      let where' = intercalate " OR " . fmap (const "tag_id=?") $ tagsIds
      tagS <- selectListFromDbE h "tags" ["tag_id","tag_name"] where' (fmap (pack . show) tagsIds)
      allSuperCats <- findAllSuperCats h  catIdParam  
      okHelper h $ DraftResponse {draft_id2 = draftIdNum, post_id2 = isNULL postId, author2 = AuthorResponse auId auInfo usIdParam, draft_name2 = nameParam, draft_cat2 =  inCatResp allSuperCats, draft_text2 = txtParam, draft_main_pic_id2 =  picId, draft_main_pic_url2 = makeMyPicUrl picId, draft_tags2 = fmap inTagResp tagS, draft_pics2 = fmap inPicIdUrl picsIds}
    ["deleteDraft"]  -> do
      lift $ logInfo (hLog h) $ "Delete draft command"
      let paramsNames = ["draft_id","user_id","password"]
      [draftIdParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [draftIdNum,usIdNum]              <- mapM tryRead [draftIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      OnlyTxt pwd <- selectOneFromDbE h "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftIdParam] 
      isUserAuthorE h  usIdNum  
      isDraftAuthor h  draftIdParam usIdParam
      deleteAllAboutDrafts h  [draftIdNum]
      okHelper h $ OkResponse { ok = True }
    ["publishDraft"]  -> do
      lift $ logInfo (hLog h) $ "Publish draft command"
      let paramsNames = ["draft_id","user_id","password"]
      [draftIdParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [draftIdNum,usIdNum]              <- mapM tryRead [draftIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      OnlyTxt pwd <- selectOneFromDbE h "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftIdParam] 
      isUserAuthorE h  usIdNum  
      auId <- isDraftAuthor h  draftIdParam usIdParam
      let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
      let params = ["d.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      Draft drId auInfo draftPostId draftName draftCatId draftTxt mPicId <- selectOneFromDbE h table params "draft_id=?" [draftIdParam]
      case draftPostId of
        0 -> do    
          picsIdsSel <- selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?" [draftIdParam] 
          let picsIds = fmap fromOnlyInt picsIdsSel
          tagS <- selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam]
          day <- lift $ getDay h 
          let insNames  = ["author_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
          let insValues = [pack . show $ auId,draftName,pack day,pack . show $ draftCatId,draftTxt,pack . show $ mPicId]          
          postId <-  insertReturnInDbE h "posts" "post_id" insNames insValues          
          insertManyInDbE h "postspics" ["post_id","pic_id"] (zip (repeat postId) picsIds)
          insertManyInDbE h "poststags" ["post_id","tag_id"] (zip (repeat postId) (fmap tag_idT tagS))
          allSuperCats <- findAllSuperCats h  draftCatId 
          okHelper h $ PostResponse {post_id = postId, author4 = AuthorResponse auId auInfo usIdNum, post_name = draftName , post_create_date = pack day, post_cat = inCatResp allSuperCats, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = makeMyPicUrl mPicId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}
        _ -> do       
          picsIdsSel <- selectListFromDbE h "draftspics" ["pic_id"] "draft_id=?" [draftIdParam] 
          let picsIds = fmap fromOnlyInt picsIdsSel
          tagS <- selectListFromDbE h "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam] 
          updateInDbE h "posts" "post_name=?,post_category_id=?,post_text=?,post_main_pic_id=?" "post_id=?" [draftName,pack . show $ draftCatId,draftTxt,pack . show $ mPicId,pack . show $ draftPostId]
          deletePostsPicsTags h  [draftPostId]
          insertManyInDbE h "postspics" ["post_id","pic_id"] (zip (repeat draftPostId) picsIds)
          insertManyInDbE h "poststags" ["post_id","tag_id"] (zip (repeat draftPostId) (fmap tag_idT tagS))
          allSuperCats <- findAllSuperCats h  draftCatId
          OnlyDay day <- selectOneFromDbE h "posts" ["post_create_date"] "post_id=?" [pack . show $ draftPostId]    
          okHelper h $ PostResponse {post_id = draftPostId, author4 = AuthorResponse auId auInfo usIdNum, post_name = draftName , post_create_date = pack . showGregorian $ day, post_cat = inCatResp allSuperCats, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = makeMyPicUrl mPicId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}
    ["getPost",postId]  -> do
      lift $ logInfo (hLog h) $ "Get post command"
      postIdNum <- tryRead postId
      isExistInDbE h "users" "user_id" "user_id=?" [postId] 
      Post pId auId auInfo usId pName pDate pCatId pText picId <- selectOneFromDbE h "posts JOIN authors ON authors.author_id = posts.author_id " ["posts.post_id","posts.author_id","author_info","user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"] "post_id=?" [postId] 
      picsIdsSel <- selectListFromDbE h "postspics" ["pic_id"] "post_id=?" [postId] 
      let picsIds = fmap fromOnlyInt picsIdsSel
      tagS <- selectListFromDbE h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?" [postId] 
      allSuperCats <- findAllSuperCats h  pCatId
      okHelper h $ PostResponse {post_id = postIdNum, author4 = AuthorResponse auId auInfo usId, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = inCatResp allSuperCats, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}
    ["getPosts", page] -> do
      lift $ logInfo (hLog h) $ "Get posts command"
      pageNum <- tryRead page
      let extractParamsList = ["posts.post_id","posts.author_id","author_info","authors.user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
      (table,where',orderBy,values) <- chooseArgs req       
      params <- selectListLimitFromDbE h table orderBy pageNum postNumberLimit extractParamsList where' values 
      let postIdsText = fmap (pack . show . post_idP) params
      let postCatsIds = fmap post_cat_idP params 
      manySuperCats <- mapM (findAllSuperCats h ) postCatsIds
      manyPostPicsIdsSel <- mapM (selectListFromDbE h "postspics" ["pic_id"] "post_id=?") $ fmap (:[]) postIdsText  
      let manyPostPicsIds = (fmap . fmap) fromOnlyInt manyPostPicsIdsSel
      tagSMany <- mapM (selectListFromDbE h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?") $ fmap (:[]) postIdsText  
      let allParams = zip4 params manySuperCats manyPostPicsIds tagSMany
      okHelper h $ PostsResponse {page10 = pageNum , posts10 = fmap (\((Post pId auId auInfo usId pName pDate pCat pText picId),cats,pics,tagS) -> PostResponse {post_id = pId, author4 = AuthorResponse auId auInfo usId, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = inCatResp cats, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl pics, post_tags = fmap inTagResp tagS}) allParams}
    ["deletePost"]  -> do
      lift $ logInfo (hLog h) $ "Delete post command"
      adminAuthE h  req
      let paramsNames = ["post_id"]
      [postIdParam] <- mapM (checkParam req) paramsNames
      [postIdNum]   <- mapM tryRead [postIdParam] 
      deleteAllAboutPosts h  [postIdNum]
      okHelper h $ OkResponse { ok = True }
    ["createComment"]  -> do
      lift $ logInfo (hLog h) $ "Create comment command"
      let paramsNames = ["post_id","comment_text","user_id","password"]
      [postIdParam,txtParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [postIdNum,usIdNum]                       <- mapM tryRead [postIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      OnlyTxt pwd <- selectOneFromDbE h "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
      commId <-  insertReturnInDbE h "comments" "comment_id" ["comment_text","post_id","user_id"] [txtParam,postIdParam,usIdParam] 
      okHelper h $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postIdNum, user_id6 = usIdNum}
    ["getComments"] -> do
      lift $ logInfo (hLog h) $ "Get comments command"
      let paramsNames = ["post_id","page"]
      [postIdParam,pageParam] <- mapM (checkParam req) paramsNames
      [postIdNum,pageNum]     <- mapM tryRead [postIdParam,pageParam] 
      isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
      comms <- selectListLimitFromDbE h "comments" "comment_id DESC" pageNum commentNumberLimit ["comment_id","user_id","comment_text"] "post_id=?" [postIdParam]
      okHelper h $ CommentsResponse {page = pageNum, post_id9 = postIdNum, comments = fmap inCommResp comms}
    ["updateComment"]  -> do
      lift $ logInfo (hLog h) $ "Update comment command"
      let paramsNames = ["comment_id","comment_text","user_id","password"]
      [commIdParam,txtParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [commIdNum,usIdNum]                       <- mapM tryRead [commIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      OnlyTxt pwd <- selectOneFromDbE h "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isCommAuthor h  commIdParam usIdParam
      updateInDbE h "comments" "comment_text=?" "comment_id=?" [txtParam,commIdParam]
      OnlyInt postId <- selectOneFromDbE h "comments" ["post_id"] "comment_id=?" [commIdParam] 
      okHelper h $ CommentResponse {comment_id = commIdNum, comment_text = txtParam, post_id6 = postId, user_id6 = usIdNum}
    ["deleteComment"]  -> do
      lift $ logInfo (hLog h) $ "Delete comment command" 
      case accessMode req of
        AdminMode -> do
          adminAuthE h  req
          let paramsNames = ["comment_id"]
          [commIdParam] <- mapM (checkParam req) paramsNames
          [commIdNum]   <- mapM tryRead [commIdParam]
          deleteFromDbE h "comments" "comment_id=?" [commIdParam]
          okHelper h $ OkResponse { ok = True }
        UserMode -> do
          let paramsNames = ["comment_id","user_id","password"]
          [commIdParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
          [commIdNum,usIdNum]              <- mapM tryRead [commIdParam,usIdParam]
          isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
          OnlyTxt pwd <- selectOneFromDbE h "users" ["password"] "user_id=?" [usIdParam] 
          userAuth pwdParam pwd
          OnlyInt postId <- selectOneFromDbE h "comments" ["post_id"] "comment_id=?" [commIdParam]  
          isCommOrPostAuthor h commIdParam (pack . show $ (postId :: Integer)) usIdParam
          deleteFromDbE h "comments" "comment_id=?" [commIdParam]
          okHelper h $ OkResponse {ok = True}      
    ["picture",picId]  -> do
      lift $ logInfo (hLog h) $ "Picture command"
      picIdNum <- tryRead picId 
      isExistInDbE h "pics" "pic_id" "pic_id=?" [picId] 
      OnlyTxt picUrl <- selectOneFromDbE h "pics" ["pic_url"] "pic_id=?" [picId] 
      res <- getPicFromUrlE h picUrl
      lift $ logInfo (hLog h) $ "Sending picture"
      return $ ResponseInfo 
        status200 
        [("Content-Type", "image/jpeg")] 
        (lazyByteString $ HT.getResponseBody res)
    ["test",usId] -> do
      lift $ logInfo (hLog h) $ "Test command"
      usIdNum <- tryRead usId
      isExistInDbE h "pics" "pic_id" "pic_id=?" [usId]
      okHelper h $ OkResponse {ok = True} 
      


getPicFromUrlE h picUrl = do
  lift $ logInfo (hLog h) $ "Getting picture from internet. Url:" ++ unpack picUrl
  case isMyUrl picUrl of
    True -> throwE $ SimpleError $ "Invalid url"
    _    -> do
      res <- checkPicUrlGetPic h picUrl
      return res


isCommOrPostAuthor h commIdParam postIdParam usIdParam = do
  isCommAuthor h commIdParam usIdParam
    `catchE`
      (\(SimpleError str) -> 
        withExceptT 
          (\(SimpleError str') -> SimpleError $ str' ++ " AND " ++ str) $ do
            isPostAuthor h  postIdParam usIdParam
            return ())

accessMode req = case fmap (isExistParam req) ["user_id","admin_id"] of
  [True,_] -> UserMode
  [_,True] -> AdminMode
  _        -> UserMode

data AccessMode = UserMode | AdminMode

isCommAuthor h  commIdParam usIdParam = do
  OnlyInt usId <- selectOneFromDbE h "comments" ["user_id"] "comment_id=?" [commIdParam]  
  case usId == (read . unpack $ usIdParam) of
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is not author of comment_id: " ++ unpack commIdParam

hideErr m = m `catchE` (\e -> throwE $ toSecret e)

toSecret (SimpleError str) = SecretError str
toSecret (SecretError str) = SecretError str

inCommResp (Comment id usId txt) = CommentIdTextUserResponse id txt usId     

isNULL 0      = PostText    "NULL" 
isNULL postId = PostInteger postId

isDraftAuthor h  draftIdParam usIdParam = do
  OnlyInt auId <- selectOneFromDbE h "drafts" ["author_id"] "draft_id=?" [draftIdParam] 
  OnlyInt usDraftId <- selectOneFromDbE h "authors" ["user_id"] "author_id=?" [pack . show $ auId]  
  case usDraftId == (read . unpack $ usIdParam) of
    True -> return auId
    False -> throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is not author of draft_id: " ++ unpack draftIdParam

isPostAuthor h  postIdParam usIdParam = do
  OnlyInt auId <- selectOneFromDbE h "posts" ["author_id"] "post_id=?" [postIdParam] 
  OnlyInt usPostId <- selectOneFromDbE h "authors" ["user_id"] "author_id=?" [pack . show $ (auId :: Integer)]  
  case (usPostId :: Integer) == (read . unpack $ usIdParam) of
    True -> return auId
    False -> throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is not author of post_id: " ++ unpack postIdParam

inTagResp (Tag tagId tagName) = TagResponse tagId tagName

makeMyPicUrl picId = pack $ "http://localhost:3000/picture/" ++ show picId

inPicIdUrl picId    = PicIdUrl picId (makeMyPicUrl picId)

checkDraftReqJson json = do 
  case (decode json :: Maybe DraftRequest) of
    Just body -> return body
    Nothing   -> case (decode json :: Maybe Object) of
      Just obj -> do
        let numParams = ["user_id","draft_category_id"]
        let textParams = ["password","draft_name","draft_text","draft_main_pic_url"]
        let arrayParams = ["draft_tags_ids","draft_pics_urls"]
        let params = numParams ++ textParams ++ arrayParams
        [usIdVal,catIdVal,pwdVal,nameVal,txtVal,mainPicUrlVal,tagsIdsVal,picsUrlsVal] <- mapM (isExistInObj obj) params
        mapM checkNumVal [usIdVal,catIdVal]
        mapM checkStrVal [pwdVal,nameVal,txtVal,mainPicUrlVal]
        checkNumArrVal tagsIdsVal
        checkStrArrVal picsUrlsVal
        throwE $ SimpleError $ "Can`t parse request body"
      Nothing -> throwE $ SimpleError $ "Invalid request body"

{-checkTagArrVal val = do
  case val of
    Array arr -> case V.toList arr of
      [] -> return ()
      xs@((Object obj) : objs) -> mapM_ checkTagObj xs
      _ -> throwE $ SimpleError $ "Can`t parse  \"draft_tags_ids\"  parameters"

checkTagObj (Object obj) = case toList obj of
  [("tag_id",Number _)] -> return ()
  _                     -> throwE $ SimpleError $ "Can`t parse  \"draft_tags_ids\"  parameter"

checkPicArrVal val = do
  case val of
    Array arr -> case V.toList arr of
      [] -> return ()
      xs@((Object obj) : objs) -> mapM_ checkPicObj xs
      _ -> throwE $ SimpleError $ "Can`t parse  \"draft_pics_urls\"  parameters"

checkPicObj (Object obj) = case toList obj of
  [("pic_url",String _)] -> return ()
  _                      -> throwE $ SimpleError $ "Can`t parse  \"draft_pics_urls\"  parameter"
-}

isExistInObj obj param = do
  case lookup param . toList $ obj of
    Just val -> return val
    Nothing -> throwE $ SimpleError $ "Can`t find parameter: " ++ unpack param

checkNumVal val = do
  case val of
    Number _ -> return ()
    _ -> throwE $ SimpleError $ "Can`t parse parameter value: " ++ show val

checkStrVal val = do
  case val of
    String _ -> return ()
    _ -> throwE $ SimpleError $ "Can`t parse parameter value: " ++ show val

checkNumArrVal values = do
  case values of
    Array arr -> case V.toList arr of
      [] -> return ()
      ((Number _) : vals) -> return ()
      _ -> throwE $ SimpleError $ "Can`t parse parameter values: " ++ show values
    _ -> throwE $ SimpleError $ "Can`t parse parameter values: " ++ show values

checkStrArrVal values = do
  case values of
    Array arr -> case V.toList arr of
      [] -> return ()
      ((String _) : vals) -> return ()
      _ -> throwE $ SimpleError $ "Can`t parse parameter values: " ++ show values
    _ -> throwE $ SimpleError $ "Can`t parse parameter values: " ++ show values

checkRelationCats h  catIdNum superCatIdNum 
  |catIdNum == superCatIdNum = throwE $ SimpleError $ "super_category_id: " ++ show superCatIdNum ++ " equal to category_id."
  |otherwise                 = do
    allSubCats <- findAllSubCats h  catIdNum
    if superCatIdNum `elem` allSubCats
      then throwE $ SimpleError $ "super_category_id: " ++ show superCatIdNum ++ " is subCategory of category_id: " ++ show catIdNum
      else return ()

findAllSubCats :: (Monad m, MonadCatch m,MonadFail m) => Handle m -> Integer -> ExceptT ReqError m [Integer]
findAllSubCats h  catId = do
  catsIds <- findOneLevelSubCats h catId 
  case catsIds of
    [] -> return [catId]
    _  -> do       
      subCatsIds <- mapM (findAllSubCats h ) catsIds
      return $ catId : (Prelude.concat  subCatsIds)

findOneLevelSubCats h catId = do
    catsIds <- selectListFromDbE h "categories" ["category_id"] "super_category_id=?" [pack . show $ catId]
    return $ fmap fromOnlyInt catsIds
    
findAllSuperCats :: (Monad m, MonadCatch m,MonadFail m) => Handle m -> Integer -> ExceptT ReqError m [(Integer,Text,[Integer])]
findAllSuperCats h  catId = do
  Cat catName superCatId <- selectOneFromDbE h "categories" ["category_name","COALESCE (super_category_id, '0') AS super_category_id"] "category_id=?" [pack . show $ catId] 
  subCatsIds <- findOneLevelSubCats h catId
  case superCatId of 
    0 -> return $ [(catId,catName,subCatsIds)]
    _ -> do
      xs <- findAllSuperCats h  superCatId
      return $ ((catId,catName,subCatsIds) : xs) 

deleteAllAboutDrafts :: (Monad m, MonadCatch m) => Handle m -> [Integer] -> ExceptT ReqError m ()
deleteAllAboutDrafts h [] = return ()
deleteAllAboutDrafts h draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteDraftsPicsTags h draftsIds
  deleteFromDbE h "drafts" where' values
  return ()

deleteDraftsPicsTags :: (Monad m, MonadCatch m) => Handle m -> [Integer] -> ExceptT ReqError m ()
deleteDraftsPicsTags h [] = return ()
deleteDraftsPicsTags h draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteFromDbE h "draftspics" where' values
  deleteFromDbE h "draftstags" where' values
  return ()

deleteAllAboutPosts :: (Monad m, MonadCatch m) => Handle m -> [Integer] -> ExceptT ReqError m ()
deleteAllAboutPosts h [] = return ()
deleteAllAboutPosts h postsIds = do
  let values = fmap (pack . show) postsIds
  let where' = intercalate " OR " . fmap (const "post_id=?") $ postsIds
  deletePostsPicsTags h postsIds
  deleteFromDbE h "comments" where' values
  draftsIdsSel <- selectListFromDbE h "drafts" ["draft_id"] where' values  
  let draftsIds = fmap fromOnlyInt draftsIdsSel
  deleteAllAboutDrafts h draftsIds
  deleteFromDbE h "drafts" where' values
  deleteFromDbE h "posts" where' values
  return ()

deletePostsPicsTags :: (Monad m, MonadCatch m) => Handle m -> [Integer] -> ExceptT ReqError m ()
deletePostsPicsTags h [] = return ()
deletePostsPicsTags h postsIds = do
  let values = fmap (pack . show) postsIds
  let where' = intercalate " OR " . fmap (const "post_id=?") $ postsIds
  deleteFromDbE h "postspics" where' values
  deleteFromDbE h "poststags" where' values
  return ()

isUserAuthorBool h usIdParam = do
  lift $ logDebug (hLog h) $ "Checking in DB is user author"  
  lift $ isExistInDb h "authors" "user_id" "user_id=?" [pack . show $ usIdParam]

isUserAuthorE h  usIdParam = do
  lift $ logDebug (hLog h) $ "Checking in DB is user author"  
  check <- lift $ isExistInDb h "authors" "user_id" "user_id=?" [pack . show $ usIdParam]
  case check of 
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ show usIdParam ++ " isn`t author."
    

checkRelationUsAu h usIdParam auIdParam = do
  check <- lift $ isExistInDb h "authors" "user_id" "user_id=?" [usIdParam] 
  case check of
    True -> do
      OnlyInt auId <- selectOneFromDbE h "authors" ["author_id"] "user_id=?" [usIdParam] 
      case auId == (read . unpack $ auIdParam) of
        True  -> return ()
        False -> throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is already author"
    False -> return ()  

checkKeyE :: (Monad m) => Text -> Text -> ExceptT ReqError m Bool
checkKeyE keyParam key 
  | keyParam == key = return True
  | otherwise       = throwE $ SimpleError "Invalid create_admin_key"

checkParam :: (Monad m) => Request -> Text -> ExceptT ReqError m Text
checkParam req param = (do
  case lookup param $ queryToQueryText $ queryString req of
    Just (Just "") -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack param ++ ". Empty input."
    Just (Just x)  -> return x
    Just Nothing   -> throwE $ SimpleError $ "Can't parse parameter:" ++ unpack param
    Nothing -> throwE $ SimpleError $ "Can't find parameter:" ++ unpack param)

checkEmptyList [] = throwE $ SimpleError "DatabaseError.Empty output"
checkEmptyList _  = return ()

tryRead :: (Monad m) => Text -> ExceptT ReqError m Integer
tryRead "" = throwE $ SimpleError "Can`t parse parameter. Empty input."
tryRead xs = case reads . unpack $ xs of
  [(a,"")] -> return a
  _        -> throwE $ SimpleError $ "Can`t parse value:" ++ unpack xs

toSelQ table params where' = 
  fromString $ "SELECT " ++ (intercalate ", " params) ++ " FROM " ++ table ++ " WHERE " ++ where'
toSelLimQ table orderBy page limitNumber params where' = 
  fromString $ "SELECT " ++ (intercalate ", " params) ++ " FROM " ++ table ++ " WHERE " ++ where' ++ " ORDER BY " ++ orderBy ++ " OFFSET " ++ show ((page -1)*limitNumber) ++ " LIMIT " ++ show (page*limitNumber)
toUpdQ table set where' = 
  fromString $ "UPDATE " ++ table ++ " SET " ++ set ++ " WHERE " ++ where' 
toDelQ table where' =
  fromString $ "DELETE FROM " ++ table ++ " WHERE " ++ where'
toExQ table checkName where' =
  fromString $ "SELECT EXISTS (SELECT " ++ checkName ++ " FROM " ++ table ++ " WHERE " ++ where' ++ ")"
toInsRetQ table returnName insNames =
  fromString $ "INSERT INTO " ++ table ++ " ( " ++ intercalate "," insNames ++ " ) VALUES ( " ++ (intercalate "," . fmap (const "?") $ insNames) ++ " ) RETURNING " ++ returnName
toInsManyQ table insNames =
  fromString $ "INSERT INTO " ++ table ++ " ( " ++ intercalate "," insNames ++ " ) VALUES ( " ++ (intercalate "," . fmap (const "?") $ insNames) ++ " ) "

selectFromDb' :: Connection -> String -> [String] -> String -> [Text] -> IO [SelectType]
selectFromDb' conn table params where' values = do
  xs <- query conn (toSelQ table params where') values
  return xs

selectOneFromDbE :: (Monad m, MonadCatch m) => Handle m -> String -> [String] -> String -> [Text] -> ExceptT ReqError m SelectType
selectOneFromDbE h table params where' values = catchDbErr $ do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table 
  xs <- lift $ selectFromDb h table params where' values
  case xs of
    []           -> throwE $ DatabaseError "DatabaseError.Empty output"
    [x] -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs

selectListFromDbE :: (Monad m, MonadCatch m) => Handle m -> String -> [String] -> String -> [Text] -> ExceptT ReqError m [SelectType]
selectListFromDbE h table params where' values = catchDbErr $ do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table
  xs <- lift $ selectFromDb h table params where' values
  lift $ logInfo (hLog h) $ "Data received from DB"
  return xs 


selectLimitFromDb' :: Connection -> String -> String -> Integer -> Integer -> [String] -> String -> [Text] -> IO [SelectType]
selectLimitFromDb' conn table orderBy page limitNumber params where' values = do
  xs <- query conn (toSelLimQ table orderBy page limitNumber params where') values
  return xs

--selectListLimitFromDbE :: (Monad m, MonadCatch m, ToRow q, FromRow r) => Handle m -> Connection -> Query -> q -> m [r]
selectListLimitFromDbE h table orderBy page limitNumber params where' values = catchDbErr $ do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table
  xs <- lift $ selectLimitFromDb h table orderBy page limitNumber params where' values
  lift $ logInfo (hLog h) $ "Data received from DB"
  return xs

updateInDb' :: Connection -> String -> String -> String -> [Text] -> IO ()
updateInDb' conn table set where' values = do
  execute conn (toUpdQ table set where') values
  return ()

updateInDbE h table set where' values = catchDbErr $ do
  lift $ updateInDb h table set where' values

deleteFromDb' :: Connection -> String -> String -> [Text] -> IO ()
deleteFromDb' conn table where' values = do
  execute conn (toDelQ table where') values
  return ()   

deleteFromDbE h table where' values = catchDbErr $ do
  lift $ deleteFromDb h table where' values   

isExistInDb' :: Connection -> String -> String -> String -> [Text] -> IO Bool
isExistInDb' conn table checkName where' values = do
  [Only check]  <- query conn (toExQ table checkName where') values
  return check

isExistInDbE :: (Monad m, MonadCatch m,MonadFail m) => Handle m -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
isExistInDbE h table checkName where' values = do
  lift $ logDebug (hLog h) $ "Checking existence entity (" ++ checkName ++ ") in the DB"
  check  <- lift $ isExistInDb h table checkName where' values 
  case check of
    True -> do
      lift $ logInfo (hLog h) $ "Entity (" ++ checkName ++ ") exist"
      return ()
    False -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " doesn`t exist."
  
ifExistInDbThrowE :: (Monad m, MonadCatch m,MonadFail m) => Handle m -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
ifExistInDbThrowE h table checkName where' values = do
  lift $ logDebug (hLog h) $ "Checking existence entity (" ++ checkName ++ ") in the DB"
  check  <- lift $ isExistInDb h table checkName where' values 
  case check of
    True -> throwE $ SimpleError $ checkName ++ ": " ++ (intercalate "," . fmap unpack $ values) ++ " already exist in " ++ table
    False -> do
      lift $ logInfo (hLog h) $ "Entity (" ++ checkName ++ ") doesn`t exist"
      return ()


insertReturnInDb' :: Connection -> String -> String -> [String] -> [Text] -> IO [Integer]
insertReturnInDb' conn table returnName insNames insValues = do
  xs <- query conn ( toInsRetQ table returnName insNames ) insValues 
  return (fmap fromOnly xs) 

insertReturnInDbE :: (Monad m, MonadCatch m, MonadFail m) => Handle m -> String -> String -> [String] -> [Text] -> ExceptT ReqError m Integer
insertReturnInDbE h table returnName insNames insValues = catchDbErr $ do
  xs <- lift $ insertReturnInDb h table returnName insNames insValues
  case xs of
    []           -> throwE $ DatabaseError "DatabaseError.Empty output"
    [x] -> do 
      lift $ logInfo (hLog h) $ "Data received from DB"
      return x
    _            -> throwE $ DatabaseError $ "DatabaseError. Output not single" ++ show xs


insertManyInDb' :: Connection -> String -> [String] -> [(Integer,Integer)] -> IO ()
insertManyInDb' conn table insNames insValues = do
  executeMany conn ( toInsManyQ table insNames ) insValues
  return ()

insertManyInDbE :: (Monad m, MonadCatch m, MonadFail m) => Handle m -> String -> [String] -> [(Integer,Integer)] -> ExceptT ReqError m ()
insertManyInDbE h table insNames insValues = catchDbErr $ do
  lift $ insertManyInDb h table insNames insValues




isMyUrl url
  | isPrefixOf "http://localhost:3000" url = True
  | otherwise                              = False

picUrlEnd url = stripPrefix "http://localhost:3000/picture/" url

checkMyPicUrl :: (Monad m) => Text -> ExceptT ReqError m Text
checkMyPicUrl url = do
  case picUrlEnd url of
    Just ""     -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url
    Just urlEnd -> return urlEnd
    Nothing     -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url 

readUrlEnd :: (Monad m,MonadCatch m,MonadFail m) => Handle m -> Text -> Text -> ExceptT ReqError m Integer
readUrlEnd h url urlEnd = do
  picIdNum <- tryRead urlEnd 
  check    <- lift $ isExistInDb h "pics" "pic_id" "pic_id=?" [(pack . show $ picIdNum)] 
  case check of 
    True  -> return picIdNum 
    False -> throwE $ SimpleError $ "Invalid end of picture url:" ++ unpack url

checkPicUrlGetPic :: (Monad m,MonadCatch m) => Handle m -> Text -> ExceptT ReqError m (HT.Response BSL.ByteString)
checkPicUrlGetPic h url = do
  res <- (lift $ (httpAction h) . fromString . unpack $ url) `catch` ( (\e -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url ++ ". " ++ (show (e :: HT.HttpException))) )
  let bs = HT.getResponseBody res
  case decodeImage $ BSL.toStrict bs of
    Right _ -> return res
    Left _  -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url


getPicId :: (Monad m,MonadCatch m,MonadFail m) => Handle m -> Text -> ExceptT ReqError m Integer
getPicId h url 
  |isMyUrl url = do
    picId <- (checkMyPicUrl url >>= \urlEnd -> readUrlEnd h url urlEnd)
    return picId
  |otherwise = do
    checkPicUrlGetPic h url
    picId    <-  insertReturnInDbE h "pics" "pic_id" ["pic_url"] [url] 
    return picId


chooseArgs req = do
  let filterDateList   = ["created_at","created_at_lt","created_at_gt"] 
  let filterTagList    = ["tag","tags_in","tags_all"] 
  let filterInList     = ["name_in","text_in","everywhere_in"] 
  let filterParamsList = filterDateList ++ ["category_id","author_name"] ++ filterTagList ++ filterInList 
  let sortList         = ["sort_by_pics_number","sort_by_category","sort_by_author","sort_by_date"] 
  mapM_ (checkComb req) [filterDateList,filterTagList,filterInList]
  manyFilterArgs <- mapM (checkFilterParam req) $ filterParamsList
  let filterArgs = Prelude.concat manyFilterArgs
  manySortArgs <- mapM (checkSortParam req) $ sortList
  let sortArgs = Prelude.concat manySortArgs
  if isDateASC $ sortArgs
    then do
      let table     = intercalate " " $ ["posts JOIN authors ON authors.author_id = posts.author_id"] ++ (firstThree . unzip3 $ filterArgs) ++ (firstThree . unzip3 $ sortArgs)  
      let where'    = intercalate " AND " $ (secondThree . unzip3 $ filterArgs) ++ ["true"]
      let orderBy   = intercalate "," $ (secondThree . unzip3 $ sortArgs) ++ ["post_create_date ASC, post_id ASC"]
      let values    = (Prelude.concat . fmap fst . thirdThree . unzip3 $ filterArgs) ++  (Prelude.concat . fmap snd . thirdThree . unzip3 $ filterArgs)
      return (table,where',orderBy,values)
    else do
      let table     = intercalate " " $ ["posts JOIN authors ON authors.author_id = posts.author_id"] ++ (firstThree . unzip3 $ filterArgs) ++ (firstThree . unzip3 $ sortArgs) 
      let where'    = intercalate " AND " $ (secondThree . unzip3 $ filterArgs) ++ ["true"]
      let orderBy   = intercalate "," $ (secondThree . unzip3 $ sortArgs) ++ ["post_create_date DESC, post_id DESC"]
      let values    = (Prelude.concat . fmap fst . thirdThree . unzip3 $ filterArgs) ++  (Prelude.concat . fmap snd . thirdThree . unzip3 $ filterArgs)
      return (table,where',orderBy,values)


isDateASC xs = foldr (\(a,b,c) cont -> if c == DateASC then True else cont) False xs


checkComb req list = case fmap (isExistParam req) list of
     (True:True:_)   -> throwE $ SimpleError $ "Invalid combination of filter parameters" 
     (_:True:True:_) -> throwE $ SimpleError $ "Invalid combination of filter parameters"
     (True:_:True:_) -> throwE $ SimpleError $ "Invalid combination of filter parameters"
     _               -> return ()

checkFilterParam :: (Monad m) => Request -> Text -> ExceptT ReqError m [(String,String,([Text],[Text]))]
checkFilterParam req param =
  case isExistParam req param of
    False -> return []
    True  -> case parseParam req param of
      Just txt -> chooseFilterArgs txt param
      _ ->  throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack param

chooseFilterArgs x param = case param of
  "created_at" -> do
    let table   = ""
    let where'  = "post_create_date = ?"
    let values  = ([],[x])
    return [(table,where',values)]
  "created_at_lt" -> do
    let table   = ""
    let where'  = "post_create_date < ?"
    let values  = ([],[x])
    return [(table,where',values)]
  "created_at_gt" -> do
    let table   = ""
    let where'  = "post_create_date < ?"
    let values  = ([],[x])
    return [(table,where',values)]
  "category_id" -> do
    let table   = ""
    let where'  = "post_category_id = ?"
    let values  = ([],[x])
    return [(table,where',values)]
  "tag" -> do
    let table   = "JOIN (SELECT post_id FROM poststags WHERE tag_id = ? GROUP BY post_id) AS t ON posts.post_id=t.post_id"
    let where'  = "true"
    let values  = ([x],[])
    return [(table,where',values)]
  "tags_in" -> do
    let table   = "JOIN (SELECT post_id FROM poststags WHERE tag_id IN (" ++ (init . tail . unpack $ x) ++ ") GROUP BY post_id) AS t ON posts.post_id=t.post_id"
    let where'  = "true"
    let values  = ([],[])
    return [(table,where',values)]
  "tags_all" -> do
    let table   = "JOIN (SELECT post_id, array_agg(ARRAY[tag_id]) AS tags_id FROM poststags GROUP BY post_id) AS t ON posts.post_id=t.post_id"
    let where'  = "tags_id @> ARRAY" ++ unpack x ++ "::bigint[]"
    let values  = ([],[])
    return [(table,where',values)]
  "name_in" -> do 
    let table   = ""
    let where'  = "post_name ILIKE ?"
    let values  = ([],[Data.Text.concat ["%",x,"%"]])          
    return [(table,where',values)]
  "text_in" -> do
    let table   = ""
    let where'  = "post_text ILIKE ?"
    let values  = ([],[Data.Text.concat ["%",x,"%"]])          
    return [(table,where',values)]
  "everywhere_in" -> do
    let table   = "JOIN users AS usrs ON authors.user_id=usrs.user_id JOIN categories AS c ON c.category_id=posts.post_category_id JOIN (SELECT pt.post_id, bool_or(tag_name ILIKE ? ) AS isintag FROM poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id  GROUP BY pt.post_id) AS tg ON tg.post_id=posts.post_id"
    let where'  = "(post_text ILIKE ? OR post_name ILIKE ? OR usrs.first_name ILIKE ? OR c.category_name ILIKE ? OR isintag = TRUE)"
    let values  = ([Data.Text.concat ["%",x,"%"]],replicate 4 $ Data.Text.concat ["%",x,"%"])
    return [(table,where',values)]
  "author_name" -> do
    let table   = "JOIN users AS us ON authors.user_id=us.user_id"
    let where'  = "us.first_name = ?"
    let values  = ([],[x])
    return [(table,where',values)]     
  _ -> throwE $ SimpleError $ "Can`t parse query parameter" ++ unpack param
        

checkSortParam :: (Monad m) => Request -> Text -> ExceptT ReqError m [(String,String,SortDate)] 
checkSortParam req param = case isExistParam req param of
  False -> return []
  True  -> do
    case parseParam req param of
      Just txt -> chooseSortArgs txt param
      _ -> throwE $ SimpleError $ "Can`t parse parameter: " ++ unpack param

chooseSortArgs "DESC" param = case param of
    "sort_by_pics_number" -> do
      let joinTable = "JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
      let orderBy = "count_pics DESC"
      return [(joinTable,orderBy,defDateSort)]
    "sort_by_category" -> do
      let joinTable = "JOIN categories ON posts.post_category_id=categories.category_id"
      let orderBy = "category_name DESC"
      return [(joinTable,orderBy,defDateSort)]
    "sort_by_author" -> do
      let joinTable = "JOIN users AS u ON authors.user_id=u.user_id"
      let orderBy = "u.first_name DESC"
      return [(joinTable,orderBy,defDateSort)]
    "sort_by_date" -> do
      let joinTable = ""
      let orderBy = "true"
      return [(joinTable,orderBy,DateDESC)]
    _ -> throwE $ SimpleError $ "Can`t parse query parameter" ++ unpack param
chooseSortArgs "ASC" param =
  case param of
    "sort_by_pics_number" -> do
      let joinTable = "JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
      let orderBy = "count_pics ASC"
      return [(joinTable,orderBy,defDateSort)]
    "sort_by_category" -> do
      let joinTable = "JOIN categories ON posts.post_category_id=categories.category_id"
      let orderBy = "category_name ASC"
      return [(joinTable,orderBy,defDateSort)]
    "sort_by_author" -> do
      let joinTable = "JOIN users AS u ON authors.user_id=u.user_id"
      let orderBy = "u.first_name ASC"
      return [(joinTable,orderBy,defDateSort)]
    "sort_by_date" -> do 
      let joinTable = ""
      let orderBy = "true"
      return [(joinTable,orderBy,DateASC)]
    _ -> throwE $ SimpleError $ "Can`t parse query parameter" ++ unpack param 
chooseSortArgs txt param 
  | Data.Text.toUpper txt == "ASC"  = chooseSortArgs "ASC"  param
  | Data.Text.toUpper txt == "DESC" = chooseSortArgs "DESC" param
  | otherwise                       = throwE $ SimpleError $ "Invalid sort value" ++ unpack txt


data SortDate = DateASC | DateDESC 
 deriving (Eq,Show,Read)

defDateSort = DateDESC                                                                           

isExistParam req txt = case lookup txt $ queryToQueryText $ queryString req of
  Just _  -> True
  Nothing -> False

data ReqError = SecretError String | SimpleError String | DatabaseError String
  deriving (Eq,Show)

data DbException = SqlError | ExecStatus | FormatError| QueryError | ResultError deriving Show
instance Exception DbException

catchDbErr m = m `catch` (\e -> do
  throwE . DatabaseError $ show (e :: DbException) )

{-instance Monoid ReqError where
  mempty = SimpleError mempty
  mappend ( SimpleError a ) ( SimpleError b ) = SimpleError (a `mappend` b)-}


isExistParamE :: (Monad m) => Request -> Text -> ExceptT ReqError m (Maybe Text)
isExistParamE req param = case lookup param $ queryToQueryText $ queryString req of
  Just x  -> return x
  Nothing -> throwE $ SimpleError $ "Can't find param" ++ unpack param

parseParam req txt = fromJust . lookup txt $ queryToQueryText $ queryString req

parseParamE req param = case fromJust . lookup param $ queryToQueryText $ queryString req of
  Just x  -> Right x
  Nothing -> Left $ SimpleError $ "Can't parse param" ++ unpack param

adminAuthE h req = do
  let authParams  = ["admin_id","password"]
  [admIdParam,pwdParam] <- hideErr $ mapM (checkParam req) authParams
  [admIdNum]            <- hideErr $ mapM tryRead [admIdParam]
  lift $ logInfo (hLog h) $ "All authorize parameters parsed"
  hideErr $ isExistInDbE h "users" "user_id" "user_id=?" [admIdParam] 
  Auth pwd admBool <- hideErr $ selectOneFromDbE h "users" ["password","admin"] "user_id=?" [admIdParam] 
  hideErr $ adminAuth pwdParam pwd admBool

adminAuth pwdParam pwd admBool
  | admBool && (pwd == pwdParam) = return True
  | admBool                      = throwE . SimpleError $ "INVALID pwd, admin = True "
  | (pwd == pwdParam)            = throwE . SimpleError $ "valid pwd, user is NOT admin"
  | otherwise                    = throwE . SimpleError $ "INVALID pwd, user is NOT admin"

userAuth pwdParam pwd 
  | pwd == pwdParam = return ()
  | otherwise       = throwE . SimpleError $ "INVALID password"


inCatResp [(x,y,z)] = CatResponse { cat_id = x , cat_name =  y, one_level_sub_cats = z , super_cat = "NULL"}
inCatResp ((x,y,z):xs) = SubCatResponse { subCat_id = x , subCat_name =  y , one_level_sub_categories = z , super_category = inCatResp xs} 
      





firstThree  (a,b,c) = a
secondThree (a,b,c) = b
thirdThree  (a,b,c) = c


