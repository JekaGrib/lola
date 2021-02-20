{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Test.Hspec
import           Control.Monad.State            

import           Logger
import           Api
import           Network.Wai
import           Network.HTTP.Types             ( status200, status404, status301, movedPermanently301, http11 )
import           Network.HTTP.Types.URI         ( queryToQueryText )
import           Network.Wai.Handler.Warp       ( run )
import           Data.Aeson
import           Data.Text                      ( pack, unpack, Text, concat, toUpper, stripPrefix, isPrefixOf )
import           Data.ByteString.Builder        ( lazyByteString )
import           Database.PostgreSQL.Simple
import qualified Network.HTTP.Simple            as HT
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Calendar             ( showGregorian, Day )
import           Database.PostgreSQL.Simple.Time
import           Data.String                    ( fromString )
import           Data.List                      ( intercalate, zip4 )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )
import           Codec.Picture                  ( decodeImage )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy           as BSL
import           Control.Monad.Catch            ( catch, throwM, MonadCatch )
import           Data.HashMap.Strict            ( toList, fromList )
import qualified Data.Vector                    as V
import           Data.Int                       ( Int64 )
import qualified Database.PostgreSQL.Simple.FromField as FF
import qualified Data.Map                       as M

data MockAction = EXISTCHEK | LOGMSG
  deriving (Eq,Show)
type TestDB = M.Map Integer Text 

logTest :: Priority -> String -> StateT (TestDB,[MockAction]) IO ()
logTest prio text = StateT $ \(db,acts) -> 
  return (() , (db,LOGMSG : acts))


testDB1 = M.fromList [(1,"fill"),(2,"call"),(3,"bill")]

isExistInDbTest :: String -> String -> String -> [Text] -> StateT (TestDB,[MockAction]) IO Bool
isExistInDbTest s s' s'' (x:xs) = StateT $ \(db,acts) -> do
  return ( M.member (read . unpack $ x) db , (db,EXISTCHEK:acts))


handleLog1 = LogHandle (LogConfig DEBUG) logTest
handle1 = Handle 
  { hLog = handleLog1,
    isExistInDb = isExistInDbTest
    }

reqTest = defaultRequest {requestMethod = "GET", httpVersion = http11, rawPathInfo = "/test/3", rawQueryString = "", requestHeaders = [("User-Agent","PostmanRuntime/7.26.8"),("Accept","*/*"),("Postman-Token","6189d61d-fa65-4fb6-a578-c4061535e7ef"),("Host","localhost:3000"),("Accept-Encoding","gzip, deflate, br"),("Connection","keep-alive"),("Content-Type","multipart/form-data; boundary=--------------------------595887703656508108682668"),("Content-Length","170")], isSecure = False, pathInfo = ["test","3"], queryString = [], requestBodyLength = KnownLength 170, requestHeaderHost = Just "localhost:3000", requestHeaderRange = Nothing}

testT :: IO ()
testT = hspec $ do
  describe "CheckExist" $ do
    it "work" $ do
      let conn = undefined
      state <- execStateT (runExceptT $ answerEx handle1 conn reqTest) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG]

data Handle m = Handle 
  { hLog             :: LogHandle m,
    dbQuery          :: forall q r. (ToRow q, FromRow r) => Connection -> Query -> q -> m [r],
    dbExecute        :: forall q. ToRow q => Connection -> Query ->  q  -> m Int64 ,
    dbExecuteMany    :: forall q. ToRow q => Connection -> Query -> [q] -> m Int64 ,
    isExistInDb      :: String -> String -> String -> [Text] -> m Bool,
    httpAction       :: HT.Request -> m (HT.Response BSL.ByteString),
    getDay           :: m String,
    getBody          :: Request -> m BSL.ByteString
    }

--forall (m :: * -> *) q r.(Monad m, MonadCatch m, ToRow q, FromRow r) => Handle m -> Connection -> Query -> q -> m [r]

defaultPictureUrl :: Text
defaultPictureUrl = "https://cdn.pixabay.com/photo/2020/01/14/09/20/anonym-4764566_960_720.jpg"
defUsId = 1
defPicId = 1
defAuthId = 1
defCatId = 1

commentNumberLimit = 20
draftNumberLimit = 5
postNumberLimit = 5


getDay' :: IO String
getDay' = do
  time    <- getZonedTime
  let day = showGregorian . localDay . zonedTimeToLocalTime $ time
  return day



getTime :: IO String
getTime = do
  time    <- getZonedTime
  return $ show time     
   

createDefaultPicture :: IO Integer
createDefaultPicture = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only picId] <- query conn "INSERT INTO pics ( pic_url ) VALUES (?) RETURNING pic_id " [defaultPictureUrl]
  return picId

createDefaultUser picId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  day <- getDay'  
  [Only userId] <- query conn "INSERT INTO users ( password, first_name , last_name , user_pic_id , user_create_date, admin) VALUES ( '12345678','DELETED','DELETED',?,?, false ) RETURNING user_id" [ pack (show picId), pack day ]
  return userId

createDefaultAuthor userId = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only authorId] <- query conn "INSERT INTO authors ( user_id , author_info) VALUES ( ?,'DELETED' ) RETURNING author_id" [pack . show $ userId]  
  return authorId

createDefaultCategory :: IO Integer
createDefaultCategory = do
  conn <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  [Only catId] <- query_ conn "INSERT INTO categories (category_name) VALUES ( 'NONE' ) RETURNING category_id" 
  return catId



addCreateAdminKey = do
  conn1 <- connectPostgreSQL "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'"
  execute_ conn1 "INSERT INTO key (create_admin_key) VALUES ( 'lola' ) "


addDefaultParameters = do
  addCreateAdminKey
  picId <- createDefaultPicture
  userId <- createDefaultUser picId
  authorId <- createDefaultAuthor userId
  createDefaultCategory
  return [picId,userId,authorId]
  


logOnErr h m = m `catchE` (\e -> do
  lift $ logWarning (hLog h) $ show e
  throwE e)

main :: IO ()
main = do
  addDefaultParameters
  time <- getTime                          
  let currLogPath = "./PostApp.LogSession: " ++ show time ++ " bot.log"
  let handleLog = LogHandle (LogConfig DEBUG) (logger handleLog currLogPath) 
  run 3000 (application handleLog)

application :: LogHandle IO -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application handleLog req send = do
  conn <- connectPostgreSQL (fromString $ "host='localhost' port=5432 user='evgenya' dbname='newdb' password='123456'")
  let h = Handle handleLog query execute executeMany (isExistInDb' conn) HT.httpLBS getDay' strictRequestBody
  logDebug (hLog h) "Connect to DB"
  ansE <- runExceptT $ logOnErr h $ answerEx h conn req 
  send ( fromE ansE )



fromE :: Either ReqError Response -> Response
fromE ansE = 
  let helper = responseBuilder status200 [("Content-Type", "application/json; charset=utf-8")] . lazyByteString in
  case ansE of
    Right a                -> a
    Left (SimpleError str) -> helper . encode $ OkInfoResponse {ok7 = False, info7 = pack str}
    Left (SecretError str) -> responseBuilder status404 [] $ "Status 404 Not Found"

okHelper h x = do
  let res = encode x
  lift $ logDebug (hLog h) $ "Output response: " ++ show res
  return . responseBuilder status200 [("Content-Type", "application/json; charset=utf-8")] . lazyByteString $ res

answerEx :: (Monad m, MonadCatch m,MonadFail m) => Handle m -> Connection -> Request -> ExceptT ReqError m Response
answerEx h conn req = do
   lift $ logDebug (hLog h) $ "Incoming request: " ++ show req
   case pathInfo req of
    ["createUser"] -> do
      lift $ logInfo (hLog h) $ "Create user command"
      let paramsNames = ["password","first_name","last_name","user_pic_url"]
      [pwdParam,fNameParam,lNameParam,picUrlParam] <- mapM (checkParam req) paramsNames
      lift $ logInfo (hLog h) $ "All parameters parsed"
      picId <- getPicId h conn picUrlParam
      day <- lift $ getDay h
      let insNames  = ["password","first_name","last_name","user_pic_id"    ,"user_create_date","admin"]
      let insValues = [pwdParam  ,fNameParam  ,lNameParam ,pack (show picId),pack day          ,"FALSE"]
      usId <- lift $ insertReturnInDb h conn "users" "user_id" insNames insValues
      lift $ logDebug (hLog h) $ "DB return user_id" ++ show usId
      lift $ logInfo (hLog h) $ "User_id: " ++ show usId ++ " created"
      okHelper h $ UserResponse {user_id = usId, first_name = fNameParam, last_name = lNameParam, user_pic_id = picId, user_pic_url = makeMyPicUrl picId, user_create_date = pack day}
    ["getUser", usId] -> do
      lift $ logInfo (hLog h) $ "Get user command"
      usIdNum <- tryRead usId
      lift $ logInfo (hLog h) $ "All parameters parsed"
      let selectParams = ["first_name","last_name","user_pic_id","user_create_date"]
      isExistInDbE h "users" "user_id" "user_id=?" [usId] 
      (fName,lName,picId,usCreateDate) <- selectTupleFromDbE h conn "users" selectParams "user_id=?" [usId]
      okHelper h $ UserResponse {user_id = usIdNum, first_name = fName, last_name = lName, user_pic_id = picId, user_pic_url = makeMyPicUrl picId, user_create_date = pack . showGregorian $ usCreateDate}
    ["deleteUser"] -> do
      lift $ logInfo (hLog h) $ "Delete user command"
      adminAuthE h conn req
      let paramsNames = ["user_id"]
      [usIdParam] <- mapM (checkParam req) paramsNames
      [usIdNum]   <- mapM tryRead [usIdParam]
      lift $ logInfo (hLog h) $ "All parameters parsed"
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      lift $ updateInDb h conn "comments" "user_id=?" "user_id=?" [pack . show $ defUsId,usIdParam]
      check <- isUserAuthorBool h conn usIdNum 
      case check of
        True -> do
          authorId <- selectFromDbE h conn "authors" ["author_id"] "user_id=?" [usIdParam]  
          lift $ updateInDb h conn "posts" "author_id=?" "author_id=?" [pack . show $ defAuthId,pack . show $ (authorId :: Integer)]
          draftsIds <- selectListFromDbE h conn "drafts" ["draft_id"] "author_id=?" [pack . show $ authorId]  
          deleteAllAboutDrafts h conn draftsIds
          lift $ deleteFromDb h conn "authors" "author_id=?" [pack . show $ authorId]
          return () 
        False -> return () 
      lift $ deleteFromDb h conn "users" "user_id=?" [usIdParam]
      lift $ logInfo (hLog h) $ "User_id: " ++ show usIdNum ++ " deleted"
      okHelper h $ OkResponse {ok = True}
    ["createAdmin"]        -> do
      lift $ logInfo (hLog h) $ "Create admin command"
      let paramsNames = ["create_admin_key","password","first_name","last_name","user_pic_url"]
      [keyParam,pwdParam,fNameParam,lNameParam,picUrlParam] <- mapM (checkParam req) paramsNames
      keys <- selectListFromDbE h conn "key" ["create_admin_key"] "true" ([]::[Text])  
      checkEmptyList keys
      checkKeyE keyParam (last keys)
      picId <- getPicId h conn picUrlParam 
      day <- lift $ getDay h
      let insNames  = ["password","first_name","last_name","user_pic_id"    ,"user_create_date","admin"]
      let insValues = [pwdParam  ,fNameParam  ,lNameParam ,pack (show picId),pack day          ,"TRUE" ]
      admId <- lift $ insertReturnInDb h conn "users" "user_id" insNames insValues 
      lift $ logDebug (hLog h) $ "DB return user_id" ++ show admId
      lift $ logInfo (hLog h) $ "User_id: " ++ show admId ++ " created as admin"
      okHelper h $ UserResponse {user_id = admId, first_name = fNameParam, last_name = lNameParam, user_pic_id = picId, user_pic_url = makeMyPicUrl picId, user_create_date = pack day }
    ["createAuthor"]        -> do
      lift $ logInfo (hLog h) $ "Create author command"
      adminAuthE h conn req
      let paramsNames = ["user_id","author_info"]
      [usIdParam,auInfoParam] <- mapM (checkParam req) paramsNames  
      [usIdNum]               <- mapM tryRead [usIdParam]  
      isExistInDbE h  "users" "user_id"  "user_id=?" [usIdParam] 
      ifExistInDbThrowE h "authors" "user_id" "user_id=?" [usIdParam] 
      auId <- lift $ insertReturnInDb h conn "authors" "user_id" ["user_id","author_info"] [usIdParam,auInfoParam]
      lift $ logDebug (hLog h) $ "DB return author_id" ++ show auId
      lift $ logInfo (hLog h) $ "Author_id: " ++ show auId ++ " created"
      okHelper h $ AuthorResponse {author_id = auId, auth_user_id = usIdNum, author_info = auInfoParam}
    ["getAuthor"]        -> do
      lift $ logInfo (hLog h) $ "Get author command"
      adminAuthE h conn req
      let paramsNames = ["author_id"]
      [auIdParam] <- mapM (checkParam req) paramsNames
      [auIdNum]   <- mapM tryRead [auIdParam]
      isExistInDbE h "authors" "author_id" "author_id=?" [auIdParam] 
      (usId,auInfo) <- selectTupleFromDbE h conn "authors" ["user_id","author_info"] "author_id=?" [auIdParam] 
      okHelper h $ AuthorResponse {author_id = auIdNum, auth_user_id = usId, author_info = auInfo}
    ["updateAuthor"]        -> do
      lift $ logInfo (hLog h) $ "Update author command"
      adminAuthE h conn req
      let paramsNames = ["author_id","user_id","author_info"]
      [auIdParam,usIdParam,auInfoParam] <- mapM (checkParam req) paramsNames
      [auIdNum,usIdNum]                 <- mapM tryRead [auIdParam,usIdParam]
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      isExistInDbE h "authors" "author_id" "author_id=?" [auIdParam] 
      checkRelationUsAu h conn usIdParam auIdParam
      lift $ updateInDb h conn "authors" "author_info=?" "author_id=?" [auInfoParam,auIdParam]
      okHelper h $ AuthorResponse {author_id = auIdNum, auth_user_id = usIdNum, author_info = auInfoParam}
    ["deleteAuthor"]   -> do
      lift $ logInfo (hLog h) $ "Delete author command"
      adminAuthE h conn req
      let paramsNames = ["author_id"]
      [auIdParam] <- mapM (checkParam req) paramsNames
      [auIdNum]   <- mapM tryRead [auIdParam]
      isExistInDbE h "authors" "author_id" "author_id=?" [auIdParam] 
      lift $ updateInDb h conn "posts" "author_id=?" "author_id=?" [pack . show $ defAuthId,auIdParam]
      draftsIds <- selectListFromDbE h conn "drafts" ["draft_id"] "author_id=?" [auIdParam] 
      deleteAllAboutDrafts h conn draftsIds
      lift $ deleteFromDb h conn "authors" "author_id=?" [auIdParam]
      okHelper h $ OkResponse {ok = True}
    ["createCategory"]        -> do
      lift $ logInfo (hLog h) $ "Create category command"
      adminAuthE h conn req
      let paramsNames = ["category_name"]
      [catNameParam] <- mapM (checkParam req) paramsNames
      catId <- lift $ insertReturnInDb h conn "categories" "category_id" ["category_name"] [catNameParam] 
      okHelper h $ CatResponse {cat_id = catId, cat_name = catNameParam, super_cat = "NULL"}
    ["createSubCategory"]        -> do
      lift $ logInfo (hLog h) $ "Create sub category command"
      adminAuthE h conn req
      let paramsNames = ["category_name","super_category_id"]
      [catNameParam,superCatIdParam] <- mapM (checkParam req) paramsNames
      [superCatIdNum]                <- mapM tryRead [superCatIdParam] 
      isExistInDbE h "categories" "category_id" "category_id=?" [superCatIdParam] 
      catId <- lift $ insertReturnInDb h conn "categories" "category_id" ["category_name","super_category_id"] [catNameParam,superCatIdParam] 
      allSuperCats <- findAllSuperCats h conn catId
      okHelper h $ inCatResp allSuperCats
    ["getCategory", catId] -> do
      lift $ logInfo (hLog h) $ "Get category command"
      catIdNum <- tryRead catId
      isExistInDbE h "categories" "category_id" "category_id=?" [catId] 
      allSuperCats <- findAllSuperCats h conn catIdNum
      okHelper h $ inCatResp allSuperCats
    ["updateCategory"] -> do
      lift $ logInfo (hLog h) $ "Update category command"
      adminAuthE h conn req
      let paramsNames = ["category_id","category_name","super_category_id"]
      [catIdParam,catNameParam,superCatIdParam] <- mapM (checkParam req) paramsNames
      [catIdNum,superCatIdNum]                  <- mapM tryRead [catIdParam,superCatIdParam]     
      isExistInDbE h "categories" "category_id" "category_id=?" [catIdParam]      
      isExistInDbE h "categories" "category_id" "category_id=?" [superCatIdParam] 
      checkRelationCats h conn catIdNum superCatIdNum
      lift $ updateInDb h conn "categories" "category_name=?,super_category_id=?" "category_id=?" [catNameParam,superCatIdParam,catIdParam]
      allSuperCats <- findAllSuperCats h conn catIdNum
      okHelper h $ inCatResp allSuperCats 
    ["deleteCategory"] -> do
      lift $ logInfo (hLog h) $ "Delete category command"
      adminAuthE h conn req
      let paramsNames = ["category_id"]
      [catIdParam] <- mapM (checkParam req) paramsNames
      [catIdNum]              <- mapM tryRead [catIdParam] 
      isExistInDbE h "categories" "category_id" "category_id=?" [catIdParam] 
      allSubCats <- findAllSubCats h conn catIdNum
      let values = fmap (pack . show) (defCatId:allSubCats)
      let where'  = intercalate " OR " . fmap (const "post_category_id=?")  $ allSubCats
      let where'' = intercalate " OR " . fmap (const "draft_category_id=?") $ allSubCats
      lift $ updateInDb h conn "posts"  "post_category_id=?"  where'  values
      lift $ updateInDb h conn "drafts" "draft_category_id=?" where'' values
      let where''' = intercalate " OR " . fmap (const "category_id=?") $ allSubCats
      lift $ deleteFromDb h conn "categories" where''' (fmap (pack . show) allSubCats)
      okHelper h $ OkResponse {ok = True}
    ["createTag"]  -> do
      lift $ logInfo (hLog h) $ "Create tag command"
      adminAuthE h conn req
      let paramsNames = ["tag_name"]
      [tagNameParam] <- mapM (checkParam req) paramsNames
      tagId <- lift $ insertReturnInDb h conn "tags" "tag_id" ["tag_name"] [tagNameParam] 
      okHelper h $ TagResponse tagId tagNameParam
    ["getTag",tagId]  -> do
      lift $ logInfo (hLog h) $ "Get tag command"
      tagIdNum <- tryRead tagId
      isExistInDbE h "tags" "tag_id" "tag_id=?" [tagId] 
      tagName <- selectFromDbE h conn "tags" ["tag_name"] "tag_id=?" [tagId] 
      okHelper h $ TagResponse tagIdNum tagName
    ["updateTag"]        -> do
      lift $ logInfo (hLog h) $ "Update tag command"
      adminAuthE h conn req
      let paramsNames = ["tag_id","tag_name"]
      [tagIdParam,tagNameParam] <- mapM (checkParam req) paramsNames
      [tagIdNum]                <- mapM tryRead [tagIdParam]
      isExistInDbE h "tags" "tag_id" "tag_id=?" [tagIdParam] 
      lift $ updateInDb h conn "tags" "tag_name=?" "tag_id=?" [tagNameParam,tagIdParam]
      okHelper h $ TagResponse tagIdNum tagNameParam
    ["deleteTag"]        -> do
      lift $ logInfo (hLog h) $ "Delete tag command"
      adminAuthE h conn req
      let paramsNames = ["tag_id"]
      [tagIdParam] <- mapM (checkParam req) paramsNames
      [tagIdNum]              <- mapM tryRead [tagIdParam] 
      lift $ deleteFromDb h conn "draftstags" "tag_id=?" [tagIdParam]
      lift $ deleteFromDb h conn "poststags" "tag_id=?" [tagIdParam]
      lift $ deleteFromDb h conn "tags" "tag_id=?" [tagIdParam]
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
      let tagsIds      = fmap tag_id3 . draft_tags_ids $ body
      let picsUrls     = fmap pic_url . draft_pics_urls $ body
      isExistInDbE h "users" "user_id" "user_id=?" [pack . show $ usIdParam] 
      pwd <- selectFromDbE h conn "users" ["password"] "user_id=?" [pack . show $ usIdParam] 
      userAuth pwdParam pwd
      isExistInDbE h "categories" "category_id" "category_id=?" [pack . show $ catIdParam] 
      mapM (isExistInDbE h "tags" "tag_id" "tag_id=?") $ fmap ( (:[]) . pack . show) tagsIds
      isUserAuthorE h conn usIdParam 
      (auId,auInfo) <- selectTupleFromDbE h conn "authors" ["author_id","author_info"] "user_id=?" [pack . show $ usIdParam] 
      picId <- getPicId h conn mPicUrlParam
      picsIds <- mapM (getPicId h conn) picsUrls
      let insNames  = ["author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      let insValues = [pack . show $ auId,nameParam,pack . show $ catIdParam,txtParam,pack . show $ picId]
      draftId <- lift $ insertReturnInDb h conn "drafts" "draft_id" insNames insValues          
      lift $ insertManyInDb h conn "draftspics" ["draft_id","pic_id"] (zip (repeat draftId) picsIds)
      lift $ insertManyInDb h conn "draftstags" ["draft_id","tag_id"] (zip (repeat draftId) tagsIds)
      let where' = intercalate " OR " . fmap (const "tag_id=?") $ tagsIds
      tagsMap <- selectTupleListFromDbE h conn "tags" ["tag_id","tag_name"] where' tagsIds 
      allSuperCats <- findAllSuperCats h conn catIdParam  
      okHelper h $ DraftResponse { draft_id2 = draftId, post_id2 = PostText "NULL" , author2 = AuthorResponse auId usIdParam auInfo, draft_name2 = nameParam , draft_cat2 =  inCatResp allSuperCats , draft_text2 = txtParam , draft_main_pic_id2 =  picId , draft_main_pic_url2 = makeMyPicUrl picId , draft_tags2 = fmap inTagResp tagsMap, draft_pics2 = fmap inPicIdUrl picsIds}
    ["createPostsDraft"]  -> do
      lift $ logInfo (hLog h) $ "Create post`s draft command"
      let paramsNames = ["post_id","user_id","password"]
      [postIdParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [postIdNum,usIdNum]              <- mapM tryRead [postIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      pwd <- selectFromDbE h conn "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isUserAuthorE h conn usIdNum 
      isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
      auId <- isPostAuthor h conn postIdParam usIdParam
      let table = "posts AS p JOIN authors AS a ON p.author_id=a.author_id"
      let params = ["author_info","post_name","post_category_id","post_text","post_main_pic_id"]
      (auInfo,postName,postCatId,postTxt,mPicId) <- selectTupleFromDbE h conn table params "post_id=?" [postIdParam]         
      picsIds <- selectListFromDbE h conn "postspics" ["pic_id"] "post_id=?" [postIdParam] 
      tagsMap <- selectTupleListFromDbE h conn "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?" [postIdParam] 
      allSuperCats <- findAllSuperCats h conn postCatId
      let insNames  = ["post_id","author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      let insValues = [postIdParam,pack . show $ auId,postName,pack . show $ postCatId,postTxt,pack . show $ mPicId]
      draftId <- lift $ insertReturnInDb h conn "drafts" "draft_id" insNames insValues 
      okHelper h $ DraftResponse {draft_id2 = draftId, post_id2 = PostInteger postIdNum, author2 = AuthorResponse auId usIdNum auInfo, draft_name2 = postName , draft_cat2 =  inCatResp allSuperCats, draft_text2 = postTxt, draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl mPicId , draft_tags2 = fmap inTagResp tagsMap, draft_pics2 = fmap inPicIdUrl picsIds}
    ["getDraft"]  -> do
      lift $ logInfo (hLog h) $ "Get draft command"
      let paramsNames = ["draft_id","user_id","password"]
      [draftIdParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [draftIdNum,usIdNum]              <- mapM tryRead [draftIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      pwd <- selectFromDbE h conn "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isUserAuthorE h conn usIdNum  
      auId <- isDraftAuthor h conn draftIdParam usIdParam
      let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
      let params = ["COALESCE (post_id, '0') AS post_id","author_info","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
      (postId,auInfo,draftName,draftCatId,draftTxt,mPicId) <- selectTupleFromDbE h conn table params "draft_id=?" [draftIdParam]        
      picsIds <- selectListFromDbE h conn "draftspics" ["pic_id"] "draft_id=?" [draftIdParam] 
      tagsMap <- selectTupleListFromDbE h conn "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam] 
      allSuperCats <- findAllSuperCats h conn draftCatId
      okHelper h $ DraftResponse { draft_id2 = draftIdNum, post_id2 = isNULL postId, author2 = AuthorResponse auId usIdNum auInfo, draft_name2 = draftName , draft_cat2 =  inCatResp allSuperCats, draft_text2 = draftTxt , draft_main_pic_id2 = mPicId, draft_main_pic_url2 = makeMyPicUrl mPicId, draft_tags2 = fmap inTagResp tagsMap, draft_pics2 = fmap inPicIdUrl picsIds}
    ["getDrafts"]  -> do
      lift $ logInfo (hLog h) $ "Get drafts command"
      let paramsNames = ["page","user_id","password"]
      [pageParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [pageNum,usIdNum]              <- mapM tryRead [pageParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      pwd <- selectFromDbE h conn "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isUserAuthorE h conn usIdNum  
      auId <- selectFromDbE h conn "authors" ["author_id"] "user_id=?" [usIdParam]  
      let table = "drafts JOIN authors ON authors.author_id = drafts.author_id"
      let orderBy = "draft_id DESC"
      let extractParams = ["draft_id","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id","author_info"]
      let where' = "drafts.author_id = ?"
      let values = [pack . show $ auId]
      params <- selectListLimitFromDbE h conn table orderBy pageNum draftNumberLimit extractParams where' values 
      let alldraftIdsText = fmap (pack . show . firstSeven) params
      let allCatIdsNum = fmap fourthSeven params
      manyAllSuperCats <- mapM (findAllSuperCats h conn) allCatIdsNum
      manyDraftPicsIds <- mapM (selectListFromDbE h conn "draftspics" ["pic_id"] "draft_id=?") $ fmap (:[]) alldraftIdsText  
      tagsMaps <- mapM (selectTupleListFromDbE h conn "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" ) $ fmap (:[]) alldraftIdsText
      let allParams = zip4 params manyAllSuperCats manyDraftPicsIds tagsMaps
      okHelper h $ DraftsResponse 
        { page9 = pageNum
        , drafts9 = fmap (\((draftId,postId,draftName,draftCat,draftText,draftMainPicId,auInfo),cats,pics,tagsMap) -> DraftResponse { draft_id2 = draftId, post_id2 = isNULL postId , author2 = AuthorResponse auId usIdNum auInfo, draft_name2 = draftName , draft_cat2 =  inCatResp cats, draft_text2 = draftText, draft_main_pic_id2 =  draftMainPicId, draft_main_pic_url2 = makeMyPicUrl draftMainPicId , draft_tags2 = fmap inTagResp tagsMap, draft_pics2 =  fmap inPicIdUrl pics}) allParams }
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
      let tagsIds      = fmap tag_id3 . draft_tags_ids $ body
      let picsUrls     = fmap pic_url . draft_pics_urls $ body
      isExistInDbE h "users" "user_id" "user_id=?" [pack . show $ usIdParam] 
      pwd <- selectFromDbE h conn "users" ["password"] "user_id=?" [pack . show $ usIdParam] 
      userAuth pwdParam pwd
      isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftId] 
      isExistInDbE h "categories" "category_id" "category_id=?" [pack . show $ catIdParam] 
      mapM (isExistInDbE h "tags" "tag_id" "tag_id=?" ) $ fmap ( (:[]) . pack . show) tagsIds
      isUserAuthorE h conn usIdParam  
      (auId,auInfo) <- selectTupleFromDbE h conn "authors" ["author_id","author_info"] "user_id=?" [pack . show $ usIdParam] 
      postId <- selectFromDbE h conn "drafts" ["COALESCE (post_id, '0') AS post_id"] "draft_id=?" [draftId] 
      picId <- getPicId h conn mPicUrlParam
      picsIds <- mapM (getPicId h conn) picsUrls
      deleteDraftsPicsTags h conn [draftIdNum]
      lift $ updateInDb h conn "drafts" "draft_name=?,draft_category_id=?,draft_text=?,draft_main_pic_id=?" "draft_id=?" [nameParam,pack . show $ catIdParam,txtParam,pack . show $ picId,draftId]
      lift $ insertManyInDb h conn "draftspics" ["draft_id","pic_id"] (zip (repeat draftIdNum) picsIds)
      lift $ insertManyInDb h conn "draftstags" ["draft_id","tag_id"] (zip (repeat draftIdNum) tagsIds)
      let where' = intercalate " OR " . fmap (const "tag_id=?") $ tagsIds
      tagsMap <- selectTupleListFromDbE h conn "tags" ["tag_id","tag_name"] where' tagsIds 
      allSuperCats <- findAllSuperCats h conn catIdParam  
      okHelper h $ DraftResponse {draft_id2 = draftIdNum, post_id2 = isNULL postId, author2 = AuthorResponse auId usIdParam auInfo, draft_name2 = nameParam, draft_cat2 =  inCatResp allSuperCats, draft_text2 = txtParam, draft_main_pic_id2 =  picId, draft_main_pic_url2 = makeMyPicUrl picId, draft_tags2 = fmap inTagResp tagsMap, draft_pics2 = fmap inPicIdUrl picsIds}
    ["deleteDraft"]  -> do
      lift $ logInfo (hLog h) $ "Delete draft command"
      let paramsNames = ["draft_id","user_id","password"]
      [draftIdParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [draftIdNum,usIdNum]              <- mapM tryRead [draftIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      pwd <- selectFromDbE h conn "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftIdParam] 
      isUserAuthorE h conn usIdNum  
      isDraftAuthor h conn draftIdParam usIdParam
      deleteAllAboutDrafts h conn [draftIdNum]
      okHelper h $ OkResponse { ok = True }
    ["publishDraft"]  -> do
      lift $ logInfo (hLog h) $ "Publish draft command"
      let paramsNames = ["draft_id","user_id","password"]
      [draftIdParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [draftIdNum,usIdNum]              <- mapM tryRead [draftIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      pwd <- selectFromDbE h conn "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isExistInDbE h "drafts" "draft_id" "draft_id=?" [draftIdParam] 
      isUserAuthorE h conn usIdNum  
      auId <- isDraftAuthor h conn draftIdParam usIdParam
      draftPostId <- selectFromDbE h conn "drafts" ["COALESCE (post_id, '0') AS post_id"] "draft_id=?" [draftIdParam] 
      case draftPostId of
        0 -> do
          let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
          let params = ["author_info","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
          (auInfo,draftName,draftCatId,draftTxt,mPicId) <- selectTupleFromDbE h conn table params "draft_id=?" [draftIdParam]         
          picsIds <- selectListFromDbE h conn "draftspics" ["pic_id"] "draft_id=?" [draftIdParam] 
          tagsMap <- selectTupleListFromDbE h conn "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam] 
          day <- lift $ getDay h 
          let insNames  = ["author_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
          let insValues = [pack . show $ auId,draftName,pack day,pack . show $ draftCatId,draftTxt,pack . show $ mPicId]          
          postId <- lift $ insertReturnInDb h conn "posts" "post_id" insNames insValues          
          lift $ insertManyInDb h conn "postspics" ["post_id","pic_id"] (zip (repeat postId) picsIds)
          lift $ insertManyInDb h conn "poststags" ["post_id","tag_id"] (zip (repeat postId) (fmap fst tagsMap))
          allSuperCats <- findAllSuperCats h conn draftCatId 
          okHelper h $ PostResponse {post_id = postId, author4 = AuthorResponse auId usIdNum auInfo, post_name = draftName , post_create_date = pack day, post_cat = inCatResp allSuperCats, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = makeMyPicUrl mPicId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagsMap}
        _ -> do
          let table = "drafts AS d JOIN authors AS a ON d.author_id=a.author_id"
          let params = ["author_info","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
          (auInfo,draftName,draftCatId,draftTxt,mPicId) <- selectTupleFromDbE h conn table params "draft_id=?" [draftIdParam]         
          picsIds <- selectListFromDbE h conn "draftspics" ["pic_id"] "draft_id=?" [draftIdParam] 
          tagsMap <- selectTupleListFromDbE h conn "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "draft_id=?" [draftIdParam] 
          lift $ updateInDb h conn "posts" "post_name=?,post_category_id=?,post_text=?,post_main_pic_id=?" "post_id=?" [draftName,pack . show $ draftCatId,draftTxt,pack . show $ mPicId,pack . show $ draftPostId]
          deletePostsPicsTags h conn [draftPostId]
          lift $ insertManyInDb h conn "postspics" ["post_id","pic_id"] (zip (repeat draftPostId) picsIds)
          lift $ insertManyInDb h conn "poststags" ["post_id","tag_id"] (zip (repeat draftPostId) (fmap fst tagsMap))
          allSuperCats <- findAllSuperCats h conn draftCatId
          day <- selectFromDbE h conn "posts" ["post_create_date"] "post_id=?" [pack . show $ draftPostId]    
          okHelper h $ PostResponse {post_id = draftPostId, author4 = AuthorResponse auId usIdNum auInfo, post_name = draftName , post_create_date = pack . showGregorian $ day, post_cat = inCatResp allSuperCats, post_text = draftTxt, post_main_pic_id = mPicId, post_main_pic_url = makeMyPicUrl mPicId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagsMap}
    ["getPost",postId]  -> do
      lift $ logInfo (hLog h) $ "Get post command"
      postIdNum <- tryRead postId
      isExistInDbE h "users" "user_id" "user_id=?" [postId] 
      (auId,usId,auInfo,pName,pDate,pCatId,pText,picId) <- selectTupleFromDbE h conn "posts JOIN authors ON authors.author_id = posts.author_id " ["posts.author_id","user_id","author_info","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"] "post_id=?" [postId] 
      picsIds <- selectListFromDbE h conn "postspics" ["pic_id"] "post_id=?" [postId] 
      tagsMap <- selectTupleListFromDbE h conn "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?" [postId] 
      allSuperCats <- findAllSuperCats h conn pCatId
      okHelper h $ PostResponse {post_id = postIdNum, author4 = AuthorResponse auId usId auInfo, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = inCatResp allSuperCats, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagsMap}
    ["getPosts", page] -> do
      lift $ logInfo (hLog h) $ "Get posts command"
      pageNum <- tryRead page
      let extractParamsList = ["posts.post_id","posts.author_id","authors.user_id","author_info","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
      (table,where',orderBy,values) <- chooseArgs req       
      params <- selectListLimitFromDbE h conn table orderBy pageNum postNumberLimit extractParamsList where' values 
      let postIdsText = fmap (pack . show . firstNine) params
      let postCatsIds = fmap seventhNine params 
      manySuperCats <- mapM (findAllSuperCats h conn) postCatsIds
      manyPostPicsIds <- mapM (selectListFromDbE h conn "postspics" ["pic_id"] "post_id=?") $ fmap (:[]) postIdsText  
      tagsMaps <- mapM (selectTupleListFromDbE h conn "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?") $ fmap (:[]) postIdsText  
      let allParams = zip4 params manySuperCats manyPostPicsIds tagsMaps
      okHelper h $ PostsResponse {page10 = pageNum , posts10 = fmap (\((pId,auId,usId,auInfo,pName,pDate,pCat,pText,picId),cats,pics,tagsMap) -> PostResponse {post_id = pId, author4 = AuthorResponse auId usId auInfo, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = inCatResp cats, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl pics, post_tags = fmap inTagResp tagsMap}) allParams}
    ["deletePost"]  -> do
      lift $ logInfo (hLog h) $ "Delete post command"
      adminAuthE h conn req
      let paramsNames = ["post_id"]
      [postIdParam] <- mapM (checkParam req) paramsNames
      [postIdNum]   <- mapM tryRead [postIdParam] 
      deleteAllAboutPosts h conn [postIdNum]
      okHelper h $ OkResponse { ok = True }
    ["createComment"]  -> do
      lift $ logInfo (hLog h) $ "Create comment command"
      let paramsNames = ["post_id","comment_text","user_id","password"]
      [postIdParam,txtParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [postIdNum,usIdNum]                       <- mapM tryRead [postIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      pwd <- selectFromDbE h conn "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
      commId <- lift $ insertReturnInDb h conn "comments" "comment_id" ["comment_text","post_id","user_id"] [txtParam,postIdParam,usIdParam] 
      okHelper h $ CommentResponse {comment_id = commId, comment_text = txtParam, post_id6 = postIdNum, user_id6 = usIdNum}
    ["getComments"] -> do
      lift $ logInfo (hLog h) $ "Get comments command"
      let paramsNames = ["post_id","page"]
      [postIdParam,pageParam] <- mapM (checkParam req) paramsNames
      [postIdNum,pageNum]     <- mapM tryRead [postIdParam,pageParam] 
      isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
      comms <- selectListLimitFromDbE h conn "comments" "comment_id DESC" pageNum commentNumberLimit ["comment_id","comment_text","user_id"] "post_id=?" [postIdParam]
      okHelper h $ CommentsResponse {page = pageNum, post_id9 = postIdNum, comments = fmap inCommResp comms}
    ["updateComment"]  -> do
      lift $ logInfo (hLog h) $ "Update comment command"
      let paramsNames = ["comment_id","comment_text","user_id","password"]
      [commIdParam,txtParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
      [commIdNum,usIdNum]                       <- mapM tryRead [commIdParam,usIdParam] 
      isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
      pwd <- selectFromDbE h conn "users" ["password"] "user_id=?" [usIdParam] 
      userAuth pwdParam pwd
      isCommAuthor h conn commIdParam usIdParam
      lift $ updateInDb h conn "comments" "comment_text=?" "comment_id=?" [txtParam,commIdParam]
      postId <- selectFromDbE h conn "comments" ["post_id"] "comment_id=?" [commIdParam] 
      okHelper h $ CommentResponse {comment_id = commIdNum, comment_text = txtParam, post_id6 = postId, user_id6 = usIdNum}
    ["deleteComment"]  -> do
      lift $ logInfo (hLog h) $ "Delete comment command" 
      case accessMode req of
        AdminMode -> do
          adminAuthE h conn req
          let paramsNames = ["comment_id"]
          [commIdParam] <- mapM (checkParam req) paramsNames
          [commIdNum]   <- mapM tryRead [commIdParam]
          lift $ deleteFromDb h conn "comments" "comment_id=?" [commIdParam]
          okHelper h $ OkResponse { ok = True }
        UserMode -> do
          let paramsNames = ["comment_id","user_id","password"]
          [commIdParam,usIdParam,pwdParam] <- mapM (checkParam req) paramsNames
          [commIdNum,usIdNum]              <- mapM tryRead [commIdParam,usIdParam]
          isExistInDbE h "users" "user_id" "user_id=?" [usIdParam] 
          pwd <- selectFromDbE h conn "users" ["password"] "user_id=?" [usIdParam] 
          userAuth pwdParam pwd
          postId <- selectFromDbE h conn "comments" ["post_id"] "comment_id=?" [commIdParam]  
          isCommOrPostAuthor h conn commIdParam (pack . show $ (postId :: Integer)) usIdParam
          lift $ deleteFromDb h conn "comments" "comment_id=?" [commIdParam]
          okHelper h $ OkResponse {ok = True}      
    ["picture",picId]  -> do
      lift $ logInfo (hLog h) $ "Picture command"
      picIdNum <- tryRead picId 
      isExistInDbE h "pics" "pic_id" "pic_id=?" [picId] 
      picUrl <- selectFromDbE h conn "pics" ["pic_url"] "pic_id=?" [picId] 
      res <- getPicFromUrlE h picUrl
      lift $ logInfo (hLog h) $ "Sending picture"
      return $ responseBuilder 
        status200 
        [("Content-Type", "image/jpeg")] 
        $ lazyByteString $ HT.getResponseBody res
    ["test",usId] -> do
      lift $ logInfo (hLog h) $ "Test command"
      usIdNum <- tryRead usId
      isExistInDbE h "users" "user_id" "user_id=?" [usId]
      okHelper h $ OkResponse {ok = True} 
      


getPicFromUrlE h picUrl = do
  lift $ logInfo (hLog h) $ "Getting picture from internet. Url:" ++ unpack picUrl
  case isMyUrl picUrl of
    True -> throwE $ SimpleError $ "Invalid url"
    _    -> do
      res <- checkPicUrlGetPic h picUrl
      return res


isCommOrPostAuthor h conn commIdParam postIdParam usIdParam = do
  isCommAuthor h conn commIdParam usIdParam
    `catchE`
      (\(SimpleError str) -> 
        withExceptT 
          (\(SimpleError str') -> SimpleError $ str' ++ " AND " ++ str) $ do
            isPostAuthor h conn postIdParam usIdParam
            return ())

accessMode req = case fmap (isExistParam req) ["user_id","admin_id"] of
  [True,_] -> UserMode
  [_,True] -> AdminMode
  _        -> UserMode

data AccessMode = UserMode | AdminMode

isCommAuthor h conn commIdParam usIdParam = do
  usId <- selectFromDbE h conn "comments" ["user_id"] "comment_id=?" [commIdParam]  
  case (usId :: Integer) == (read . unpack $ usIdParam) of
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is not author of comment_id: " ++ unpack commIdParam

hideErr m = m `catchE` (\e -> throwE $ toSecret e)

toSecret (SimpleError str) = SecretError str
toSecret (SecretError str) = SecretError str

inCommResp (a,b,c) = CommentIdTextUserResponse a b c     

isNULL 0      = PostText    "NULL" 
isNULL postId = PostInteger postId

isDraftAuthor h conn draftIdParam usIdParam = do
  auId <- selectFromDbE h conn "drafts" ["author_id"] "draft_id=?" [draftIdParam] 
  usDraftId <- selectFromDbE h conn "authors" ["user_id"] "author_id=?" [pack . show $ (auId :: Integer)]  
  case (usDraftId :: Integer) == (read . unpack $ usIdParam) of
    True -> return auId
    False -> throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is not author of draft_id: " ++ unpack draftIdParam

isPostAuthor h conn postIdParam usIdParam = do
  auId <- selectFromDbE h conn "posts" ["author_id"] "post_id=?" [postIdParam] 
  usPostId <- selectFromDbE h conn "authors" ["user_id"] "author_id=?" [pack . show $ (auId :: Integer)]  
  case (usPostId :: Integer) == (read . unpack $ usIdParam) of
    True -> return auId
    False -> throwE $ SimpleError $ "user_id: " ++ unpack usIdParam ++ " is not author of post_id: " ++ unpack postIdParam

inTagResp (tagId,tagName) = TagResponse tagId tagName

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
        checkTagArrVal tagsIdsVal
        checkPicArrVal picsUrlsVal
        throwE $ SimpleError $ "Can`t parse request body"
      Nothing -> throwE $ SimpleError $ "Invalid request body"

checkTagArrVal val = do
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

checkRelationCats h conn catIdNum superCatIdNum 
  |catIdNum == superCatIdNum = throwE $ SimpleError $ "super_category_id: " ++ show superCatIdNum ++ " equal to category_id."
  |otherwise                 = do
    allSubCats <- findAllSubCats h conn catIdNum
    if superCatIdNum `elem` allSubCats
      then throwE $ SimpleError $ "super_category_id: " ++ show superCatIdNum ++ " is subCategory of category_id: " ++ show catIdNum
      else return ()

findAllSubCats :: (Monad m, MonadCatch m,MonadFail m) => Handle m -> Connection -> Integer -> ExceptT ReqError m [Integer]
findAllSubCats h conn catId = do
  check <- lift $ isExistInDb h "categories" "category_id" "super_category_id=?" [pack . show $ catId] 
  case check of
    False -> return [catId]
    True  -> do
      xs <- selectListFromDbE h conn "categories" ["category_id"] "super_category_id=?" [pack . show $ catId] 
      ys <- mapM (findAllSubCats h conn) xs
      return $ catId : (Prelude.concat  ys)   
    
findAllSuperCats :: (Monad m, MonadCatch m) => Handle m -> Connection -> Integer -> ExceptT ReqError m [(Integer,Text)]
findAllSuperCats h conn catId = do
  (catName,superCatId) <- selectTupleFromDbE h conn "categories" ["category_name","COALESCE (super_category_id, '0') AS super_category_id"] "category_id=?" [pack . show $ catId] 
  case superCatId of 
    0 -> return $ [(catId,catName)]
    _ -> do
      xs <- findAllSuperCats h conn superCatId
      return $ ((catId,catName) : xs) 

deleteAllAboutDrafts :: (Monad m) => Handle m -> Connection -> [Integer] -> ExceptT ReqError m ()
deleteAllAboutDrafts h conn [] = return ()
deleteAllAboutDrafts h conn draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  deleteDraftsPicsTags h conn draftsIds
  lift $ deleteFromDb h conn "drafts" where' values
  return ()

deleteDraftsPicsTags :: (Monad m) => Handle m -> Connection -> [Integer] -> ExceptT ReqError m ()
deleteDraftsPicsTags h conn [] = return ()
deleteDraftsPicsTags h conn draftsIds = do
  let values = fmap (pack . show) draftsIds
  let where' = intercalate " OR " . fmap (const "draft_id=?") $ draftsIds
  lift $ deleteFromDb h conn "draftspics" where' values
  lift $ deleteFromDb h conn "draftstags" where' values
  return ()

deleteAllAboutPosts :: (Monad m, MonadCatch m) => Handle m -> Connection -> [Integer] -> ExceptT ReqError m ()
deleteAllAboutPosts h conn [] = return ()
deleteAllAboutPosts h conn postsIds = do
  let values = fmap (pack . show) postsIds
  let where' = intercalate " OR " . fmap (const "post_id=?") $ postsIds
  deletePostsPicsTags h conn postsIds
  lift $ deleteFromDb h conn "comments" where' values
  draftIds <- selectListFromDbE h conn "drafts" ["draft_id"] where' values  
  deleteAllAboutDrafts h conn draftIds
  lift $ deleteFromDb h conn "drafts" where' values
  lift $ deleteFromDb h conn "posts" where' values
  return ()

deletePostsPicsTags :: (Monad m) => Handle m -> Connection -> [Integer] -> ExceptT ReqError m ()
deletePostsPicsTags h conn [] = return ()
deletePostsPicsTags h conn postsIds = do
  let values = fmap (pack . show) postsIds
  let where' = intercalate " OR " . fmap (const "post_id=?") $ postsIds
  lift $ deleteFromDb h conn "postspics" where' values
  lift $ deleteFromDb h conn "poststags" where' values
  return ()

isUserAuthorBool h conn usIdParam = do
  lift $ logDebug (hLog h) $ "Checking in DB is user author"  
  lift $ isExistInDb h "authors" "user_id" "user_id=?" [pack . show $ usIdParam]

isUserAuthorE h conn usIdParam = do
  lift $ logDebug (hLog h) $ "Checking in DB is user author"  
  check <- lift $ isExistInDb h "authors" "user_id" "user_id=?" [pack . show $ usIdParam]
  case check of 
    True -> return ()
    False -> throwE $ SimpleError $ "user_id: " ++ show usIdParam ++ " isn`t author."
    

checkRelationUsAu h conn usIdParam auIdParam = do
  check <- lift $ isExistInDb h "authors" "user_id" "user_id=?" [usIdParam] 
  case check of
    True -> do
      auId <- selectFromDbE h conn "authors" ["author_id"] "user_id=?" [usIdParam] 
      case (auId :: Integer) == (read . unpack $ auIdParam) of
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

selectFromDb :: (Monad m, MonadCatch m, ToRow q, FromRow r) => Handle m -> Connection -> Query -> q -> m [r]
selectFromDb h conn q values = do
  xs <- dbQuery h conn q values
  return xs

selectFromDbE :: (Monad m, MonadCatch m, ToRow q, FF.FromField a) => Handle m -> Connection -> String -> [String] -> String -> q -> ExceptT ReqError m a
selectFromDbE h conn table params where' values = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table 
  xs <- lift $ selectFromDb h conn (toSelQ table params where') values
  case xs of
    []           -> throwE $ SimpleError "DatabaseError.Empty output"
    [Only value] -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return value
    _            -> throwE $ SimpleError "DatabaseError"

selectListFromDbE :: (Monad m, MonadCatch m, ToRow q, FF.FromField a) => Handle m -> Connection -> String -> [String] -> String -> q -> ExceptT ReqError m [a]
selectListFromDbE h conn table params where' values = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table
  xs <- lift $ selectFromDb h conn (toSelQ table params where') values 
  case xs of
    []                -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return []
    ((Only value):ys) -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return . fmap fromOnly $ xs


selectTupleFromDbE h conn table params where' values = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table
  xs <- lift $ selectFromDb h conn (toSelQ table params where') values
  case xs of
    []      -> throwE $ SimpleError "DatabaseError.Empty output"
    [tuple] -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return tuple
    _       -> throwE $ SimpleError "DatabaseError"

selectTupleListFromDbE h conn table params where' values = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table
  xs <- lift $ selectFromDb h conn (toSelQ table params where') values 
  case xs of
    []             -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return []
    (tuple:tuples) -> do
      lift $ logInfo (hLog h) $ "Data received from DB"
      return xs

--selectListLimitFromDbE :: (Monad m, MonadCatch m, ToRow q, FromRow r) => Handle m -> Connection -> Query -> q -> m [r]
selectListLimitFromDbE h conn table orderBy page limitNumber params where' values = do
  lift $ logDebug (hLog h) $ "Select data from DB. Table: " ++ table
  xs <- lift $ selectFromDb h conn (toSelLimQ table orderBy page limitNumber params where') values
  lift $ logInfo (hLog h) $ "Data received from DB"
  return xs

updateInDb h conn table set where' values = do
  (dbExecute h) conn (toUpdQ table set where') values

deleteFromDb h conn table where' values = do
  (dbExecute h) conn (toDelQ table where') values   

{-
isExistInDb :: (Monad m, MonadCatch m, MonadFail m) => Handle m -> Connection -> String -> String -> String -> [Text] -> m Bool
isExistInDb h conn table checkName where' values = do
  [Only check]  <- dbQuery h conn (toExQ table checkName where') values
  return check
-}

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

insertReturnInDb :: forall m a. (Monad m, MonadCatch m, MonadFail m, FF.FromField a) => Handle m -> Connection -> String -> String -> [String] -> [Text] -> m a
insertReturnInDb h conn table returnName insNames insValues = do
  [Only x] <- dbQuery h conn ( toInsRetQ table returnName insNames ) insValues
  return x

insertManyInDb h conn table insNames insValues = do
  (dbExecuteMany h) conn ( toInsManyQ table insNames ) insValues






isMyUrl url
  | isPrefixOf "http://localhost:3000" url = True
  | otherwise                              = False

picUrlEnd url = stripPrefix "http://localhost:3000/picture/" url

checkMyPicUrl :: (Monad m) =>  Connection -> Text -> ExceptT ReqError m Text
checkMyPicUrl conn url = do
  case picUrlEnd url of
    Just ""     -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url
    Just urlEnd -> return urlEnd
    Nothing     -> throwE $ SimpleError $ "Invalid picture url:" ++ unpack url 

readUrlEnd :: (Monad m,MonadCatch m,MonadFail m) => Handle m -> Connection -> Text -> Text -> ExceptT ReqError m Integer
readUrlEnd h conn url urlEnd = do
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


getPicId :: (Monad m,MonadCatch m,MonadFail m) => Handle m -> Connection -> Text -> ExceptT ReqError m Integer
getPicId h conn url 
  |isMyUrl url = do
    picId <- (checkMyPicUrl conn url >>= \urlEnd -> readUrlEnd h conn url urlEnd)
    return picId
  |otherwise = do
    checkPicUrlGetPic h url
    picId    <- lift $ insertReturnInDb h conn "pics" "pic_id" ["pic_url"] [url] 
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

data ReqError = SecretError String | SimpleError String
  deriving Show

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

adminAuthE h conn req = do
  let authParams  = ["admin_id","password"]
  [admIdParam,pwdParam] <- hideErr $ mapM (checkParam req) authParams
  [admIdNum]            <- hideErr $ mapM tryRead [admIdParam]
  lift $ logInfo (hLog h) $ "All authorize parameters parsed"
  hideErr $ isExistInDbE h "users" "user_id" "user_id=?" [admIdParam] 
  (pwd,admBool) <- hideErr $ selectTupleFromDbE h conn "users" ["password","admin"] "user_id=?" [admIdParam] 
  hideErr $ adminAuth pwdParam pwd admBool

adminAuth pwdParam pwd admBool
  | admBool && (pwd == pwdParam) = return True
  | admBool                      = throwE . SimpleError $ "INVALID pwd, admin = True "
  | (pwd == pwdParam)            = throwE . SimpleError $ "valid pwd, user is NOT admin"
  | otherwise                    = throwE . SimpleError $ "INVALID pwd, user is NOT admin"

userAuth pwdParam pwd 
  | pwd == pwdParam = return ()
  | otherwise       = throwE . SimpleError $ "INVALID password"


inCatResp [(x,y)] = CatResponse { cat_id = x , cat_name =  y , super_cat = "NULL"}
inCatResp ((x,y):xs) = SubCatResponse { subCat_id = x , subCat_name =  y , super_category = inCatResp xs} 
      





firstThree (a,b,c) = a
firstFour  (a,b,c,d) = a
firstFive  (a,b,c,d,e) = a
firstSix   (a,b,c,d,e,f) = a
firstSeven (a,b,c,d,e,f,g) = a
firstEight (a,b,c,d,e,f,g,h) = a
firstNine  (a,b,c,d,e,f,g,h,i) = a
firstTen   (a,b,c,d,e,f,g,h,i,j) = a

secondThree (a,b,c) = b
secondFour  (a,b,c,d) = b
secondFive  (a,b,c,d,e) = b
secondSix   (a,b,c,d,e,f) = b
secondSeven (a,b,c,d,e,f,g) = b
secondEight (a,b,c,d,e,f,g,h) = b
secondNine  (a,b,c,d,e,f,g,h,i) = b
secondTen   (a,b,c,d,e,f,g,h,i,j) = b

thirdThree (a,b,c) = c
thirdFour  (a,b,c,d) = c
thirdFive  (a,b,c,d,e) = c
thirdSix   (a,b,c,d,e,f) = c
thirdSeven (a,b,c,d,e,f,g) = c
thirdEight (a,b,c,d,e,f,g,h) = c
thirdNine  (a,b,c,d,e,f,g,h,i) = c
thirdTen   (a,b,c,d,e,f,g,h,i,j) = c

fourthFour  (a,b,c,d) = d
fourthFive  (a,b,c,d,e) = d
fourthSix   (a,b,c,d,e,f) = d
fourthSeven (a,b,c,d,e,f,g) = d
fourthEight (a,b,c,d,e,f,g,h) = d
fourthNine  (a,b,c,d,e,f,g,h,i) = d
fourthTen   (a,b,c,d,e,f,g,h,i,j) = d

fifthFive  (a,b,c,d,e) = e
fifthSix   (a,b,c,d,e,f) = e
fifthSeven (a,b,c,d,e,f,g) = e
fifthEight (a,b,c,d,e,f,g,h) = e
fifthNine  (a,b,c,d,e,f,g,h,i) = e
fifthTen   (a,b,c,d,e,f,g,h,i,j) = e

sixthSix   (a,b,c,d,e,f) = f
sixthSeven (a,b,c,d,e,f,g) = f
sixthEight (a,b,c,d,e,f,g,h) = f
sixthNine  (a,b,c,d,e,f,g,h,i) = f
sixthTen   (a,b,c,d,e,f,g,h,i,j) = f

seventhSeven (a,b,c,d,e,f,g) = g
seventhEight (a,b,c,d,e,f,g,h) = g
seventhNine  (a,b,c,d,e,f,g,h,i) = g
seventhTen   (a,b,c,d,e,f,g,h,i,j) = g

eighthEight (a,b,c,d,e,f,g,h) = h
eighthNine  (a,b,c,d,e,f,g,h,i) = h
eighthTen   (a,b,c,d,e,f,g,h,i,j) = h

ninthNine  (a,b,c,d,e,f,g,h,i) = i
ninthTen   (a,b,c,d,e,f,g,h,i,j) = i


{-fee :: Integer -> Integer -> ExceptT String IO Integer
fee a b 
  |a <= b = return a
  | True  = throwE $ "a > b"

lee :: Integer -> Integer -> ExceptT String IO Integer
lee d c
  | d + c > 100 = return d
  | True        = throwE $ "d+c < 100"-}

