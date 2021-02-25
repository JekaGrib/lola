{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ScopedTypeVariables #-}


import           Test.Hspec
import           Control.Monad.State            


import           App
import           Api
import           Logger
import           Network.Wai
import           Network.HTTP.Types             ( status200, status404, status301, movedPermanently301, http11 )
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

data MockAction = EXISTCHEK | LOGMSG | INSERTDATA
  deriving (Eq,Show)

data TestDB = TestDB 
  { picsT :: PicsT,
    keyT  :: KeyT,
    usersT :: UsersT,
    authorsT :: AuthorsT,
    tagsT :: TagsT,
    categoriesT :: CategoriesT,
    postsT :: PostsT,
    commentsT :: CommentsT,
    postsPicsT :: PostsPicsT,
    postsTagsT :: PostsTagsT,
    draftsT :: DraftsT,
    draftsPicsT :: DraftsPicsT,
    draftTagsT :: DraftsTagsT
  }


type PicsT = [PicsL]
data PicsL = PicsL { pic_idPicsV ::  Integer, pic_urlV :: Text }
type KeyT  = [Text]
type UsersT = [UsersL]
data UsersL = UsersL { user_idUsersV :: Integer, passwordV :: Text, first_nameV :: Text, last_nameV :: Text, user_pic_idV :: Integer, user_create_dateV :: Day, admin :: Bool }
type AuthorsT = [(Integer,Text,Integer)]
type TagsT = [(Integer,Text)]
type CategoriesT = [(Integer,Text,Maybe Integer)]
type PostsT = [(Integer,Integer,Text,Day,Integer,Text,Integer)]
type CommentsT = [(Integer,Text,Integer,Integer)]
type PostsPicsT = [(Integer,Integer)]
type PostsTagsT = [(Integer,Integer)]
type DraftsT = [(Integer,Integer,Integer,Text,Integer,Text,Integer)]
type DraftsPicsT = [(Integer,Integer)]
type DraftsTagsT = [(Integer,Integer)]

logTest :: Priority -> String -> StateT (TestDB,[MockAction]) IO ()
logTest prio text = StateT $ \(db,acts) -> 
  return (() , (db,LOGMSG : acts))

emptyDB = TestDB [] [] [] [] [] [] [] [] [] [] [] [] []

picsL1 = PicsL 1 "https://www.publicdomainpictures.net/pictures/20000/t2/parapente.jpg"
picsL2 = PicsL 2 "https://www.stockvault.net/data/2017/05/20/236363/thumb16.jpg"
picsL3 = PicsL 3 "https://cdn.pixabay.com/photo/2016/07/22/18/39/daisy-1535532__340.jpg"
picsL4 = PicsL 4 "https://img.freepik.com/free-photo/poster-with-vertical-frames-on-empty-white-wall-in-living-room-interior-with-blue-velvet-armchair-3d-rendering_41470-2907.jpg?size=626&ext=jpg&ga=GA1.2.42293934.1613723092"
picsL5 = PicsL 5 "https://skitterphoto.com/photos/skitterphoto-9649-thumbnail.jpg"

picsT1 = [picsL1,picsL2,picsL3,picsL4,picsL5]

usersL1 = UsersL 1 "12345678" "DELETED" "DELETED" 1 (fromGregorian 2018 01 01) True
usersL2 = UsersL 2 "87654321" "Lidia" "Klimova"   2 (fromGregorian 2018 02 01) False
usersL3 = UsersL 3 "kukui"    "Ira" "Medvedeva"   2 (fromGregorian 2018 03 01) False
usersL4 = UsersL 4 "1234dom"  "Lisa" "Karimova"   3 (fromGregorian 2018 04 01) True
usersL5 = UsersL 5 "335jsu"   "Anton" "Petrov"    4 (fromGregorian 2018 05 01) False

usersT1 = [usersL1,usersL2,usersL3,usersL4,usersL5]

readGregorian x@(a:b:c:d:'-':e:f:'-':g:h:[]) = fromGregorian (yearG x) (monG x) (dayG x)
dayG (a:b:c:d:'-':e:f:'-':g:h:[]) = read (g:h:[])
monG (a:b:c:d:'-':e:f:'-':g:h:[]) = read (e:f:[])
yearG (a:b:c:d:'-':e:f:'-':g:h:[]) = read (a:b:c:d:[])

testDB1 = emptyDB {picsT = picsT1,usersT = usersT1}

getDayTest = return "2020-02-20"

isExistInDbTest :: String -> String -> String -> [Text] -> StateT (TestDB,[MockAction]) IO Bool
isExistInDbTest s s' s'' xs = StateT $ \(db,acts) -> do
  return ( foo s s' s'' xs db , (db,EXISTCHEK:acts))

foo "pics" s' s'' xs db = moo s' s'' xs (picsT db)
moo "pic_id" s'' xs pics = loo s'' xs (fmap pic_idPicsV pics)
loo "pic_id=?" [x] picsIds = elem (read . unpack $ x) picsIds

insertReturnInDbTest :: String -> String -> [String] -> [Text] -> StateT (TestDB,[MockAction]) IO [(Only Integer)]
insertReturnInDbTest s s' xs ys = StateT $ \(db,acts) -> do
  return ( snd . soo s s' xs ys $ db , (fst . soo s s' xs ys $ db,INSERTDATA:acts))

soo :: String -> String -> [String] -> [Text] -> TestDB -> (TestDB, [(Only Integer)])
soo "pics" s' xs ys db = poo s' xs ys db
soo "users" s' xs ys db = woo s' xs ys db
--soo ".." s' xs ys db = (emptyDB, [(5 :: Integer,7 :: Double)])

poo :: String -> [String] -> [Text] -> TestDB -> (TestDB, [(Only Integer)]) 
poo "pic_id" ["pic_url"] [url] db =
  let pT = picsT db in
    case pT of
      [] -> (db {picsT = [ PicsL 1 url ]}, [(Only 1)])
      _  -> let num = (pic_idPicsV . last $ pT) in (db {picsT = pT ++ [ PicsL num url ]}, [(Only num)])
 
woo :: FromRow r => String -> [String] -> [Text] -> TestDB -> (TestDB, [R r])
woo "user_id" ["password","first_name","last_name","user_pic_id","user_create_date","admin"] [pwd,fN,lN,pI,cD,aD] db = 
    let uT = usersT db in
    case uT of
      [] -> ( db {usersT = [ UsersL 1 pwd fN lN (read . unpack $ pI) (readGregorian . unpack $ cD) (read . unpack $ aD) ]}, [(Only 1)])
      _  -> let num = (user_idUsersV . last $ uT) 
            in ( db {usersT = uT ++ [ UsersL num pwd fN lN (read . unpack $ pI) (readGregorian . unpack $ cD) (read . unpack $ aD) ]}, [(Only num)] )
    

handleLog1 = LogHandle (LogConfig DEBUG) logTest
handle1 = Handle 
  { hLog = handleLog1,
    isExistInDb = isExistInDbTest,
    insertReturnInDb = insertReturnInDbTest,
    getDay = getDayTest,
    httpAction = HT.httpLBS
    }

reqTest0 = defaultRequest {requestMethod = "GET", httpVersion = http11, rawPathInfo = "/test/3", rawQueryString = "", requestHeaders = [("User-Agent","PostmanRuntime/7.26.8"),("Accept","*/*"),("Postman-Token","6189d61d-fa65-4fb6-a578-c4061535e7ef"),("Host","localhost:3000"),("Accept-Encoding","gzip, deflate, br"),("Connection","keep-alive"),("Content-Type","multipart/form-data; boundary=--------------------------595887703656508108682668"),("Content-Length","170")], isSecure = False, pathInfo = ["test","3"], queryString = [], requestBodyLength = KnownLength 170, requestHeaderHost = Just "localhost:3000", requestHeaderRange = Nothing}
reqTest1 = defaultRequest {requestMethod = "GET", httpVersion = http11, rawPathInfo = "/createUser", rawQueryString = "?password=654321&first_name=Kate&last_name=Grick&user_pic_url=https://images.pexels.com/photos/4617160/pexels-photo-4617160.jpeg?auto=compress%26cs=tinysrgb%26dpr=2%26h=650%26w=940", requestHeaders = [("Host","localhost:3000"),("User-Agent","curl/7.68.0"),("Accept","*/*")], isSecure = False, pathInfo = ["createUser"], queryString = [("password",Just "654321"),("first_name",Just "Kate"),("last_name",Just "Grick"),("user_pic_url",Just "https://images.pexels.com/photos/4617160/pexels-photo-4617160.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=650&w=940")], requestBodyLength = KnownLength 0, requestHeaderHost = Just "localhost:3000", requestHeaderRange = Nothing}

main :: IO ()
main = hspec $ do
  describe "CheckExist" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest0) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest0) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe` 
        "{\"ok\":true}"
  describe "createUser" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest1) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,LOGMSG,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest1) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe` 
        "{\"user_id\":5,\"first_name\":\"Kate\",\"last_name\":\"Grick\",\"user_pic_id\":5,\"user_pic_url\":\"http://localhost:3000/picture/5\",\"user_create_date\":\"2020-02-20\"}"

