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

data MockAction = EXISTCHEK | LOGMSG
  deriving (Eq,Show)

data TestDB = TestDB 
  { picsT :: PicsT,
    keyT  :: KeyT,
    userST :: UsersT,
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

testDB1 = emptyDB {picsT = picsT1}

isExistInDbTest :: String -> String -> String -> [Text] -> StateT (TestDB,[MockAction]) IO Bool
isExistInDbTest s s' s'' xs = StateT $ \(db,acts) -> do
  return ( foo s s' s'' xs db , (db,EXISTCHEK:acts))

foo "pics" s' s'' xs db = moo s' s'' xs (picsT db)
moo "pic_id" s'' xs pics = loo s'' xs (fmap pic_idPicsV pics)
loo "pic_id=?" [x] picsIds = elem (read . unpack $ x) picsIds

handleLog1 = LogHandle (LogConfig DEBUG) logTest
handle1 = Handle 
  { hLog = handleLog1,
    isExistInDb = isExistInDbTest
    }

reqTest1 = defaultRequest {requestMethod = "GET", httpVersion = http11, rawPathInfo = "/test/3", rawQueryString = "", requestHeaders = [("User-Agent","PostmanRuntime/7.26.8"),("Accept","*/*"),("Postman-Token","6189d61d-fa65-4fb6-a578-c4061535e7ef"),("Host","localhost:3000"),("Accept-Encoding","gzip, deflate, br"),("Connection","keep-alive"),("Content-Type","multipart/form-data; boundary=--------------------------595887703656508108682668"),("Content-Length","170")], isSecure = False, pathInfo = ["test","3"], queryString = [], requestBodyLength = KnownLength 170, requestHeaderHost = Just "localhost:3000", requestHeaderRange = Nothing}


main :: IO ()
main = hspec $ do
  describe "CheckExist" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest1) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest1) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe` 
        "{\"ok\":true}"

