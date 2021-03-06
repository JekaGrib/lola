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
import           Data.Text                      ( pack, unpack, Text, concat, toUpper, stripPrefix )
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
import           Data.List                      ( intercalate, zip4, nub, find, sortOn, intersect, sort, group, groupBy, isPrefixOf, isInfixOf, isSuffixOf, union )
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
import           Database.PostgreSQL.Simple.ToRow
import           Prelude                        as P


data MockAction = EXISTCHEK | LOGMSG | INSERTDATA | SELECTDATA | UPDATEDATA | DELETEDATA | INSERTMANYDATA | SELECTLIMITDATA
  deriving (Eq,Show)

data TestDB = TestDB 
  { picsT :: PicsT,
    keyT  :: KeyT,
    usersT :: UsersT,
    authorsT :: AuthorsT,
    tagsT :: TagsT,
    catsT :: CatsT,
    postsT :: PostsT,
    commentsT :: CommentsT,
    postsPicsT :: PostsPicsT,
    postsTagsT :: PostsTagsT,
    draftsT :: DraftsT,
    draftsPicsT :: DraftsPicsT,
    draftsTagsT :: DraftsTagsT
  }


type PicsT       = [PicsL]
type KeyT        = [Text]
type UsersT      = [UsersL]
type AuthorsT    = [AuthorsL]
type TagsT       = [TagsL]
type CatsT       = [CatsL]
type PostsT      = [PostsL]
type CommentsT   = [CommentsL]
type PostsPicsT  = [PostsPicsL]
type PostsTagsT  = [PostsTagsL]
type DraftsT     = [DraftsL]
type DraftsPicsT = [DraftsPicsL]
type DraftsTagsT = [DraftsTagsL]

data PicsL = PicsL { pic_idPL ::  Integer, pic_urlPL :: Text }
data UsersL = UsersL { user_idUL :: Integer, passwordUL :: Text, first_nameUL :: Text, last_nameUL :: Text, user_pic_idUL :: Integer, user_create_dateUL :: Day, adminUL :: Bool }
data AuthorsL = AuthorsL { author_idAL :: Integer, author_infoAL :: Text, user_idAL :: Integer }
data TagsL = TagsL { tag_idTL :: Integer, tag_nameTL :: Text }
data CatsL = CatsL { cat_idCL :: Integer, cat_nameCL :: Text, super_cat_idCL :: Maybe Integer }
data PostsL = PostsL { post_idPL :: Integer, author_idPL :: Integer, post_namePL :: Text, post_create_datePL :: Day, post_cat_idPL :: Integer, post_textPL :: Text, post_pic_idPL :: Integer }
data CommentsL = CommentsL { comment_idCL :: Integer, comment_textCL :: Text, post_idCL :: Integer, user_idCL :: Integer }
data PostsPicsL = PostsPicsL { post_idPPL :: Integer, pic_idPPL :: Integer }
data PostsTagsL = PostsTagsL { post_idPTL :: Integer, tag_idPTL :: Integer }
data DraftsL = DraftsL { draft_idDL :: Integer, post_idDL :: Maybe Integer, author_idDL :: Integer, draft_nameDL :: Text, draft_cat_idDL :: Integer, draft_textDL :: Text, draft_pic_idDL :: Integer }
data DraftsPicsL = DraftsPicsL { draft_idDPL :: Integer, pic_idDPL :: Integer }
data DraftsTagsL = DraftsTagsL { draft_idDTL :: Integer, tag_idDTL :: Integer }

-- Types for JOIN tables
data PostsLAuthorsL = PostsLAuthorsL { postsL_PAL :: PostsL, authorsL_PAL :: AuthorsL}
data PostsLAuthorsLUsersL = PostsLAuthorsLUsersL { postsLAuthorsL_PAUL :: PostsLAuthorsL, usersL_PAUL :: UsersL}
data PostsLCatsL = PostsLCatsL { postsL_PCL :: PostsL, catsL_PCL :: CatsL}
data PostsTagsLTagsL = PostsTagsLTagsL { postsTagsL_PTLTL :: PostsTagsL, tagsL_PTLTL :: TagsL}

data JoinTable1 = JoinTable1 { post_idJT1 :: Integer, tags_idJT1 :: [Integer]}
data JoinTable2 = JoinTable2 { post_idJT2 :: Integer, isintagJT2 :: Bool}
data JoinTable3 = JoinTable3 { post_idJT3 :: Integer, count_picsJT3 :: Int}


logTest :: Priority -> String -> StateT (TestDB,[MockAction]) IO ()
logTest prio text = StateT $ \(db,acts) -> 
  return (() , (db,LOGMSG : acts))

emptyDB = TestDB [] [] [] [] [] [] [] [] [] [] [] [] []

picsL1  = PicsL 1  "https://www.publicdomainpictures.net/pictures/20000/t2/parapente.jpg"
picsL2  = PicsL 2  "https://www.stockvault.net/data/2017/05/20/236363/thumb16.jpg"
picsL3  = PicsL 3  "https://cdn.pixabay.com/photo/2016/07/22/18/39/daisy-1535532__340.jpg"
picsL4  = PicsL 4  "https://img.freepik.com/free-photo/poster-with-vertical-frames-on-empty-white-wall-in-living-room-interior-with-blue-velvet-armchair-3d-rendering_41470-2907.jpg?size=626&ext=jpg&ga=GA1.2.42293934.1613723092"
picsL5  = PicsL 5  "https://skitterphoto.com/photos/skitterphoto-9649-thumbnail.jpg"
picsL6  = PicsL 6  "https://cdn.pixabay.com/photo/2021/01/23/18/40/child-5943325__340.jpg"
picsL7  = PicsL 7  "https://skitterphoto.com/photos/skitterphoto-9649-thumbnail.jpg"
picsL8  = PicsL 8  "https://skitterphoto.com/photos/skitterphoto-9391-thumbnail.jpg"
picsL9  = PicsL 9  "https://img.freepik.com/free-photo/confident-young-businessman-in-suit-standing-with-arms-folded_171337-18599.jpg?size=626&ext=jpg&ga=GA1.2.42293934.1613723092"
picsL10 = PicsL 10 "https://skitterphoto.com/photos/skitterphoto-9777-thumbnail.jpg"


picsT1 = [picsL1,picsL2,picsL3,picsL4,picsL5,picsL6,picsL7,picsL8,picsL9,picsL10]

keyT1 = ["lola"]

fG = fromGregorian

usersL1  = UsersL 1  "12345678" "DELETED" "DELETED"   1 (fG 2018 01 01) True
usersL2  = UsersL 2  "87654321" "Lidia"   "Klimova"   2 (fG 2018 02 01) False
usersL3  = UsersL 3  "kukui"    "Ira"     "Medvedeva" 2 (fG 2018 03 01) False
usersL4  = UsersL 4  "1234dom"  "Lisa"    "Karimova"  3 (fG 2018 04 01) True
usersL5  = UsersL 5  "335jsu"   "Anton"   "Petrov"    5 (fG 2018 05 01) False
usersL6  = UsersL 6  "057ccc"   "Vika"    "Petrov"    2 (fG 2018 07 01) False
usersL7  = UsersL 7  "gfjdj123" "Luck"    "Petrov"    7 (fG 2018 07 09) True
usersL8  = UsersL 8  "344los"   "Ben"     "Petrov"    7 (fG 2018 08 23) False
usersL9  = UsersL 9  "rtrfgh"   "Den"     "Petrov"    9 (fG 2018 08 25) True
usersL10 = UsersL 10 "KIH55i"   "Victor"  "Petrov"    8 (fG 2018 11 01) False

usersT1 = [usersL1,usersL2,usersL3,usersL4,usersL5,usersL6,usersL7,usersL8,usersL9,usersL10]

authorsL1 = AuthorsL 1 "DELETED"                1
authorsL2 = AuthorsL 2 "i don`t like it"        4
authorsL3 = AuthorsL 3 "London is the capital"  6
authorsL4 = AuthorsL 4 "i have a cat"           8
authorsL5 = AuthorsL 5 "I have been in Germany" 9

authorsT1 = [authorsL1, authorsL2, authorsL3, authorsL4, authorsL5]

tagsL1  = TagsL 1  "Cats"
tagsL2  = TagsL 2  "Dogs"
tagsL3  = TagsL 3  "Weather"
tagsL4  = TagsL 4  "Love"
tagsL5  = TagsL 5  "Winter"
tagsL6  = TagsL 6  "Sommer"
tagsL7  = TagsL 7  "Autumn"
tagsL8  = TagsL 8  "Spring"
tagsL9  = TagsL 9  "Mondey"
tagsL10 = TagsL 10 "Home"
tagsL11 = TagsL 11 "Work"
tagsL12 = TagsL 12 "Medicine"
tagsL13 = TagsL 13 "Life"
tagsL14 = TagsL 14 "Disco"
tagsL15 = TagsL 15 "Music"

tagsT1 = [tagsL1, tagsL2, tagsL3, tagsL4, tagsL5, tagsL6, tagsL7, tagsL8, tagsL9, tagsL10, tagsL11, tagsL12, tagsL13, tagsL14, tagsL15]

catsL1  = CatsL 1  "DELETED"    Nothing
catsL2  = CatsL 2  "Sport"      Nothing
catsL3  = CatsL 3  "football"   (Just 2)
catsL4  = CatsL 4  "Basketball" (Just 2)
catsL5  = CatsL 5  "Tennus"     (Just 2)
catsL6  = CatsL 6  "Hobby"      Nothing
catsL7  = CatsL 7  "Dance"      (Just 6)
catsL8  = CatsL 8  "Camping"    (Just 6)
catsL9  = CatsL 9  "BigTennis"  (Just 5)
catsL10 = CatsL 10 "Ping-Pong"  (Just 5)
catsL11 = CatsL 11 "Place"      Nothing
catsL12 = CatsL 12 "Europe"     (Just 11)
catsL13 = CatsL 13 "Germany"    (Just 12)
catsL14 = CatsL 14 "Africa"     (Just 11)
catsL15 = CatsL 15 "Poland"     (Just 12)
catsL16 = CatsL 16 "Egypt"      (Just 14)

catsT1 = [catsL1,catsL2,catsL3,catsL4,catsL5,catsL6,catsL7,catsL8,catsL9,catsL10,catsL11,catsL12,catsL13,catsL14,catsL15,catsL16]

txt1 = "Do you want to know the history of jeans? In 1850 a young man, Levi Strauss, came to California from Germany. California was famous for its gold. Many people were working there. They were looking for gold and needed strong clothes. First Levi Strauss sold canvas to workers. Canvas was strong and soon Levi used it to make jeans. All workers liked his jeans and bought them. His first jeans had no colour. Then Levi coloured his jeans. Today everyone in the world knows the famous blue jeans of Levi Strauss."
txt2 = "Advertising companies say advertising is necessary and important. It informs people about new products. Advertising hoardings in the street make our environment colourful. And adverts on TV are often funny. Sometimes they are mini-dramas and we wait for the next programme in the mini-drama. Advertising can educate, too. Adverts tell us about new, healthy products"
txt3 = "some consumers argue that advertising is a bad thing. They say that advertising is bad for children. Adverts make children ‘pester’ their parents to buy things for them. Advertisers know we love our children and want to give them everything."
txt4 = "Marco Polo is famous for his journeys across Asia. He was one of the first Europeans to travel in Mongolia and China. He wrote a famous sook called ‘The Travels’."
txt5 = "I’m having a great time here in Sydney. The different sports are exciting, and there are lots of other exciting things too. For example the mascots are really great! They are called Olly, Syd and Millie. They are Australian ‘ animals and they are the symbols of the Sydney Games. The kookaburra is an Australian bird. She got her name, Olly, from the word ‘Olympics’. She’s a symbol of friendship and honesty. Then there’s Syd (from Sydney). He’s a platypus with a duck’s nose."


postsL1 = PostsL 1 2 "Molly"     (fG 2019 04 01) 12 txt1 7
postsL2 = PostsL 2 2 "Glass"     (fG 2019 06 01) 16 txt2 2
postsL3 = PostsL 3 4 "Sorry"     (fG 2019 07 25) 2  txt3 3
postsL4 = PostsL 4 1 "Parlament" (fG 2019 07 21) 4  txt4 9
postsL5 = PostsL 5 3 "Victory"   (fG 2019 11 01) 7  txt5 6

postsT1 = [postsL1,postsL2,postsL3,postsL4,postsL5]

commentsL1  = CommentsL 1  "Cool"             3 3
commentsL2  = CommentsL 2  "I like it"        4 3
commentsL3  = CommentsL 3  "Sunny"            2 3
commentsL4  = CommentsL 4  "Funny"            2 2
commentsL5  = CommentsL 5  "Very interesting" 2 4
commentsL6  = CommentsL 6  "Pretty"           5 6
commentsL7  = CommentsL 7  "Hmm"              1 5
commentsL8  = CommentsL 8  "Crazy"            1 2
commentsL9  = CommentsL 9  "I think about it" 4 4
commentsL10 = CommentsL 10 "You lucky"        2 2

commentsT1 = [commentsL1,commentsL2,commentsL3,commentsL4,commentsL5,commentsL5,commentsL6,commentsL7,commentsL8,commentsL9,commentsL10]

postsPicsL1  = PostsPicsL 1 2
postsPicsL2  = PostsPicsL 1 3
postsPicsL3  = PostsPicsL 1 4
postsPicsL4  = PostsPicsL 1 5
postsPicsL5  = PostsPicsL 3 7
postsPicsL6  = PostsPicsL 3 8
postsPicsL7  = PostsPicsL 4 4
postsPicsL8  = PostsPicsL 5 2
postsPicsL9  = PostsPicsL 5 8
postsPicsL10 = PostsPicsL 5 10

postsPicsT1 = [postsPicsL1,postsPicsL2,postsPicsL3,postsPicsL3,postsPicsL4,postsPicsL5,postsPicsL6,postsPicsL7,postsPicsL8,postsPicsL9,postsPicsL10]

postsTagsL1  = PostsTagsL 1 11
postsTagsL2  = PostsTagsL 2 1
postsTagsL3  = PostsTagsL 2 12
postsTagsL4  = PostsTagsL 2 10
postsTagsL5  = PostsTagsL 3 7
postsTagsL6  = PostsTagsL 4 2
postsTagsL7  = PostsTagsL 4 8
postsTagsL8  = PostsTagsL 4 14
postsTagsL9  = PostsTagsL 5 5
postsTagsL10 = PostsTagsL 5 15

postsTagsT1 = [postsTagsL1,postsTagsL2,postsTagsL3,postsTagsL4,postsTagsL5,postsTagsL6,postsTagsL7,postsTagsL8,postsTagsL9,postsTagsL10]

txt6  = "The universe is enormous, so the chances of us being the only living creatures are small. Although we think we are intelligent and that we know a lot about Space, we have only explored a very small area. We might be the only creatures that can travel in Space, but it is unlikely."
txt7  = "In fact, some people say that we might have been visited by aliens. These people point to ‘wonders’ such as Stonehenge in Britain and the Nazca lines in Peru as proof that aliens have been here.So, what are the chances that there is life out there?"
txt8  = "Certainly, when one thinks of films the name ‘Hollywood’ comes to mind, but it wasn’t always the movie capital of the world. In the early 1900s a few companies looking for a good location settled in the thriving city of Los Angeles. Both the sea and sunshine made this an ideal location, and over the next 30 years it developed into the ‘home of movies’ and became the only place to be during the so-called Golden Age in the 1930s and 1940s."
txt9  = "People say that travelling is dangerous, for example, driving a car. They point to the fact that there are so many cars on the roads that the chances of an accident are very high. But that’s nothing compared to Space. Space will soon be so dangerous to travel in that only a mad man would even try."
txt10 = "The reason is simple; ever since we started exploring Space in the late 1950s we have been leaving things up there. There is now so much rubbish circling the Earth that from a distance our planet appears to have a ring around it, making it look a bit like Saturn. Unless we start cleaning up after ourselves, we are in danger."

draftsL1 = DraftsL 1 Nothing  5 "Filin" 8  txt6  9   
draftsL2 = DraftsL 2 Nothing  5 "Train" 2  txt7  7   
draftsL3 = DraftsL 3 Nothing  2 "Box"   11 txt8  4   
draftsL4 = DraftsL 4 (Just 3) 4 "Table" 14 txt9  2   
draftsL5 = DraftsL 5 Nothing  4 "Cort"  3  txt10 3

draftsT1 = [draftsL1,draftsL2,draftsL3,draftsL4,draftsL5]

draftsPicsL1  = DraftsPicsL 1 2
draftsPicsL2  = DraftsPicsL 1 5
draftsPicsL3  = DraftsPicsL 2 3
draftsPicsL4  = DraftsPicsL 4 6
draftsPicsL5  = DraftsPicsL 4 8
draftsPicsL6  = DraftsPicsL 4 9
draftsPicsL7  = DraftsPicsL 4 10
draftsPicsL8  = DraftsPicsL 4 3
draftsPicsL9  = DraftsPicsL 5 7
draftsPicsL10 = DraftsPicsL 5 6

draftsPicsT1 = [draftsPicsL1,draftsPicsL2,draftsPicsL3,draftsPicsL4,draftsPicsL5,draftsPicsL6,draftsPicsL7,draftsPicsL8,draftsPicsL9,draftsPicsL10]

draftsTagsL1  = DraftsTagsL 1 3
draftsTagsL2  = DraftsTagsL 2 8
draftsTagsL3  = DraftsTagsL 2 12
draftsTagsL4  = DraftsTagsL 2 14
draftsTagsL5  = DraftsTagsL 2 15
draftsTagsL6  = DraftsTagsL 3 6
draftsTagsL7  = DraftsTagsL 4 8
draftsTagsL8  = DraftsTagsL 5 4
draftsTagsL9  = DraftsTagsL 5 6
draftsTagsL10 = DraftsTagsL 5 9

draftsTagsT1 = [draftsTagsL1,draftsTagsL2,draftsTagsL3,draftsTagsL4,draftsTagsL5,draftsTagsL6,draftsTagsL7,draftsTagsL8,draftsTagsL9,draftsTagsL10]

readGregorian x@(a:b:c:d:'-':e:f:'-':g:h:[]) = fG (yearG x) (monG x) (dayG x)
dayG (a:b:c:d:'-':e:f:'-':g:h:[]) = read (g:h:[])
monG (a:b:c:d:'-':e:f:'-':g:h:[]) = read (e:f:[])
yearG (a:b:c:d:'-':e:f:'-':g:h:[]) = read (a:b:c:d:[])

--testDB1 = emptyDB {picsT = picsT1,usersT = usersT1}
testDB1 = TestDB picsT1 keyT1 usersT1 authorsT1 tagsT1 catsT1 postsT1 commentsT1 postsPicsT1 postsTagsT1 draftsT1 draftsPicsT1 draftsTagsT1

getDayTest = return "2020-02-20"

updateInDbTest :: String -> String -> String -> [Text] -> StateT (TestDB,[MockAction]) IO ()
updateInDbTest table set where' values = StateT $ \(db,acts) -> do
  return $ update table set where' values db (UPDATEDATA:acts)
  
update "comments"   set where' values db acts = updateInComments set where' values db acts
update "posts"      set where' values db acts = updateInPosts    set where' values db acts
update "authors"    set where' values db acts = updateInAuthors  set where' values db acts
update "categories" set where' values db acts = updateInCats     set where' values db acts
update "drafts"     set where' values db acts = updateInDrafts   set where' values db acts
update "tags"       set where' values db acts = updateInTags     set where' values db acts


updateInComments "user_id=?" "user_id=?" [x,y] db acts =
  let numX = (read $ unpack x :: Integer) in
  let numY = (read $ unpack y :: Integer) in
  let updateFoo line acc = if user_idCL line == numY then ( (line {user_idCL = numX}) : acc) else (line:acc) in
  let newCommentsT = foldr updateFoo [] (commentsT db) in
  let newDb = db {commentsT = newCommentsT} in
    ((), (newDb,acts))
updateInComments "comment_text=?" "comment_id=?" [x,y] db acts =
  let numY = (read $ unpack y :: Integer) in
  let updateFoo line acc = if comment_idCL line == numY then ( (line {comment_textCL = x}) : acc) else (line:acc) in
  let newCommentsT = foldr updateFoo [] (commentsT db) in
  let newDb = db {commentsT = newCommentsT} in
    ((), (newDb,acts))

updateInPosts "author_id=?" "author_id=?" [x,y] db acts =
  let numX = (read $ unpack x :: Integer) in
  let numY = (read $ unpack y :: Integer) in
  let updateFoo line acc = if author_idPL line == numY then ((line {author_idPL = numX}) : acc) else (line:acc) in
  let newPostsT = foldr updateFoo [] (postsT db) in
  let newDb = db {postsT = newPostsT} in
    ((), (newDb,acts))
updateInPosts "post_name=?,post_category_id=?,post_text=?,post_main_pic_id=?" "post_id=?" [a,b,c,d,e]  db acts =
  let numB = (read $ unpack b :: Integer) in
  let numD = (read $ unpack d :: Integer) in
  let numE = (read $ unpack e :: Integer) in
  let updateFoo line acc = if post_idPL line == numE then ((line {post_namePL = a,post_cat_idPL = numB,post_textPL = c, post_pic_idPL = numD}) : acc) else (line:acc) in
  let newPostsT = foldr updateFoo [] (postsT db) in
  let newDb = db {postsT = newPostsT} in
    ((), (newDb,acts))
updateInPosts "post_category_id=?" where' (x:values) db acts =
  let numX = (read $ unpack x :: Integer) in
  let numValues = fmap (read . unpack) values in
  let unOr = filter ((/=) "OR") . words in
  let newPostsT = updatePostTable "post_category_id=?" (unOr where') numX numValues (postsT db) in
  let newDb = db {postsT = newPostsT} in
    ((), (newDb,acts))

updatePostTable "post_category_id=?" ["post_category_id=?"] x [y] posts = 
  let updateFoo line acc = if post_cat_idPL line == y then ((line {post_cat_idPL = x}) : acc) else (line:acc) in
    foldr updateFoo [] posts
updatePostTable "post_category_id=?" ("post_category_id=?":zs) x (y:ys) posts = updatePostTable "post_category_id=?" zs x ys (updatePostTable "post_category_id=?" ["post_category_id=?"] x [y] posts)

updateInDrafts "draft_category_id=?" where' (x:values) db acts =
  let numX = (read $ unpack x :: Integer) in
  let numValues = fmap (read . unpack) values in
  let unOr = filter ((/=) "OR") . words in
  let newDraftsT = updateDraftTable "draft_category_id=?" (unOr where') numX numValues (draftsT db) in
  let newDb = db {draftsT = newDraftsT} in
    ((), (newDb,acts))
updateInDrafts "draft_name=?,draft_category_id=?,draft_text=?,draft_main_pic_id=?" "draft_id=?" [x,y,z,a,b] db acts =
  let numY = (read $ unpack y :: Integer) in
  let numA = (read $ unpack a :: Integer) in
  let numB = (read $ unpack b :: Integer) in  
  let updateFoo line acc = if draft_idDL line == numB then ((line {draft_nameDL = x,draft_cat_idDL = numY,draft_textDL = z,draft_pic_idDL = numA}) : acc) else (line:acc) in
  let newDraftsT = foldr updateFoo [] (draftsT db) in
  let newDb = db {draftsT = newDraftsT} in
    ((), (newDb,acts))

updateDraftTable "draft_category_id=?" ["draft_category_id=?"] x [y] drafts = 
  let updateFoo line acc = if draft_cat_idDL line == y then ((line {draft_cat_idDL = x}) : acc) else (line:acc) in
    foldr updateFoo [] drafts
updateDraftTable "draft_category_id=?" ("draft_category_id=?":zs) x (y:ys) drafts = updateDraftTable "draft_category_id=?" zs x ys (updateDraftTable "draft_category_id=?" ["draft_category_id=?"] x [y] drafts)

updateInAuthors "author_info=?,user_id=?" "author_id=?" [x,y,z] db acts =
  let numY = (read $ unpack y :: Integer) in
  let numZ = (read $ unpack z :: Integer) in
  let updateFoo line acc = if author_idAL line == numZ then ((line {author_infoAL = x,user_idAL = numY}) : acc) else (line:acc) in
  let newAuthorsT = foldr updateFoo [] (authorsT db) in
  let newDb = db {authorsT = newAuthorsT} in
    ((), (newDb,acts))

updateInCats "category_name=?,super_category_id=?" "category_id=?" [x,y,z] db acts =
  let numY = (read $ unpack y :: Integer) in
  let numZ = (read $ unpack z :: Integer) in
  let updateFoo line acc = if cat_idCL line == numZ then ((line {cat_nameCL = x,super_cat_idCL = Just numY}) : acc) else (line:acc) in
  let newCatsT = foldr updateFoo [] (catsT db) in
  let newDb = db {catsT = newCatsT} in
    ((), (newDb,acts))

updateInTags "tag_name=?" "tag_id=?" [x,y] db acts =
  let numY = (read $ unpack y :: Integer) in
  let updateFoo line acc = if tag_idTL line == numY then ( (line {tag_nameTL = x}) : acc) else (line:acc) in
  let newTagsT = foldr updateFoo [] (tagsT db) in
  let newDb = db {tagsT = newTagsT} in
    ((), (newDb,acts))

isExistInDbTest ::  String -> String -> String -> [Text] -> StateT (TestDB,[MockAction]) IO Bool
isExistInDbTest table checkName where' values = StateT $ \(db,acts) -> do
  return ( isExist table checkName where' values db , (db,EXISTCHEK:acts))

isExist "pics"       checkName where' values db = isExistInPics    checkName where' values (picsT    db)
isExist "users"      checkName where' values db = isExistInUsers   checkName where' values (usersT   db)
isExist "authors"    checkName where' values db = isExistInAuthors checkName where' values (authorsT db)
isExist "categories" checkName where' values db = isExistInCats    checkName where' values (catsT    db)
isExist "tags"       checkName where' values db = isExistInTags    checkName where' values (tagsT    db)
isExist "posts"      checkName where' values db = isExistInPosts   checkName where' values (postsT   db)
isExist "drafts"     checkName where' values db = isExistInDrafts  checkName where' values (draftsT  db)

--isExistInAuthors "author_id" "user_id=?" [x] authors = not . null $ find ( (==) (read . unpack $ x) . user_idAL ) authors
isExistInPics    "pic_id"      "pic_id=?"      [x] pics    = not . null $ find ( (==) (read . unpack $ x) . pic_idPL ) pics
isExistInUsers   "user_id"     "user_id=?"     [x] users   = not . null $ find ( (==) (read . unpack $ x) . user_idUL ) users 
isExistInAuthors "user_id"     "user_id=?"     [x] authors = not . null $ find ( (==) (read . unpack $ x) . user_idAL ) authors
isExistInAuthors "author_id"   "author_id=?"   [x] authors = not . null $ find ( (==) (read . unpack $ x) . author_idAL ) authors
isExistInCats    "category_id" "category_id=?" [x] cats    = not . null $ find ( (==) (read . unpack $ x) . cat_idCL ) cats
isExistInTags    "tag_id"      "tag_id=?"      [x] tags    = not . null $ find ( (==) (read . unpack $ x) . tag_idTL ) tags
isExistInPosts   "post_id"     "post_id=?"     [x] posts   = not . null $ find ( (==) (read . unpack $ x) . post_idPL ) posts
isExistInDrafts  "draft_id"    "draft_id=?"    [x] drafts  = not . null $ find ( (==) (read . unpack $ x) . draft_idDL ) drafts



insertReturnInDbTest :: String -> String -> [String] -> [Text] -> StateT (TestDB,[MockAction]) IO [Integer]
insertReturnInDbTest table returnName insNames insValues = StateT $ \(db,acts) -> do
  return $ insReturn table returnName insNames insValues db (INSERTDATA:acts)

insReturn ::  String -> String -> [String] -> [Text] -> TestDB -> [MockAction] -> ([Integer],(TestDB,[MockAction]))
insReturn "pics"       returnName insNames insValues db acts = insReturnInPics       returnName insNames insValues db acts
insReturn "users"      returnName insNames insValues db acts = insReturnInUsers      returnName insNames insValues db acts
insReturn "authors"    returnName insNames insValues db acts = insReturnInAuthors    returnName insNames insValues db acts
insReturn "categories" returnName insNames insValues db acts = insReturnInCategories returnName insNames insValues db acts
insReturn "tags"       returnName insNames insValues db acts = insReturnInTags       returnName insNames insValues db acts
insReturn "drafts"     returnName insNames insValues db acts = insReturnInDrafts     returnName insNames insValues db acts
insReturn "posts"      returnName insNames insValues db acts = insReturnInPosts      returnName insNames insValues db acts
insReturn "comments"   returnName insNames insValues db acts = insReturnInComments   returnName insNames insValues db acts



insReturnInPics :: String -> [String] -> [Text] -> TestDB -> [MockAction] -> ([Integer],(TestDB,[MockAction]))
insReturnInPics "pic_id" ["pic_url"] [url] db acts =
  let pT = picsT db in
    case pT of
      [] -> ([1], (db {picsT = [ PicsL 1 url ]}, acts))
      _  -> let num = (pic_idPL . last $ pT) + 1 in
        ([num], (db {picsT = pT ++ [ PicsL num url ]}, acts))

insReturnInComments :: String -> [String] -> [Text] -> TestDB -> [MockAction] -> ([Integer],(TestDB,[MockAction]))
insReturnInComments "comment_id" ["comment_text","post_id","user_id"] [txt,pI,uI] db acts =
  let cT = commentsT db in
    case cT of
      [] -> ([1], (db {commentsT = [ CommentsL 1 txt (read . unpack $ pI) (read . unpack $ uI)]}, acts))
      _  -> let num = (comment_idCL . last $ cT) + 1 in
        ([num], (db {commentsT = cT ++ [ CommentsL num txt (read . unpack $ pI) (read . unpack $ uI)]}, acts))
 
insReturnInUsers :: String -> [String] -> [Text] -> TestDB -> [MockAction] -> ([Integer],(TestDB,[MockAction]))
insReturnInUsers "user_id" ["password","first_name","last_name","user_pic_id","user_create_date","admin"] [pwd,fN,lN,pI,cD,aD] db acts = 
    let uT = usersT db in
    case uT of
      [] -> ([1], ( db {usersT = [ UsersL 1 pwd fN lN (read . unpack $ pI) (readGregorian . unpack $ cD) (read . unpack $ aD) ]}, acts ))
      _  -> let num = (user_idUL . last $ uT) + 1 in
        ([num], ( db {usersT = uT ++ [ UsersL num pwd fN lN (read . unpack $ pI) (readGregorian . unpack $ cD) (read . unpack $ aD) ]}, acts ))

insReturnInAuthors :: String -> [String] -> [Text] -> TestDB -> [MockAction] -> ([Integer],(TestDB,[MockAction]))
insReturnInAuthors "user_id" ["user_id","author_info"] [usId,auI] db acts = 
    let aT = authorsT db in
    case aT of
      [] -> ([1], ( db {authorsT = [ AuthorsL 1 auI (read . unpack $ usId) ]}, acts ))
      _  -> let num = (author_idAL . last $ aT) + 1 in
        ([num], ( db {authorsT = aT ++ [ AuthorsL num auI (read . unpack $ usId) ]}, acts ))

insReturnInCategories :: String -> [String] -> [Text] -> TestDB -> [MockAction] -> ([Integer],(TestDB,[MockAction]))
insReturnInCategories "category_id" ["category_name"] [cN] db acts = 
    let cT = catsT db in
    case cT of
      [] -> ([1], ( db {catsT = [ CatsL 1 cN Nothing]}, acts ))
      _  -> let num = (cat_idCL . last $ cT) + 1 in
        ([num], ( db {catsT = cT ++ [ CatsL num cN Nothing ]}, acts ))
insReturnInCategories "category_id" ["category_name","super_category_id"] [cN,sCId] db acts = 
    let cT = catsT db in
    let numSCId = (read $ unpack sCId) :: Integer in
    case cT of
      [] -> ([1], ( db {catsT = [ CatsL 1 cN (Just numSCId)]}, acts ))
      _  -> let num = (cat_idCL . last $ cT) + 1 in
        ([num], ( db {catsT = cT ++ [ CatsL num cN (Just numSCId) ]}, acts ))

insReturnInTags :: String -> [String] -> [Text] -> TestDB -> [MockAction] -> ([Integer],(TestDB,[MockAction]))
insReturnInTags "tag_id" ["tag_name"] [tN] db acts = 
    let tT = tagsT db in
    case tT of
      [] -> ([1], ( db {tagsT = [ TagsL 1 tN ]}, acts ))
      _  -> let num = (tag_idTL . last $ tT) + 1 in
        ([num], ( db {tagsT = tT ++ [ TagsL num tN ]}, acts ))

insReturnInDrafts :: String -> [String] -> [Text] -> TestDB -> [MockAction] -> ([Integer],(TestDB,[MockAction]))
insReturnInDrafts "draft_id" ["author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"] [auI,drN,catI,txt,picI] db acts = 
    let dT = draftsT db in
    case dT of
      [] -> ([1], ( db {draftsT = [ DraftsL 1 Nothing (read . unpack $ auI) drN (read . unpack $ catI) txt (read . unpack $ picI) ]}, acts ))
      _  -> let num = (draft_idDL . last $ dT) + 1 in
        ([num], ( db {draftsT = dT ++ [ DraftsL num Nothing (read . unpack $ auI) drN (read . unpack $ catI) txt (read . unpack $ picI) ]}, acts ))
insReturnInDrafts "draft_id" ["post_id","author_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"] [pI,auI,drN,catI,txt,picI] db acts = 
    let dT = draftsT db in
    case dT of
      [] -> ([1], ( db {draftsT = [ DraftsL 1 (Just (read . unpack $ pI)) (read . unpack $ auI) drN (read . unpack $ catI) txt (read . unpack $ picI) ]}, acts ))
      _  -> let num = (draft_idDL . last $ dT) + 1 in
        ([num], ( db {draftsT = dT ++ [ DraftsL num (Just (read . unpack $ pI)) (read . unpack $ auI) drN (read . unpack $ catI) txt (read . unpack $ picI) ]}, acts ))

insReturnInPosts :: String -> [String] -> [Text] -> TestDB -> [MockAction] -> ([Integer],(TestDB,[MockAction]))
insReturnInPosts "post_id" ["author_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"] [auI,pN,pDat,catI,txt,picI] db acts = 
  let pT = postsT db in
  case pT of
    [] -> ([1], ( db {postsT = [ PostsL 1 (read . unpack $ auI) pN (readGregorian . unpack $ pDat) (read . unpack $ catI) txt (read . unpack $ picI) ]}, acts ))
    _  -> let num = (post_idPL . last $ pT) + 1 in
      ([num], ( db {postsT = pT ++ [ PostsL num (read . unpack $ auI) pN (readGregorian . unpack $ pDat) (read . unpack $ catI) txt (read . unpack $ picI) ]}, acts ))

insertManyInDbTest :: String -> [String] -> [(Integer,Integer)] -> StateT (TestDB,[MockAction]) IO ()
insertManyInDbTest table insNames insValues = StateT $ \(db,acts) -> do
  return $ (() , (insMany table insNames insValues db, (INSERTMANYDATA:acts)))

insMany "draftspics" names values db = insManyInDraftsPics names values db
insMany "draftstags" names values db = insManyInDraftsTags names values db
insMany "postspics"  names values db = insManyInPostsPics  names values db
insMany "poststags"  names values db = insManyInPostsTags  names values db

insManyInDraftsPics ["draft_id","pic_id"] [(x,y)] db =
  let newDraftsPicsT = (draftsPicsT db) ++ [DraftsPicsL x y] in
    db {draftsPicsT = newDraftsPicsT} 
insManyInDraftsPics ["draft_id","pic_id"] ((x,y):values) db = insManyInDraftsPics ["draft_id","pic_id"] values (insManyInDraftsPics ["draft_id","pic_id"] [(x,y)] db)  

insManyInDraftsTags ["draft_id","tag_id"] [(x,y)] db =
  let newDraftsTagsT = (draftsTagsT db) ++ [DraftsTagsL x y] in
    db {draftsTagsT = newDraftsTagsT}
insManyInDraftsTags ["draft_id","tag_id"] ((x,y):values) db = insManyInDraftsTags ["draft_id","tag_id"] values (insManyInDraftsTags ["draft_id","tag_id"] [(x,y)] db) 

insManyInPostsPics ["post_id","pic_id"] [(x,y)] db =
  let newPostsPicsT = (postsPicsT db) ++ [PostsPicsL x y] in
    db {postsPicsT = newPostsPicsT} 
insManyInPostsPics ["post_id","pic_id"] ((x,y):values) db = insManyInPostsPics ["post_id","pic_id"] values (insManyInDraftsPics ["draft_id","pic_id"] [(x,y)] db)  

insManyInPostsTags ["post_id","tag_id"] [(x,y)] db =
  let newPostsTagsT = (postsTagsT db) ++ [PostsTagsL x y] in
    db {postsTagsT = newPostsTagsT}
insManyInPostsTags ["post_id","tag_id"] ((x,y):values) db = insManyInPostsTags ["post_id","tag_id"] values (insManyInDraftsTags ["draft_id","tag_id"] [(x,y)] db) 
    

selectFromDbTest :: String -> [String] -> String -> [Text] -> StateT (TestDB,[MockAction]) IO [SelectType]
selectFromDbTest table params where' values = StateT $ \(db,acts) -> do
  return (select table params where' values db, (db,SELECTDATA:acts))

select "users"      params where' values db = selectFromUsers      params where' values (usersT   db)
select "authors"    params where' values db = selectFromAuthors    params where' values (authorsT db)
select "drafts"     params where' values db = selectFromDrafts     params where' values (draftsT  db)
select "key"        params where' values db = selectFromKey        params where' values (keyT  db)
select "categories" params where' values db = selectFromCats       params where' values (catsT db)
select "tags"       params where' values db = selectFromTags       params where' values (tagsT db)
select "postspics"  params where' values db = selectFromPostsPics  params where' values (postsPicsT db)
select "draftspics" params where' values db = selectFromDraftsPics params where' values (draftsPicsT db)
select "posts"      params where' values db = selectFromPosts      params where' values (postsT db)
select "comments"   params where' values db = selectFromComments   params where' values (commentsT db)
select "posts AS p JOIN authors AS a ON p.author_id=a.author_id" params where' values db = 
  selectFromPostsAuthors params where' values (postsT db) (authorsT db)
select "posts JOIN authors ON authors.author_id = posts.author_id " params where' values db = 
  selectFromPostsAuthors params where' values (postsT db) (authorsT db)
select "drafts AS d JOIN authors AS a ON d.author_id=a.author_id" params where' values db = 
  selectFromDraftsAuthors params where' values (draftsT db) (authorsT db)
select "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" params where' values db = 
  selectFromPostsTagsTags params where' values (postsTagsT db) (tagsT db)
select "draftstags AS dt JOIN tags ON dt.tag_id=tags.tag_id" params where' values db = 
  selectFromDraftsTagsTags params where' values (draftsTagsT db) (tagsT db)


selectFromUsers ["first_name","last_name","user_pic_id","user_create_date"] "user_id=?" [x] users = 
  let validLines = filter ( (==) (read . unpack $ x) . user_idUL ) users in
    fmap usersLToUser validLines
selectFromUsers ["password","admin"] "user_id=?" [x] users = 
  let validLines = filter ( (==) (read . unpack $ x) . user_idUL ) users in
    fmap usersLToAuth validLines
selectFromUsers ["password"] "user_id=?" [x] users = 
  let validLines = filter ( (==) (read . unpack $ x) . user_idUL ) users in
    fmap (OnlyTxt . passwordUL) validLines

usersLToUser (UsersL id pwd fN lN picId date admBool) = User fN lN picId date

usersLToAuth (UsersL id pwd fN lN picId date admBool) = Auth pwd admBool

selectFromKey ["create_admin_key"] "true" [] key = fmap OnlyTxt key

selectFromPostsPics ["pic_id"] "post_id=?" [x] postspics =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . post_idPPL ) postspics in
    fmap (OnlyInt . pic_idPPL) validLines

selectFromComments ["post_id"] "comment_id=?" [x] comments =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . comment_idCL ) comments in
    fmap (OnlyInt . post_idCL) validLines
selectFromComments ["user_id"] "comment_id=?" [x] comments =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . comment_idCL ) comments in
    fmap (OnlyInt . user_idCL) validLines

selectFromDrafts ["COALESCE (post_id, '0') AS post_id"] "draft_id=?" [x] drafts =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . draft_idDL ) drafts in
    fmap (OnlyInt . coalesce . post_idDL) validLines
selectFromDrafts ["draft_id"] "author_id=?" [x] drafts =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . author_idDL ) drafts in
    fmap (OnlyInt . draft_idDL) validLines
selectFromDrafts ["draft_id"] "post_id=?" [x] drafts =
  let numX = read $ unpack x in
  let validLines = filter ( (==) (Just numX) . post_idDL ) drafts in
    fmap (OnlyInt . draft_idDL) validLines

selectFromDraftsPics ["pic_id"] "draft_id=?" [x] draftspics =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . draft_idDPL ) draftspics in
    fmap (OnlyInt . pic_idDPL) validLines


selectFromAuthors ["author_id"] "user_id=?" [x] authors =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . user_idAL ) authors in
    fmap (OnlyInt . author_idAL) validLines
selectFromAuthors ["author_id","author_info","user_id"] "author_id=?" [x] authors =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . author_idAL ) authors in
    fmap authorsLToAuthor  validLines
selectFromAuthors ["author_id","author_info","user_id"] "user_id=?" [x] authors =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . user_idAL ) authors in
    fmap authorsLToAuthor  validLines

authorsLToAuthor (AuthorsL id info usId) = Author id info usId



selectFromCats ["category_name","COALESCE (super_category_id, '0') AS super_category_id"] "category_id=?" [x] cats =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . cat_idCL ) cats in
    fmap catsLToCatCoalesce  validLines
selectFromCats ["category_id"] "super_category_id=?" [x] cats =
  let numX = read $ unpack x in
  let validLines = filter ( (==) (Just numX) . super_cat_idCL ) cats in
    fmap (OnlyInt . cat_idCL) validLines

catsLToCatCoalesce (CatsL id name superCat) = Cat name (coalesce superCat)

coalesce (Just x) = x
coalesce Nothing  = 0

selectFromTags ["tag_name"] "tag_id=?" [x] tags =
  let numX = read $ unpack x in
  let validLines = filter ( (==) numX . tag_idTL ) tags in
    fmap (OnlyTxt . tag_nameTL) validLines

selectFromTags ["tag_id","tag_name"] where' values tags =
  let numValues = fmap (read . unpack) values in
  let unOr = filter ((/=) "OR") . words in
  let validLines = findValidLinesInTags (unOr where') numValues tags in
    fmap tagsLToTag validLines

tagsLToTag (TagsL id name) = Tag id name

findValidLinesInTags ["tag_id=?"] [x] tags = filter ( (==) x . tag_idTL ) tags
findValidLinesInTags ("tag_id=?":ys) (x:xs) tags = findValidLinesInTags ys xs (findValidLinesInTags ["tag_id=?"] [x] tags)

selectFromPostsAuthors ["user_id"] "post_id=?" [x] posts authors =
  let numX = read $ unpack x in
  let validPostsLines = filter ( (==) numX . post_idPL ) posts in
  let joinAuthorLine postL = fmap (\auL -> PostsLAuthorsL postL auL) $ filter ( ((==) (author_idPL postL)) . author_idAL) authors in
  let validLines = concatMap joinAuthorLine validPostsLines in
    fmap (OnlyInt . user_idAL . authorsL_PAL)  validLines
selectFromPostsAuthors ["a.author_id","author_info","post_name","post_category_id","post_text","post_main_pic_id"]
 "post_id=?" [x] posts authors =
    let numX = read $ unpack x in
    let validPostsLines = filter ( (==) numX . post_idPL ) posts in
    let joinAuthorLine postL = fmap (\auL -> PostsLAuthorsL postL auL) $ filter ( ((==) (author_idPL postL)) . author_idAL) authors in
    let validLines = concatMap joinAuthorLine validPostsLines in
      fmap toPostInfo  validLines
selectFromPostsAuthors ["posts.post_id","posts.author_id","author_info","user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
 "post_id=?" [x] posts authors =
    let numX = read $ unpack x in
    let validPostsLines = filter ( (==) numX . post_idPL ) posts in
    let joinAuthorLine postL = fmap (\auL -> PostsLAuthorsL postL auL) $ filter ( ((==) (author_idPL postL)) . author_idAL) authors in
    let validLines = concatMap joinAuthorLine validPostsLines in
      fmap toPost  validLines

toPost (PostsLAuthorsL (PostsL pId auI pN pDat pCatI pTxt pPicI) (AuthorsL auId auInfo usId)) = 
  Post pId auId auInfo usId pN pDat pCatI pTxt pPicI
toPostInfo (PostsLAuthorsL (PostsL pId auI pN pDat pCatI pTxt pPicI) (AuthorsL auId auInfo usId)) = 
  PostInfo auId auInfo pN pCatI pTxt pPicI

selectFromDraftsAuthors ["user_id"] "draft_id=?" [x] drafts authors =
  let numX = read $ unpack x in
  let validDraftsLines = filter ( (==) numX . draft_idDL ) drafts in
  let joinAuthorLine draftLine = fmap ((,) draftLine) $ filter ( ((==) (author_idDL draftLine)) . author_idAL) authors in
  let validLines = concatMap joinAuthorLine validDraftsLines in
    fmap (OnlyInt . user_idAL . snd)  validLines
selectFromDraftsAuthors ["d.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"]
 "draft_id=?" [x] drafts authors =
    let numX = read $ unpack x in
    let validDraftsLines = filter ( (==) numX . draft_idDL ) drafts in
    let joinAuthorLine draftLine = fmap ((,) draftLine) $ filter ( ((==) (author_idDL draftLine)) . author_idAL) authors in
    let validLines = concatMap joinAuthorLine validDraftsLines in
      fmap toDraft validLines

toDraft ((DraftsL dI Nothing auI dN dCatI dTxt dPicI),(AuthorsL auId auInfo usId)) =
  Draft dI auInfo 0 dN dCatI dTxt dPicI
toDraft ((DraftsL dI (Just pI) auI dN dCatI dTxt dPicI),(AuthorsL auId auInfo usId)) =
  Draft dI auInfo pI dN dCatI dTxt dPicI

selectFromPostsTagsTags ["tags.tag_id","tag_name"] "post_id=?" [x] poststags tags =
  let numX = read $ unpack x in
  let validPostsTagsLines = filter ( (==) numX . post_idPTL ) poststags in
  let joinTagLine postTagLine = fmap ((,) postTagLine) $ filter ( ((==) (tag_idPTL postTagLine)) . tag_idTL) tags in
  let validLines = concatMap joinTagLine validPostsTagsLines in
    fmap toTag validLines

selectFromDraftsTagsTags ["tags.tag_id","tag_name"] "draft_id=?" [x] draftstags tags =
  let numX = read $ unpack x in
  let validDraftsTagsLines = filter ( (==) numX . draft_idDTL ) draftstags in
  let joinTagLine draftTagLine = fmap ((,) draftTagLine) $ filter ( ((==) (tag_idDTL draftTagLine)) . tag_idTL) tags in
  let validLines = concatMap joinTagLine validDraftsTagsLines in
    fmap toTag validLines

toTag (line,tagL) = tagsLToTag tagL

selectFromPosts ["post_create_date"] "post_id=?" [x] posts = 
  let validLines = filter ( (==) (read . unpack $ x) . post_idPL ) posts in
    fmap (OnlyDay . post_create_datePL) validLines

selectLimitFromDbTest :: String -> String -> Integer -> Integer -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> StateT (TestDB,[MockAction]) IO [SelectType]
selectLimitFromDbTest table orderBy page limitNumber params where' values filterargs sortargs = StateT $ \(db,acts) -> do
  return (selectLim table orderBy page limitNumber params where' values filterargs sortargs db, (db,SELECTLIMITDATA:acts))

fI = fromInteger

runSort :: [Integer] -> [([Integer] -> [[Integer]])] -> [[Integer]]
runSort [] _  = [[]]
runSort [x] _ = [[x]]
runSort xs [] = [xs]
runSort xs (f:[]) = f xs
runSort xs (f1:f2:[]) = concatMap f2 $ runSort xs [f1]
runSort xs fs = concatMap (last fs) $ runSort xs (init fs)

selectLim "drafts JOIN authors ON authors.author_id = drafts.author_id" "draft_id DESC" page limitNum ["drafts.draft_id","author_info","COALESCE (post_id, '0') AS post_id","draft_name","draft_category_id","draft_text","draft_main_pic_id"] 
  "drafts.author_id = ?" [x] [] [] db =
    let drafts = draftsT db in
    let authors = authorsT db in
    let numX = read $ unpack x in
    let validDraftsLines = filter ( (==) numX . author_idDL ) drafts in
    let joinAuthorLine draftLine = fmap ((,) draftLine) $ filter ( ((==) (author_idDL draftLine)) . author_idAL) authors in
    let validLines = concatMap joinAuthorLine validDraftsLines in
      drop (fI ((page-1)*limitNum)) . take (fI (page*limitNum)) . reverse . sortOn draft_idD $ (fmap toDraft validLines)
selectLim "comments" "comment_id DESC" pageNum commentNumberLimit ["comment_id","user_id","comment_text"] "post_id=?" [x] [] [] db =
  let numX = read $ unpack x in
  let validComLs = filter ( (==) numX . post_idCL ) (commentsT db) in
    fmap toComm validComLs
selectLim "posts JOIN authors ON authors.author_id = posts.author_id" defOrderBy page limitNum ["posts.post_id","posts.author_id","author_info","authors.user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"] 
  "true" [] filterargs sortargs db =
    let validPostIds = nub . intersectAll . fmap (filterIt db) $ filterargs in
    let sortFoos = fmap (sortFoo db) sortargs in
    let postIds = P.concat $ runSort validPostIds sortFoos in
    fmap toPost $ concatMap (toPostsLAuthorsL db) postIds

toComm (CommentsL id txt pI usId) = Comment id usId txt

intersectAll [] = []
intersectAll [x] = x
intersectAll (x:xs) = x `intersect` (intersectAll xs)

toPostLines :: TestDB -> Integer -> [PostsL]
toPostLines db postId = filter ( ((==) postId) . post_idPL) (postsT db)

toPostsLAuthorsL :: TestDB -> Integer -> [PostsLAuthorsL]
toPostsLAuthorsL db postId  =
  let joinAuthorLine postL = fmap (\auL -> PostsLAuthorsL postL auL) $ filter ( ((==) (author_idPL postL)) . author_idAL) (authorsT db) in
    concatMap joinAuthorLine . toPostLines db $ postId

fromIlike :: String -> String
fromIlike str = unEscapeUnderSc . unEscapePercent . tail . init $ str 

unEscapePercent :: String -> String
unEscapePercent [] = [] 
unEscapePercent xs@(y:ys) = case isInfixOf "\\%" xs of
  False -> xs
  True  -> case isPrefixOf "\\%" xs of
    False -> y : (unEscapePercent ys)
    True  -> unEscapePercent ('%':(drop 2 xs))

unEscapeUnderSc :: String -> String
unEscapeUnderSc [] = [] 
unEscapeUnderSc xs@(y:ys) = case isInfixOf "\\_" xs of
  False -> xs
  True  -> case isPrefixOf "\\_" xs of
    False -> y : (unEscapeUnderSc ys)
    True  -> unEscapeUnderSc ('_':(drop 2 xs))

filterIt :: TestDB -> FilterArg -> [Integer]
filterIt db (FilterArg "" "post_create_date = ?" ([],[x])) = fmap post_idPL . filter (((==) (readGregorian . unpack $ x)) . post_create_datePL) . postsT $ db
filterIt db (FilterArg "" "post_create_date < ?" ([],[x])) = fmap post_idPL . filter (((>) (readGregorian . unpack $ x)) . post_create_datePL) . postsT $ db
filterIt db (FilterArg "" "post_create_date > ?" ([],[x])) = fmap post_idPL . filter (((<) (readGregorian . unpack $ x)) . post_create_datePL) . postsT $ db
filterIt db (FilterArg "" "post_category_id = ?" ([],[x])) = 
  let numX = read $ unpack x in
    fmap post_idPL . filter (((==) numX) . post_cat_idPL) . postsT $ db
filterIt db (FilterArg "JOIN (SELECT post_id FROM poststags WHERE tag_id = ? GROUP BY post_id) AS t ON posts.post_id=t.post_id"
  "true" ([x],[])) = 
    let numX = read $ unpack x in
      fmap post_idPTL . filter ( ((==) numX) . tag_idPTL ) $ (postsTagsT db) 
--filterIt db (FilterArg "JOIN (SELECT post_id FROM poststags WHERE tag_id IN (" ++ (init . tail . unpack $ x) ++ ") GROUP BY post_id) AS t ON posts.post_id=t.post_id"
  --"true" ([],[])) = fmap (fromOnlyInt . post_idPTL) . filter ( (==) numX . tag_idPTL ) (postsTagsT db)
--filterIt db (FilterArg "JOIN (SELECT post_id, array_agg(ARRAY[tag_id]) AS tags_id FROM poststags GROUP BY post_id) AS t ON posts.post_id=t.post_id"     
  --"tags_id @> ARRAY" ++ show xs ++ "::bigint[]" ([],[])) = fmap (fromOnlyInt . post_idPTL) . filter ( (==) numX . tag_idPTL ) (postsTagsT db)
filterIt db (FilterArg "" "post_name ILIKE ?" ([],[xs])) = fmap post_idPL . filter (isInfixOf (fromIlike . unpack $ xs) . unpack . post_namePL) . postsT $ db
filterIt db (FilterArg "" "post_text ILIKE ?" ([],[xs])) = fmap post_idPL . filter (isInfixOf (fromIlike . unpack $ xs) . unpack . post_textPL) . postsT $ db
filterIt db (FilterArg "JOIN users AS usrs ON authors.user_id=usrs.user_id JOIN categories AS c ON c.category_id=posts.post_category_id JOIN (SELECT pt.post_id, bool_or(tag_name ILIKE ? ) AS isintag FROM poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id  GROUP BY pt.post_id) AS tg ON tg.post_id=posts.post_id"
  "(post_text ILIKE ? OR post_name ILIKE ? OR usrs.first_name ILIKE ? OR c.category_name ILIKE ? OR isintag = TRUE)" ([xs],[ys,zs,as,bs])) =
    let joinAuthorLine postL = fmap (\auL -> PostsLAuthorsL postL auL) $ filter ( ((==) (author_idPL postL)) . author_idAL) (authorsT db) in
    let joinUserLine pAuL = fmap (\uL -> PostsLAuthorsLUsersL pAuL uL) $ filter ( ((==) (user_idAL . authorsL_PAL $ pAuL)) . user_idUL) (usersT db) in
    let joinCatLine pL = fmap (\cL -> PostsLCatsL pL cL) $ filter ( ((==) (post_cat_idPL pL)) . cat_idCL) (catsT db) in
    let joinTagLine ptL = fmap (\tL -> PostsTagsLTagsL ptL tL) $ filter ( ((==) (tag_idPTL ptL)) . tag_idTL) (tagsT db) in
    let validPostTxtIds  = nub . fmap post_idPL . filter (isInfixOf (fromIlike . unpack $ xs) . unpack . post_textPL) . postsT $ db in
    let validPostNameIds = nub . fmap post_idPL . filter (isInfixOf (fromIlike . unpack $ xs) . unpack . post_namePL) . postsT $ db in
    let validUsNameIds   = nub . fmap (post_idPL . postsL_PAL . postsLAuthorsL_PAUL ) . filter (isInfixOf (fromIlike . unpack $ xs) . unpack . first_nameUL . usersL_PAUL ) . concatMap joinUserLine . concatMap joinAuthorLine $ (postsT db) in
    let validCatNameIds  = nub . fmap (post_idPL . postsL_PCL ) . filter (isInfixOf (fromIlike . unpack $ xs) . unpack . cat_nameCL . catsL_PCL ) . concatMap joinCatLine $ (postsT db) in
    let validTagNameIds  = nub . fmap (post_idPTL . postsTagsL_PTLTL ) . filter (isInfixOf (fromIlike . unpack $ xs) . unpack . tag_nameTL . tagsL_PTLTL ) . concatMap joinTagLine $ (postsTagsT db) in
      validPostTxtIds `union` validPostNameIds `union` validUsNameIds `union` validCatNameIds `union` validTagNameIds
filterIt db (FilterArg "JOIN users AS us ON authors.user_id=us.user_id"
  "us.first_name = ?" ([],[x])) =
    let joinAuthorLine postL = fmap (\auL -> PostsLAuthorsL postL auL) $ filter ( ((==) (author_idPL postL)) . author_idAL) (authorsT db) in
    let joinUserLine pAuL = fmap (\uL -> PostsLAuthorsLUsersL pAuL uL) $ filter ( ((==) (user_idAL . authorsL_PAL $ pAuL)) . user_idUL) (usersT db) in
    fmap (post_idPL . postsL_PAL . postsLAuthorsL_PAUL ) . filter ((==) (x) . first_nameUL . usersL_PAUL ) . concatMap joinUserLine . concatMap joinAuthorLine $ (postsT db)
filterIt db (FilterArg "JOIN (SELECT post_id, array_agg(ARRAY[tag_id]) AS tags_id FROM poststags GROUP BY post_id) AS t ON posts.post_id=t.post_id"
  where' ([],[])) =
    let phrase1 = "tags_id @> ARRAY" in
    let phrase2 = "::bigint[]" in
    case and [(isPrefixOf phrase1 where'),(isSuffixOf phrase2 where')] of
      False -> error "Pattern match fail in filterIt function in \"tags_all\" search"
      True  -> 
        let xs = read . reverse . drop (length phrase2) . reverse . drop (length phrase1) $ where' :: [Integer] in
        let joinTable1 postsTagsLs = case postsTagsLs of {[] -> JoinTable1 0 []; lines -> JoinTable1 (post_idPTL . head $ lines) (fmap tag_idPTL lines)} in
          fmap post_idJT1 . filter ( (\ys -> all (`elem` ys) xs) . tags_idJT1) . fmap joinTable1 . groupMy post_idPTL . sortOn post_idPTL $ (postsTagsT db)
filterIt db (FilterArg table "true" ([],[])) =
    let phrase1 = "JOIN (SELECT post_id FROM poststags WHERE tag_id IN (" in
    let phrase2 = ") GROUP BY post_id) AS t ON posts.post_id=t.post_id" in
    case and [(isPrefixOf phrase1 table),(isSuffixOf phrase2 table)] of
      False -> error "Pattern match fail in filterIt function in where='true' search"
      True  -> 
        let xs = read . reverse . (']':) . drop (length phrase2) . reverse . ('[':) . drop (length phrase1) $ table :: [Integer] in
        let joinTable1 postsTagsLs = case postsTagsLs of {[] -> JoinTable1 0 []; lines -> JoinTable1 (post_idPTL . head $ lines) (fmap tag_idPTL lines)} in
          fmap post_idJT1 . filter ( (\ys -> any (`elem` ys) xs) . tags_idJT1) . fmap joinTable1 . groupMy post_idPTL . sortOn post_idPTL $ (postsTagsT db)

groupMy :: Ord b => (a -> b) -> ([a] -> [[a]])
groupMy f = groupBy (\a b -> f a == f b)

sortFoo :: TestDB -> SortArg -> ([Integer] -> [[Integer]])
sortFoo db (SortArg "JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
  "count_pics ASC" dS) =
    let pullValidLines postId = filter ( (==) postId . post_idPPL ) (postsPicsT db) in
      (fmap . fmap) post_idJT3 . groupMy count_picsJT3 . sortOn count_picsJT3 . fmap toJoinTable3 . group . sort . fmap post_idPPL . concatMap pullValidLines 
sortFoo db (SortArg "JOIN (SELECT post_id, count (post_id) AS count_pics FROM postspics GROUP BY post_id) AS counts ON posts.post_id=counts.post_id"
  "count_pics DESC" dS) =
    let pullValidLines postId = filter ( (==) postId . post_idPPL ) (postsPicsT db) in
      reverse . (fmap . fmap) post_idJT3 . groupMy count_picsJT3 . sortOn count_picsJT3 . fmap toJoinTable3 . group . sort . fmap post_idPPL . concatMap pullValidLines 
sortFoo db (SortArg "JOIN categories ON posts.post_category_id=categories.category_id"
  "category_name ASC" dS) =
    let joinCatLine pL = fmap (\cL -> PostsLCatsL pL cL) $ filter ( ((==) (post_cat_idPL pL)) . cat_idCL) (catsT db) in
      (fmap . fmap) (post_idPL . postsL_PCL ) . groupMy (cat_nameCL . catsL_PCL ) . sortOn (cat_nameCL . catsL_PCL ) . concatMap joinCatLine . concatMap (toPostLines db) 
sortFoo db (SortArg "JOIN categories ON posts.post_category_id=categories.category_id"
  "category_name DESC" dS) =
    let joinCatLine pL = fmap (\cL -> PostsLCatsL pL cL) $ filter ( ((==) (post_cat_idPL pL)) . cat_idCL) (catsT db) in
      reverse . (fmap . fmap) (post_idPL . postsL_PCL ). groupMy (cat_nameCL . catsL_PCL ) . sortOn (cat_nameCL . catsL_PCL ) . concatMap joinCatLine . concatMap (toPostLines db) 
sortFoo db (SortArg "JOIN users AS u ON authors.user_id=u.user_id" "u.first_name ASC" dS) = 
  let joinUserLine pAuL = fmap (\uL -> PostsLAuthorsLUsersL pAuL uL) $ filter ( ((==) (user_idAL . authorsL_PAL $ pAuL)) . user_idUL) (usersT db) in
    (fmap . fmap) (post_idPL . postsL_PAL . postsLAuthorsL_PAUL) . groupMy (first_nameUL . usersL_PAUL) . sortOn (first_nameUL . usersL_PAUL) . concatMap joinUserLine . concatMap (toPostsLAuthorsL db)
sortFoo db (SortArg "JOIN users AS u ON authors.user_id=u.user_id" "u.first_name DESC" dS) = 
  let joinUserLine pAuL = fmap (\uL -> PostsLAuthorsLUsersL pAuL uL) $ filter ( ((==) (user_idAL . authorsL_PAL $ pAuL)) . user_idUL) (usersT db) in
    reverse . (fmap . fmap) (post_idPL . postsL_PAL . postsLAuthorsL_PAUL) . groupMy (first_nameUL . usersL_PAUL) . sortOn (first_nameUL . usersL_PAUL) . concatMap joinUserLine . concatMap (toPostsLAuthorsL db)
sortFoo db (SortArg "" "true" dS) = (\a -> [a])

--defOrdFuncs db "post_create_date ASC, post_id ASC" =
  --sortOn post_create_datePL . concatMap (toPostLines db)

toJoinTable3 []        = JoinTable3 0 0
toJoinTable3 xs@(y:ys) = JoinTable3 y (length xs)


deleteFromDbTest :: String -> String -> [Text] -> StateT (TestDB,[MockAction]) IO ()
deleteFromDbTest table where' values = StateT $ \(db,acts) -> do
  return $ delete' table where' values db (DELETEDATA:acts)

delete' :: String -> String -> [Text] -> TestDB -> [MockAction] -> ( (), (TestDB,[MockAction]))
delete' "users"      where' values db acts = deleteFromUsers   where' values db acts
delete' "authors"    where' values db acts = deleteFromAuthors where' values db acts
delete' "drafts"     where' values db acts = deleteFromDrafts where' values db acts
delete' "draftspics" where' values db acts = deleteFromDraftsPics where' values db acts
delete' "draftstags" where' values db acts = deleteFromDraftsTags where' values db acts
delete' "categories" where' values db acts = deleteFromCats where' values db acts
delete' "tags"       where' values db acts = deleteFromTags where' values db acts
delete' "poststags"  where' values db acts = deleteFromPostsTags where' values db acts
delete' "postspics"  where' values db acts = deleteFromPostsPics where' values db acts
delete' "posts"      where' values db acts = deleteFromPosts     where' values db acts
delete' "comments"   where' values db acts = deleteFromComments  where' values db acts

deleteFromUsers "user_id=?" [x] db acts = 
  let numX = read $ unpack x in
  let newUsersT = filter  ((==) numX . user_idUL) (usersT db) in
  let newDb = db {usersT = newUsersT} in
    ((), (newDb,acts))

deleteFromPosts "post_id=?" [x] db acts = 
  let numX = read $ unpack x in
  let newPostsT = filter  ((==) numX . post_idPL) (postsT db) in
  let newDb = db {postsT = newPostsT} in
    ((), (newDb,acts))

deleteFromComments "post_id=?" [x] db acts = 
  let numX = read $ unpack x in
  let newCommentsT = filter  ((==) numX . post_idCL) (commentsT db) in
  let newDb = db {commentsT = newCommentsT} in
    ((), (newDb,acts))
deleteFromComments "comment_id=?" [x] db acts = 
  let numX = read $ unpack x in
  let newCommentsT = filter  ((==) numX . comment_idCL) (commentsT db) in
  let newDb = db {commentsT = newCommentsT} in
    ((), (newDb,acts))

deleteFromAuthors "author_id=?" [x] db acts = 
  let numX = read $ unpack x in
  let newAuthorsT = filter  ((/=) numX . author_idAL) (authorsT db) in
  let newDb = db {authorsT = newAuthorsT} in
    ((), (newDb,acts))

deleteFromDrafts where' values db acts = 
  let numValues = fmap (read . unpack) values in
  let unOr = filter ((/=) "OR") . words in
  let newDraftsT = makeNewDraftsT (unOr where') numValues (draftsT db) in
  let newDb = db {draftsT = newDraftsT} in
    ((), (newDb,acts))    

makeNewDraftsT [] [] drafts = drafts
makeNewDraftsT ["draft_id=?"] [numX] drafts = filter ((/=) numX . draft_idDL) drafts
makeNewDraftsT ("draft_id=?":ys) (numX:xs) drafts = makeNewDraftsT ys xs (makeNewDraftsT ["draft_id"] [numX] drafts) 

deleteFromCats where' values db acts = 
  let numValues = fmap (read . unpack) values in
  let unOr = filter ((/=) "OR") . words in
  let newCatsT = makeNewCatsT (unOr where') numValues (catsT db) in
  let newDb = db {catsT = newCatsT} in
    ((), (newDb,acts))

makeNewCatsT ["category_id=?"] [numX] cats = filter ((/=) numX . cat_idCL) cats
makeNewCatsT ("category_id=?":ys) (numX:xs) cats = makeNewCatsT ys xs (makeNewCatsT ["category_id"] [numX] cats) 

deleteFromDraftsPics where' values db acts = 
  let numValues = fmap (read . unpack) values in
  let unOr = filter ((/=) "OR") . words in
  let newDraftsPicsT = makeNewDraftsPicsT (unOr where') numValues (draftsPicsT db) in
  let newDb = db {draftsPicsT = newDraftsPicsT} in
    ((), (newDb,acts))    

makeNewDraftsPicsT [] [] draftspics = draftspics
makeNewDraftsPicsT ["draft_id=?"] [numX] draftspics = filter ((/=) numX . draft_idDPL) draftspics
makeNewDraftsPicsT ("draft_id=?":ys) (numX:xs) draftspics = makeNewDraftsPicsT ys xs (makeNewDraftsPicsT ["draft_id"] [numX] draftspics) 

deleteFromDraftsTags where' values db acts = 
  let numValues = fmap (read . unpack) values in
  let unOr = filter ((/=) "OR") . words in
  let newDraftsTagsT = makeNewDraftsTagsT (unOr where') numValues (draftsTagsT db) in
  let newDb = db {draftsTagsT = newDraftsTagsT} in
    ((), (newDb,acts))    

makeNewDraftsTagsT [] [] draftstags = draftstags
makeNewDraftsTagsT ["draft_id=?"] [numX] draftstags = filter ((/=) numX . draft_idDTL) draftstags
makeNewDraftsTagsT ("draft_id=?":ys) (numX:xs) draftstags = makeNewDraftsTagsT ys xs (makeNewDraftsTagsT ["draft_id"] [numX] draftstags) 
makeNewDraftsTagsT ["tag_id=?"] [numX] draftstags = filter ((/=) numX . tag_idDTL) draftstags

deleteFromTags "tag_id=?" [x] db acts = 
  let numX = read $ unpack x in
  let newTagsT = filter  ((==) numX . tag_idTL) (tagsT db) in
  let newDb = db {tagsT = newTagsT} in
    ((), (newDb,acts))

deleteFromPostsPics where' values db acts = 
  let numValues = fmap (read . unpack) values in
  let unOr = filter ((/=) "OR") . words in
  let newPostsPicsT = makeNewPostsPicsT (unOr where') numValues (postsPicsT db) in
  let newDb = db {postsPicsT = newPostsPicsT} in
    ((), (newDb,acts))    

makeNewPostsPicsT [] [] postspics = postspics
makeNewPostsPicsT ["post_id=?"] [numX] postspics = filter ((/=) numX . post_idPPL) postspics
makeNewPostsPicsT ("post_id=?":ys) (numX:xs) postspics = makeNewPostsPicsT ys xs (makeNewPostsPicsT ["post_id"] [numX] postspics) 

deleteFromPostsTags where' values db acts = 
  let numValues = fmap (read . unpack) values in
  let unOr = filter ((/=) "OR") . words in
  let newPostsTagsT = makeNewPostsTagsT (unOr where') numValues (postsTagsT db) in
  let newDb = db {postsTagsT = newPostsTagsT} in
    ((), (newDb,acts))    

makeNewPostsTagsT [] [] poststags = poststags
makeNewPostsTagsT ["post_id=?"] [numX] poststags = filter ((/=) numX . post_idPTL) poststags
makeNewPostsTagsT ("draft_id=?":ys) (numX:xs) poststags = makeNewPostsTagsT ys xs (makeNewPostsTagsT ["post_id"] [numX] poststags) 
makeNewPostsTagsT ["tag_id=?"] [numX] poststags = filter ((/=) numX . tag_idPTL) poststags


handleLog1 = LogHandle (LogConfig DEBUG) logTest
handle1 = Handle 
  { hConf = Config 1 1 1 1,
    hLog = handleLog1,
    selectFromDb = selectFromDbTest,
    selectLimitFromDb = selectLimitFromDbTest,
    updateInDb = updateInDbTest,
    deleteFromDb = deleteFromDbTest,
    isExistInDb = isExistInDbTest,
    insertReturnInDb = insertReturnInDbTest,
    insertManyInDb = insertManyInDbTest,
    getDay = getDayTest,
    httpAction = HT.httpLBS,
    getBody = getBodyTest1
    }

reqTest0  = defaultRequest {requestMethod = "GET", httpVersion = http11, rawPathInfo = "/test/3", rawQueryString = "", requestHeaders = [("User-Agent","PostmanRuntime/7.26.8"),("Accept","*/*"),("Postman-Token","6189d61d-fa65-4fb6-a578-c4061535e7ef"),("Host","localhost:3000"),("Accept-Encoding","gzip, deflate, br"),("Connection","keep-alive"),("Content-Type","multipart/form-data; boundary=--------------------------595887703656508108682668"),("Content-Length","170")], isSecure = False, pathInfo = ["test","3"], queryString = [], requestBodyLength = KnownLength 170, requestHeaderHost = Just "localhost:3000", requestHeaderRange = Nothing}
reqTest1  = defaultRequest {requestMethod = "GET", httpVersion = http11, rawPathInfo = "/createUser", rawQueryString = "?password=654321&first_name=Kate&last_name=Grick&user_pic_url=https://images.pexels.com/photos/4617160/pexels-photo-4617160.jpeg?auto=compress%26cs=tinysrgb%26dpr=2%26h=650%26w=940", requestHeaders = [("Host","localhost:3000"),("User-Agent","curl/7.68.0"),("Accept","*/*")], isSecure = False, pathInfo = ["createUser"], queryString = [("password",Just "654321"),("first_name",Just "Kate"),("last_name",Just "Grick"),("user_pic_url",Just "https://images.pexels.com/photos/4617160/pexels-photo-4617160.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=650&w=940")], requestBodyLength = KnownLength 0, requestHeaderHost = Just "localhost:3000", requestHeaderRange = Nothing}
reqTest2  = reqTest1 {rawPathInfo = "/getUser/3"        , rawQueryString = "", pathInfo = ["getUser","3"], queryString = []}
reqTest3  = reqTest1 {rawPathInfo = "/deleteUser"       , rawQueryString = "?user_id=2&admin_id=4&password=1234dom", pathInfo = ["deleteUser"], queryString = [("user_id",Just "2"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest4  = reqTest1 {rawPathInfo = "/deleteUser"       , rawQueryString = "?user_id=6&admin_id=4&password=1234dom", pathInfo = ["deleteUser"], queryString = [("user_id",Just "6"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest5  = reqTest1 {rawPathInfo = "/createAdmin"      , rawQueryString = "?password=654321&first_name=Chris&last_name=Wirt&user_pic_url=https://images.pexels.com/photos/4617160/pexels-photo-4617160.jpeg?auto=compress%26cs=tinysrgb%26dpr=2%26h=650%26w=940&create_admin_key=lola", pathInfo = ["createAdmin"], queryString = [("password",Just "654321"),("first_name",Just "Chris"),("last_name",Just "Wirt"),("user_pic_url",Just "https://images.pexels.com/photos/4617160/pexels-photo-4617160.jpeg?auto=compress&cs=tinysrgb&dpr=2&h=650&w=940"),("create_admin_key",Just "lola")]}
reqTest6  = reqTest1 {rawPathInfo = "/createAuthor"     , rawQueryString = "?user_id=3&author_info=SuperAuthor&admin_id=4&password=1234dom", pathInfo = ["createAuthor"], queryString = [("user_id",Just "3"),("author_info",Just "SuperAuthor"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest7  = reqTest1 {rawPathInfo = "/getAuthor"        , rawQueryString = "?author_id=3&admin_id=4&password=1234dom", pathInfo = ["getAuthor"], queryString = [("author_id",Just "3"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest8  = reqTest1 {rawPathInfo = "/updateAuthor"     , rawQueryString = "?author_id=3&author_info=Very funny&user_id=7&admin_id=4&password=1234dom", pathInfo = ["updateAuthor"], queryString = [("author_id",Just "3"),("author_info",Just "Very funny"),("user_id",Just "7"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest9  = reqTest1 {rawPathInfo = "/deleteAuthor"     , rawQueryString = "?author_id=4&admin_id=4&password=1234dom", pathInfo = ["deleteAuthor"], queryString = [("author_id",Just "3"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest10 = reqTest1 {rawPathInfo = "/createCategory"   , rawQueryString = "?category_name=Animals", pathInfo = ["createCategory"], queryString = [("category_name",Just "Animals")]}
reqTest11 = reqTest1 {rawPathInfo = "/createCategory"   , rawQueryString = "?category_name=Juzz&admin_id=4&password=1234dom", pathInfo = ["createCategory"], queryString = [("category_name",Just "Juzz"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest12 = reqTest1 {rawPathInfo = "/createSubCategory", rawQueryString = "?category_name=France&admin_id=4&password=1234dom&super_category_id=12", pathInfo = ["createSubCategory"], queryString = [("category_name",Just "France"),("admin_id",Just "4"),("password",Just "1234dom"),("super_category_id",Just "12")]}
reqTest13 = reqTest1 {rawPathInfo = "/getCategory/14"   , rawQueryString = "", pathInfo = ["getCategory","14"], queryString = []}
reqTest14 = reqTest1 {rawPathInfo = "/updateCategory"   , rawQueryString = "?category_id=5&category_name=Games&admin_id=4&password=1234dom&super_category_id=6", pathInfo = ["updateCategory"], queryString = [("category_id",Just "5"),("category_name",Just "Games"),("admin_id",Just "4"),("password",Just "1234dom"),("super_category_id",Just "6")]}
reqTest15 = reqTest1 {rawPathInfo = "/deleteCategory"   , rawQueryString = "?category_id=11&admin_id=4&password=1234dom", pathInfo = ["deleteCategory"], queryString = [("category_id",Just "11"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest16 = reqTest1 {rawPathInfo = "/createTag"        , rawQueryString = "?tag_name=Bears&admin_id=4&password=1234dom", pathInfo = ["createTag"], queryString = [("tag_name",Just "Bears"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest17 = reqTest1 {rawPathInfo = "/getTag/12"        , rawQueryString = "", pathInfo = ["getTag","12"], queryString = []}
reqTest18 = reqTest1 {rawPathInfo = "/updateTag"        , rawQueryString = "?tag_id=7&tag_name=King&admin_id=4&password=1234dom", pathInfo = ["updateTag"], queryString = [("tag_id",Just "7"),("tag_name",Just "King"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest19 = reqTest1 {rawPathInfo = "/deleteTag"        , rawQueryString = "?tag_id=13&admin_id=4&password=1234dom", pathInfo = ["deleteTag"], queryString = [("tag_id",Just "13"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest20 = reqTest1 {rawPathInfo = "/createNewDraft"   , rawQueryString = "", pathInfo = ["createNewDraft"], queryString = []}
reqTest21 = reqTest1 {rawPathInfo = "/createPostsDraft" , rawQueryString = "?post_id=3&user_id=8&password=344los", pathInfo = ["createPostsDraft"], queryString = [("post_id",Just "3"),("user_id",Just "8"),("password",Just "344los")]}
reqTest22 = reqTest1 {rawPathInfo = "/createPostsDraft" , rawQueryString = "?post_id=3&user_id=4&password=1234dom", pathInfo = ["createPostsDraft"], queryString = [("post_id",Just "3"),("user_id",Just "4"),("password",Just "1234dom")]}
reqTest23 = reqTest1 {rawPathInfo = "/getDraft"         , rawQueryString = "?draft_id=4&user_id=8&password=344los", pathInfo = ["getDraft"], queryString = [("draft_id",Just "4"),("user_id",Just "8"),("password",Just "344los")]}
reqTest24 = reqTest1 {rawPathInfo = "/getDrafts"        , rawQueryString = "?page=1&user_id=8&password=344los", pathInfo = ["getDrafts"], queryString = [("page",Just "1"),("user_id",Just "8"),("password",Just "344los")]}
reqTest25 = reqTest1 {rawPathInfo = "/getDrafts"        , rawQueryString = "?page=2&user_id=8&password=344los", pathInfo = ["getDrafts"], queryString = [("page",Just "2"),("user_id",Just "8"),("password",Just "344los")]}
reqTest26 = reqTest1 {rawPathInfo = "/updateDraft/5"    , rawQueryString = "", pathInfo = ["updateDraft","5"], queryString = []}
reqTest27 = reqTest1 {rawPathInfo = "/deleteDraft"      , rawQueryString = "?draft_id=4&user_id=8&password=344los", pathInfo = ["deleteDraft"], queryString = [("draft_id",Just "4"),("user_id",Just "8"),("password",Just "344los")]}
reqTest28 = reqTest1 {rawPathInfo = "/publishDraft"     , rawQueryString = "?draft_id=5&user_id=8&password=344los", pathInfo = ["publishDraft"], queryString = [("draft_id",Just "5"),("user_id",Just "8"),("password",Just "344los")]}
reqTest29 = reqTest1 {rawPathInfo = "/publishDraft"     , rawQueryString = "?draft_id=4&user_id=8&password=344los", pathInfo = ["publishDraft"], queryString = [("draft_id",Just "4"),("user_id",Just "8"),("password",Just "344los")]}
reqTest30 = reqTest1 {rawPathInfo = "/getPost/2"        , rawQueryString = "", pathInfo = ["getPost","2"], queryString = []}
reqTest31 = reqTest1 {rawPathInfo = "/deletePost"       , rawQueryString = "?post_id=4&admin_id=4&password=1234dom", pathInfo = ["deletePost"], queryString = [("post_id",Just "4"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest32 = reqTest1 {rawPathInfo = "/createComment"    , rawQueryString = "?comment_text=sweet&post_id=3&user_id=8&password=344los", pathInfo = ["createComment"], queryString = [("comment_text",Just "sweet"),("post_id",Just "3"),("user_id",Just "8"),("password",Just "344los")]}
reqTest33 = reqTest1 {rawPathInfo = "/updateComment"    , rawQueryString = "?comment_id=4&comment_text=creepy&user_id=2&password=87654321", pathInfo = ["updateComment"], queryString = [("comment_id",Just "4"),("comment_text",Just "creepy"),("user_id",Just "2"),("password",Just "87654321")]}
reqTest34 = reqTest1 {rawPathInfo = "/deleteComment"    , rawQueryString = "?comment_id=4&admin_id=4&password=1234dom", pathInfo = ["deleteComment"], queryString = [("comment_id",Just "4"),("admin_id",Just "4"),("password",Just "1234dom")]}
reqTest35 = reqTest1 {rawPathInfo = "/deleteComment"    , rawQueryString = "?comment_id=4&user_id=2&password=87654321", pathInfo = ["deleteComment"], queryString = [("comment_id",Just "4"),("user_id",Just "2"),("password",Just "87654321")]}
reqTest36 = reqTest1 {rawPathInfo = "/getPosts/1"       , rawQueryString = "?category_id=16&sort_by_author=desc", pathInfo = ["getPosts","1"], queryString = [("category_id",Just "16"),("sort_by_author",Just "desc")]}


--getBodyTest1 reqTest20 = return $ "{\"user_id\":6,\"password\":\"057ccc\",\"draft_name\":\"rock\",\"draft_category_id\":3,\"draft_text\":\"heyhey\",\"draft_main_pic_url\":\"https://cdn.pixabay.com/photo/2019/09/24/16/32/chameleon-4501712_960_720.jpg\",\"draft_tags_ids\":[1,2,3],\"draft_pics_urls\":[\"https://cdn.pixabay.com/photo/2019/12/26/10/44/horse-4720178_960_720.jpg\",\"https://cdn.pixabay.com/photo/2020/08/27/19/03/zebra-5522697_960_720.jpg\"]}"
getBodyTest1 reqTest26 = return $ "{\"user_id\":6,\"password\":\"057ccc\",\"draft_name\":\"rock\",\"draft_category_id\":3,\"draft_text\":\"heyhey\",\"draft_main_pic_url\":\"https://cdn.pixabay.com/photo/2019/09/24/16/32/chameleon-4501712_960_720.jpg\",\"draft_tags_ids\":[1,2,3],\"draft_pics_urls\":[\"https://cdn.pixabay.com/photo/2019/12/26/10/44/horse-4720178_960_720.jpg\",\"https://cdn.pixabay.com/photo/2020/08/27/19/03/zebra-5522697_960_720.jpg\"]}"

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
        "{\"user_id\":11,\"first_name\":\"Kate\",\"last_name\":\"Grick\",\"user_pic_id\":11,\"user_pic_url\":\"http://localhost:3000/picture/11\",\"user_create_date\":\"2020-02-20\"}"
  describe "getUser" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest2) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]      
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest2) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe` 
        "{\"user_id\":3,\"first_name\":\"Ira\",\"last_name\":\"Medvedeva\",\"user_pic_id\":2,\"user_pic_url\":\"http://localhost:3000/picture/2\",\"user_create_date\":\"2018-03-01\"}"
  describe "deleteUser" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest3) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,UPDATEDATA,LOGMSG,EXISTCHEK,DELETEDATA,LOGMSG]      
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest3) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "deleteUser Author" $ do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest4) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,UPDATEDATA,LOGMSG,EXISTCHEK,LOGMSG,SELECTDATA,LOGMSG,UPDATEDATA,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA,DELETEDATA,LOGMSG]      
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest4) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "createAdmin" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest5) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,LOGMSG,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest5) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"user_id\":11,\"first_name\":\"Chris\",\"last_name\":\"Wirt\",\"user_pic_id\":11,\"user_pic_url\":\"http://localhost:3000/picture/11\",\"user_create_date\":\"2020-02-20\"}"
  describe "createAuthor" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest6) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,INSERTDATA,LOGMSG,LOGMSG,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest6) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"author_id\":6,\"author_info\":\"SuperAuthor\",\"user_id\":3}"    
  describe "getAuthor" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest7) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest7) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"author_id\":3,\"author_info\":\"London is the capital\",\"user_id\":6}"
  describe "updateAuthor" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest8) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,EXISTCHEK,UPDATEDATA]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest8) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"author_id\":3,\"author_info\":\"Very funny\",\"user_id\":7}"
  describe "deleteAuthor" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest9) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,UPDATEDATA,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest9) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "createCategory without admin params" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest10) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest10) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "Status 404 Not Found"
  describe "createCategory" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest11) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest11) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"category_id\":17,\"category_name\":\"Juzz\",\"sub_categories\":[],\"super_category\":\"NULL\"}"
  describe "createSubCategory" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest12) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,INSERTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest12) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"category_id\":17,\"category_name\":\"France\",\"sub_categories\":[],\"super_category\":{\"category_id\":12,\"category_name\":\"Europe\",\"sub_categories\":[13,15,17],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}}}"
  describe "getCategory" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest13) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest13) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}}"
  describe "updateCategory" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest14) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,UPDATEDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest14) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"category_id\":5,\"category_name\":\"Games\",\"sub_categories\":[9,10],\"super_category\":{\"category_id\":6,\"category_name\":\"Hobby\",\"sub_categories\":[5,7,8],\"super_category\":\"NULL\"}}"
  describe "deleteCategory" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest15) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,UPDATEDATA,UPDATEDATA,DELETEDATA]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest15) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "createTag" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest16) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest16) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"tag_id\":16,\"tag_name\":\"Bears\"}"
  describe "getTag" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest17) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest17) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"tag_id\":12,\"tag_name\":\"Medicine\"}"
  describe "updateTag" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest18) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,UPDATEDATA]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest18) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"tag_id\":7,\"tag_name\":\"King\"}"
  describe "deleteTag" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest19) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA,DELETEDATA,DELETEDATA]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest19) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "createNewDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest20) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTMANYDATA,INSERTMANYDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest20) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"draft_id\":6,\"post_id\":\"NULL\",\"author\":{\"author_id\":3,\"author_info\":\"London is the capital\",\"user_id\":6},\"draft_name\":\"rock\",\"draft_category\":{\"category_id\":3,\"category_name\":\"football\",\"sub_categories\":[],\"super_category\":{\"category_id\":2,\"category_name\":\"Sport\",\"sub_categories\":[3,4,5],\"super_category\":\"NULL\"}},\"draft_text\":\"heyhey\",\"draft_main_pic_id\":11,\"draft_main_pic_url\":\"http://localhost:3000/picture/11\",\"draft_pics\":[{\"pic_id\":12,\"pic_url\":\"http://localhost:3000/picture/12\"},{\"pic_id\":13,\"pic_url\":\"http://localhost:3000/picture/13\"}],\"draft_tags\":[]}"  
  describe "createPostsDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest21) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG]      
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest21) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"draft_id\":6,\"post_id\":3,\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"draft_name\":\"Sorry\",\"draft_category\":{\"category_id\":2,\"category_name\":\"Sport\",\"sub_categories\":[3,4,5],\"super_category\":\"NULL\"},\"draft_text\":\"some consumers argue that advertising is a bad thing. They say that advertising is bad for children. Adverts make children \226\128\152pester\226\128\153 their parents to buy things for them. Advertisers know we love our children and want to give them everything.\",\"draft_main_pic_id\":3,\"draft_main_pic_url\":\"http://localhost:3000/picture/3\",\"draft_pics\":[{\"pic_id\":7,\"pic_url\":\"http://localhost:3000/picture/7\"},{\"pic_id\":8,\"pic_url\":\"http://localhost:3000/picture/8\"}],\"draft_tags\":[{\"tag_id\":7,\"tag_name\":\"Autumn\"}]}"
  describe "createPostsDraft. User not author" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest22) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest22) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":false,\"info\":\"user_id: 4 is not author of post_id: 3\"}"
  describe "getDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest23) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest23) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"draft_id\":4,\"post_id\":3,\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"draft_name\":\"Table\",\"draft_category\":{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}},\"draft_text\":\"People say that travelling is dangerous, for example, driving a car. They point to the fact that there are so many cars on the roads that the chances of an accident are very high. But that\226\128\153s nothing compared to Space. Space will soon be so dangerous to travel in that only a mad man would even try.\",\"draft_main_pic_id\":2,\"draft_main_pic_url\":\"http://localhost:3000/picture/2\",\"draft_pics\":[{\"pic_id\":6,\"pic_url\":\"http://localhost:3000/picture/6\"},{\"pic_id\":8,\"pic_url\":\"http://localhost:3000/picture/8\"},{\"pic_id\":9,\"pic_url\":\"http://localhost:3000/picture/9\"},{\"pic_id\":10,\"pic_url\":\"http://localhost:3000/picture/10\"},{\"pic_id\":3,\"pic_url\":\"http://localhost:3000/picture/3\"}],\"draft_tags\":[{\"tag_id\":8,\"tag_name\":\"Spring\"}]}"
  describe "getDrafts page1" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest24) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTLIMITDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest24) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"page\":1,\"drafts\":[{\"draft_id\":5,\"post_id\":\"NULL\",\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"draft_name\":\"Cort\",\"draft_category\":{\"category_id\":3,\"category_name\":\"football\",\"sub_categories\":[],\"super_category\":{\"category_id\":2,\"category_name\":\"Sport\",\"sub_categories\":[3,4,5],\"super_category\":\"NULL\"}},\"draft_text\":\"The reason is simple; ever since we started exploring Space in the late 1950s we have been leaving things up there. There is now so much rubbish circling the Earth that from a distance our planet appears to have a ring around it, making it look a bit like Saturn. Unless we start cleaning up after ourselves, we are in danger.\",\"draft_main_pic_id\":3,\"draft_main_pic_url\":\"http://localhost:3000/picture/3\",\"draft_pics\":[{\"pic_id\":7,\"pic_url\":\"http://localhost:3000/picture/7\"},{\"pic_id\":6,\"pic_url\":\"http://localhost:3000/picture/6\"}],\"draft_tags\":[{\"tag_id\":4,\"tag_name\":\"Love\"},{\"tag_id\":6,\"tag_name\":\"Sommer\"},{\"tag_id\":9,\"tag_name\":\"Mondey\"}]},{\"draft_id\":4,\"post_id\":3,\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"draft_name\":\"Table\",\"draft_category\":{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}},\"draft_text\":\"People say that travelling is dangerous, for example, driving a car. They point to the fact that there are so many cars on the roads that the chances of an accident are very high. But that\226\128\153s nothing compared to Space. Space will soon be so dangerous to travel in that only a mad man would even try.\",\"draft_main_pic_id\":2,\"draft_main_pic_url\":\"http://localhost:3000/picture/2\",\"draft_pics\":[{\"pic_id\":6,\"pic_url\":\"http://localhost:3000/picture/6\"},{\"pic_id\":8,\"pic_url\":\"http://localhost:3000/picture/8\"},{\"pic_id\":9,\"pic_url\":\"http://localhost:3000/picture/9\"},{\"pic_id\":10,\"pic_url\":\"http://localhost:3000/picture/10\"},{\"pic_id\":3,\"pic_url\":\"http://localhost:3000/picture/3\"}],\"draft_tags\":[{\"tag_id\":8,\"tag_name\":\"Spring\"}]}]}"
  describe "getDrafts page2" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest25) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTLIMITDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest25) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"page\":2,\"drafts\":[]}"
  describe "updateDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest26) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTDATA,LOGMSG,DELETEDATA,DELETEDATA,UPDATEDATA,INSERTMANYDATA,INSERTMANYDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest26) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"draft_id\":5,\"post_id\":\"NULL\",\"author\":{\"author_id\":3,\"author_info\":\"London is the capital\",\"user_id\":6},\"draft_name\":\"rock\",\"draft_category\":{\"category_id\":3,\"category_name\":\"football\",\"sub_categories\":[],\"super_category\":{\"category_id\":2,\"category_name\":\"Sport\",\"sub_categories\":[3,4,5],\"super_category\":\"NULL\"}},\"draft_text\":\"heyhey\",\"draft_main_pic_id\":11,\"draft_main_pic_url\":\"http://localhost:3000/picture/11\",\"draft_pics\":[{\"pic_id\":12,\"pic_url\":\"http://localhost:3000/picture/12\"},{\"pic_id\":13,\"pic_url\":\"http://localhost:3000/picture/13\"}],\"draft_tags\":[]}"
  describe "deleteDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest27) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA,DELETEDATA,DELETEDATA]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest27) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"  
  describe "publishDraft" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest28) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,INSERTDATA,LOGMSG,INSERTMANYDATA,INSERTMANYDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest28) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"post_id\":6,\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"post_name\":\"Cort\",\"post_create_date\":\"2020-02-20\",\"post_category\":{\"category_id\":3,\"category_name\":\"football\",\"sub_categories\":[],\"super_category\":{\"category_id\":2,\"category_name\":\"Sport\",\"sub_categories\":[3,4,5],\"super_category\":\"NULL\"}},\"post_text\":\"The reason is simple; ever since we started exploring Space in the late 1950s we have been leaving things up there. There is now so much rubbish circling the Earth that from a distance our planet appears to have a ring around it, making it look a bit like Saturn. Unless we start cleaning up after ourselves, we are in danger.\",\"post_main_pic_id\":3,\"post_main_pic_url\":\"http://localhost:3000/picture/3\",\"post_pics\":[{\"pic_id\":7,\"pic_url\":\"http://localhost:3000/picture/7\"},{\"pic_id\":6,\"pic_url\":\"http://localhost:3000/picture/6\"}],\"post_tags\":[{\"tag_id\":4,\"tag_name\":\"Love\"},{\"tag_id\":6,\"tag_name\":\"Sommer\"},{\"tag_id\":9,\"tag_name\":\"Mondey\"}]}"
  describe "publishDraft  (posts draft)" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest29) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,UPDATEDATA,DELETEDATA,DELETEDATA,INSERTMANYDATA,INSERTMANYDATA,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest29) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"post_id\":3,\"author\":{\"author_id\":4,\"author_info\":\"i have a cat\",\"user_id\":8},\"post_name\":\"Table\",\"post_create_date\":\"2019-07-25\",\"post_category\":{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}},\"post_text\":\"People say that travelling is dangerous, for example, driving a car. They point to the fact that there are so many cars on the roads that the chances of an accident are very high. But that\226\128\153s nothing compared to Space. Space will soon be so dangerous to travel in that only a mad man would even try.\",\"post_main_pic_id\":2,\"post_main_pic_url\":\"http://localhost:3000/picture/2\",\"post_pics\":[{\"pic_id\":6,\"pic_url\":\"http://localhost:3000/picture/6\"},{\"pic_id\":8,\"pic_url\":\"http://localhost:3000/picture/8\"},{\"pic_id\":9,\"pic_url\":\"http://localhost:3000/picture/9\"},{\"pic_id\":10,\"pic_url\":\"http://localhost:3000/picture/10\"},{\"pic_id\":3,\"pic_url\":\"http://localhost:3000/picture/3\"}],\"post_tags\":[{\"tag_id\":8,\"tag_name\":\"Spring\"}]}"
  describe "getPost" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest30) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest30) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"post_id\":2,\"author\":{\"author_id\":2,\"author_info\":\"i don`t like it\",\"user_id\":4},\"post_name\":\"Glass\",\"post_create_date\":\"2019-06-01\",\"post_category\":{\"category_id\":16,\"category_name\":\"Egypt\",\"sub_categories\":[],\"super_category\":{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}}},\"post_text\":\"Advertising companies say advertising is necessary and important. It informs people about new products. Advertising hoardings in the street make our environment colourful. And adverts on TV are often funny. Sometimes they are mini-dramas and we wait for the next programme in the mini-drama. Advertising can educate, too. Adverts tell us about new, healthy products\",\"post_main_pic_id\":2,\"post_main_pic_url\":\"http://localhost:3000/picture/2\",\"post_pics\":[],\"post_tags\":[{\"tag_id\":1,\"tag_name\":\"Cats\"},{\"tag_id\":12,\"tag_name\":\"Medicine\"},{\"tag_id\":10,\"tag_name\":\"Home\"}]}"
  describe "deletePost" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest31) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA,DELETEDATA,DELETEDATA,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest31) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "createComment" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest32) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,INSERTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest32) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"comment_id\":11,\"comment_text\":\"sweet\",\"post_id\":3,\"user_id\":8}"
  describe "updateComment" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest33) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,UPDATEDATA,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest33) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"comment_id\":4,\"comment_text\":\"creepy\",\"post_id\":2,\"user_id\":2}"
  describe "deleteComment (admin) " $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest34) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest34) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "deleteComment (comment owner) " $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest35) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,EXISTCHEK,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,DELETEDATA]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest35) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"ok\":true}"
  describe "getPosts" $  do
    it "work" $ do
      state <- execStateT (runExceptT $ answerEx handle1 reqTest36) (testDB1,[])
      (reverse . snd $ state) `shouldBe` 
        [LOGMSG,LOGMSG,LOGMSG,SELECTLIMITDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG,LOGMSG,SELECTDATA,LOGMSG]
      ansE <- evalStateT (runExceptT $ answerEx handle1 reqTest36) (testDB1,[])
      let resInfo = fromE ansE
      (toLazyByteString . resBuilder $ resInfo) `shouldBe`
        "{\"page\":1,\"posts\":[{\"post_id\":2,\"author\":{\"author_id\":2,\"author_info\":\"i don`t like it\",\"user_id\":4},\"post_name\":\"Glass\",\"post_create_date\":\"2019-06-01\",\"post_category\":{\"category_id\":16,\"category_name\":\"Egypt\",\"sub_categories\":[],\"super_category\":{\"category_id\":14,\"category_name\":\"Africa\",\"sub_categories\":[16],\"super_category\":{\"category_id\":11,\"category_name\":\"Place\",\"sub_categories\":[12,14],\"super_category\":\"NULL\"}}},\"post_text\":\"Advertising companies say advertising is necessary and important. It informs people about new products. Advertising hoardings in the street make our environment colourful. And adverts on TV are often funny. Sometimes they are mini-dramas and we wait for the next programme in the mini-drama. Advertising can educate, too. Adverts tell us about new, healthy products\",\"post_main_pic_id\":2,\"post_main_pic_url\":\"http://localhost:3000/picture/2\",\"post_pics\":[],\"post_tags\":[{\"tag_id\":1,\"tag_name\":\"Cats\"},{\"tag_id\":12,\"tag_name\":\"Medicine\"},{\"tag_id\":10,\"tag_name\":\"Home\"}]}]}"


