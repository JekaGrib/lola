--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module TestDB where


import           Data.Text                      ( Text)
import           Data.Time.Calendar             ( Day, fromGregorian )


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

emptyDB :: TestDB
emptyDB = TestDB [] [] [] [] [] [] [] [] [] [] [] [] []

picsL1,picsL2,picsL3,picsL4,picsL5,picsL6,picsL7,picsL8,picsL9,picsL10 :: PicsL
picsL1  = PicsL 1  "picture1"
picsL2  = PicsL 2  "picture2"
picsL3  = PicsL 3  "picture3"
picsL4  = PicsL 4  "picture4"
picsL5  = PicsL 5  "picture5"
picsL6  = PicsL 6  "picture6"
picsL7  = PicsL 7  "picture7"
picsL8  = PicsL 8  "picture8"
picsL9  = PicsL 9  "picture9"
picsL10 = PicsL 10 "picture10"

picsT1 :: PicsT
picsT1 = [picsL1,picsL2,picsL3,picsL4,picsL5,picsL6,picsL7,picsL8,picsL9,picsL10]

keyT1 :: KeyT
keyT1 = ["lola"]

fG = fromGregorian

usersL1,usersL2,usersL3,usersL4,usersL5,usersL6,usersL7,usersL8,usersL9,usersL10 :: UsersL
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
txt5 = "Im having a great time here in Sydney. The different sports are exciting, and there are lots of other exciting things too. For example the mascots are really great! They are called Olly, Syd and Millie. They are Australian ‘ animals and they are the symbols of the Sydney Games. The kookaburra is an Australian bird. She got her name, Olly, from the word ‘Olympics’. She’s a symbol of friendship and honesty. Then there’s Syd (from Sydney). He’s a platypus with a duck’s nose."


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

--testDB1 = emptyDB {picsT = picsT1,usersT = usersT1}
testDB1 = TestDB picsT1 keyT1 usersT1 authorsT1 tagsT1 catsT1 postsT1 commentsT1 postsPicsT1 postsTagsT1 draftsT1 draftsPicsT1 draftsTagsT1

