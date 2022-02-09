{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}





module Methods.Post where
          
import           Api.Response (PostResponse(..), OkResponse(..),PostsResponse(..),AuthorResponse(..))
import           Logger
import           Types
import           Oops
import           Methods.Common
import Methods.Common.Select (Post(..),Tag)
import Methods.Common.MakeCatResp (makeCatResp)
import qualified Methods.Common.MakeCatResp (Handle,makeH)
import ParseQueryStr (DeletePost(..))
import           Network.Wai (Request)
import           Data.Text                      ( pack,Text )
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Catch            ( MonadCatch)
import Methods.Post.LimitArg (chooseArgs, isDateASC,LimitArg(..), FilterArg(..), SortArg(..))
import           Data.List                      ( zip4 )
import           Data.Time.Calendar             ( showGregorian)
import  Conf (Config(..),extractConn)
import           Database.PostgreSQL.Simple (withTransaction)
import Methods.Common.DeleteMany ( deleteAllAboutPost)
import qualified Methods.Common.DeleteMany (Handle,makeH)




data Handle m = Handle 
  { hConf              :: Config,
    hLog               :: LogHandle m ,
    selectNum          :: Table -> [Param] -> Where -> [Text] -> m [Id],
    selectTag          :: Table -> [Param] -> Where -> [Text] -> m [Tag],
    selectPost         :: Table -> [Param] -> Where -> [Text] -> m [Post],
    selectLimitPost    :: Table -> String -> Integer -> Integer -> [String] -> String -> [Text] -> [FilterArg] -> [SortArg] -> m [Post],
    updateInDb         :: Table -> String -> String -> [Text] -> m (),
    deleteFromDb       :: Table -> String -> [Text] -> m (),
    isExistInDb        :: Table -> String -> String -> [Text] -> m Bool,
    withTransactionDB  :: forall a. m a -> m a,
    hCatResp :: Methods.Common.MakeCatResp.Handle m,
    hDelMany :: Methods.Common.DeleteMany.Handle m
    }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH = let conn = extractConn conf in
  Handle 
    conf 
    logH 
    (selectOnly' conn) 
    (select' conn)  
    (select' conn) 
    (selectLimit' conn) 
    (updateInDb' conn) 
    (deleteFromDb' conn) 
    (isExistInDb' conn) 
    (withTransaction conn)
    (Methods.Common.MakeCatResp.makeH conf logH)
    (Methods.Common.DeleteMany.makeH conf )


  
getPost :: (MonadCatch m) => Handle m -> PostId -> ExceptT ReqError m ResponseInfo 
getPost h postIdNum = do
  let postIdParam = numToTxt postIdNum
  Post pId auId auInfo usId pName pDate pCatId pText picId <- checkOneIfExistE (hLog h) (selectPost h) "posts JOIN authors ON authors.author_id = posts.author_id " ["posts.post_id","posts.author_id","author_info","user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"] "post_id=?" postIdParam 
  picsIds <- checkListE (hLog h) $ selectNum h "postspics" ["pic_id"] "post_id=?" [postIdParam] 
  tagS <- checkListE (hLog h) $ selectTag h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?" [postIdParam] 
  catResp <- makeCatResp (hCatResp h) pCatId
  lift $ logInfo (hLog h) $ "Post_id: " ++ show pId ++ " sending in response" 
  okHelper $ PostResponse {post_id = pId, author4 = AuthorResponse auId auInfo usId, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl picsIds, post_tags = fmap inTagResp tagS}

getPosts :: (MonadCatch m) => Handle m -> Request -> Integer -> ExceptT ReqError m ResponseInfo 
getPosts h req pageNum = do
  let extractParams = ["posts.post_id","posts.author_id","author_info","authors.user_id","post_name","post_create_date","post_category_id","post_text","post_main_pic_id"]
  LimitArg filterArgs sortArgs <- chooseArgs req 
  let defTable = "posts JOIN authors ON authors.author_id = posts.author_id"
  let defOrderBy = if isDateASC sortArgs then "post_create_date ASC, post_id ASC" else "post_create_date DESC, post_id DESC"
  let defWhere = "true"
  let defValues = []
  posts <- checkListE (hLog h) $ selectLimitPost h defTable defOrderBy pageNum (cPostsLimit . hConf $ h) extractParams defWhere defValues filterArgs sortArgs 
  let postIdsText = fmap (pack . show . post_idP) posts
  let postCatsIds = fmap post_cat_idP posts 
  manyCatResp <- mapM (makeCatResp (hCatResp h)) postCatsIds
  manyPostPicsIds <- mapM (checkListE (hLog h) . selectNum h "postspics" ["pic_id"] "post_id=?") $ fmap (:[]) postIdsText  
  tagSMany <- mapM (checkListE (hLog h) . selectTag h "poststags AS pt JOIN tags ON pt.tag_id=tags.tag_id" ["tags.tag_id","tag_name"] "post_id=?") $ fmap (:[]) postIdsText  
  let allParams = zip4 posts manyCatResp manyPostPicsIds tagSMany
  lift $ logInfo (hLog h) $ "Post_ids: " ++ show (fmap post_idP posts) ++ " sending in response" 
  okHelper $ PostsResponse {page10 = pageNum , posts10 = fmap (\((Post pId auId auInfo usId pName pDate _ pText picId),catResp,pics,tagS) -> PostResponse {post_id = pId, author4 = AuthorResponse auId auInfo usId, post_name = pName , post_create_date = pack . showGregorian $ pDate, post_cat = catResp, post_text = pText, post_main_pic_id = picId, post_main_pic_url = makeMyPicUrl picId, post_pics = fmap inPicIdUrl pics, post_tags = fmap inTagResp tagS}) allParams}

deletePost :: (MonadCatch m) => Handle m -> DeletePost -> ExceptT ReqError m ResponseInfo 
deletePost h (DeletePost postIdNum) = do
  let postIdParam = numToTxt postIdNum
  isExistInDbE h "posts" "post_id" "post_id=?" [postIdParam] 
  withTransactionDBE h $ deleteAllAboutPost (hDelMany h) postIdNum
  lift $ logInfo (hLog h) $ "Post_id: " ++ show postIdNum ++ " deleted" 
  okHelper $ OkResponse { ok = True }


isExistInDbE :: (MonadCatch m) => Handle m  -> String -> String -> String -> [Text] -> ExceptT ReqError m ()
isExistInDbE h = checkIsExistE (hLog h) (isExistInDb h)

withTransactionDBE :: (MonadCatch m) => Handle m  -> m a -> ExceptT ReqError m a
withTransactionDBE h = checkTransactE (hLog h) . withTransactionDB h