module Psql.Methods.Category where

import Database.PostgreSQL.Simple (Connection)
import Psql.Methods.Common
import Psql.ToQuery.Delete (Delete (..))
import Psql.ToQuery.Insert (InsertPair (..), InsertRet (..))
import Psql.ToQuery.Select (Where (..))
import Psql.ToQuery.Update (Set (..), Update (..))
import Types

updateDbCat' :: Connection -> CatName -> CategoryId -> IO ()
updateDbCat' conn catName catId = do
  let set = SetPair "category_name=?" (Txt catName)
  let wh = WherePair "category_id=?" (Id catId)
  updateInDb' conn (Update "categories" [set] wh)

updateDbSubCat' :: Connection -> CatName -> SuperCatId -> CategoryId -> IO ()
updateDbSubCat' conn catName superCatId catId = do
  let setName = SetPair "category_name=?" (Txt catName)
      setCat = SetPair "super_category_id=?" (Id superCatId)
      wh = WherePair "category_id=?" (Id catId)
  updateInDb' conn (Update "categories" [setName, setCat] wh)

updateDbCatsForPosts' :: Connection -> CategoryId -> [CategoryId] -> IO ()
updateDbCatsForPosts' conn newCatId catIds = do
  let set = SetPair "post_category_id=?" (Id newCatId)
  let toWhPair catId = WherePair "post_category_id=?" (Id catId)
  let wh = WhereOr $ map toWhPair catIds
  updateInDb' conn (Update "posts" [set] wh)

updateDbCatsForDrafts' :: Connection -> CategoryId -> [CategoryId] -> IO ()
updateDbCatsForDrafts' conn newCatId catIds = do
  let set = SetPair "draft_category_id=?" (Id newCatId)
  let toWhPair catId = WherePair "draft_category_id=?" (Id catId)
  let wh = WhereOr $ map toWhPair catIds
  updateInDb' conn (Update "drafts" [set] wh)

deleteDbCats' :: Connection -> [CategoryId] -> IO ()
deleteDbCats' conn catIds = do
  let toWhPair catId = WherePair "category_id=?" (Id catId)
  let wh = WhereOr $ map toWhPair catIds
  deleteFromDb' conn (Delete "categories" wh)

insertReturnCat' :: Connection -> CatName -> IO CategoryId
insertReturnCat' conn catName = do
  let insPair = InsertPair "category_name" (Txt catName)
  insertReturn' conn (InsertRet "categories" [insPair] "category_id")

insertReturnSubCat' :: Connection -> CatName -> SuperCatId -> IO CategoryId
insertReturnSubCat' conn catName superCatId = do
  let insPairName = InsertPair "category_name" (Txt catName)
  let insPairCat = InsertPair "super_category_id" (Id superCatId)
  insertReturn' conn (InsertRet "categories" [insPairName, insPairCat] "category_id")
