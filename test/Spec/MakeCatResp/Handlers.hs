{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -Werror #-}

module Spec.MakeCatResp.Handlers where

import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT (..), modify)
import Database.PostgreSQL.Simple (ExecStatus (FatalError), SqlError (..))
import Methods.Common.MakeCatResp
import qualified Spec.Auth.Handlers (handle0)
import Spec.Conf (defConf)
import qualified Spec.Exist.Handlers (handle)
import Spec.Log (handLogWarning)
import Spec.MakeCatResp.Types
import Spec.Types (MockAction (..))
import Types
import Oops (UnexpectedDbOutPutException(..))
import Psql.Selecty (Cat(..))


handle :: Handle (StateT [MockAction] IO)
handle =
  Handle
    defConf
    handLogWarning
    selectCatsTest
    selectSubCatsTest
    

selectCatsTest :: CategoryId -> StateT [MockAction] IO [Cat]
selectCatsTest catId = do
  modify (MakeCatRMock (SelectCats catId) :)
  if (catId <= 20) 
    then return $ map toCat . filter ((== catId) . cat_idCat) $ exampleCats
    else return $ [Cat "u" 20]

selectSubCatsTest :: CategoryId -> StateT [MockAction] IO [SubCategoryId]
selectSubCatsTest catId = do
  modify (MakeCatRMock (SelectSubCats catId) :)
  return $ map cat_idCat . filter ((== catId) . super_cat_idCat) $ exampleCats

data Category = Category {cat_idCat :: CategoryId, cat_nameCat :: CatName, super_cat_idCat :: SuperCatId}

exampleCats :: [Category]
exampleCats =
  [ Category 1  "a" 0
  , Category 2  "b" 0
  , Category 3  "c" 0
  , Category 4  "d" 1
  , Category 5  "e" 1
  , Category 6  "f" 1
  , Category 7  "g" 2
  , Category 8  "h" 2
  , Category 9  "i" 3
  , Category 10 "j" 3
  , Category 11 "k" 4
  , Category 12 "l" 4
  , Category 13 "m" 6
  , Category 14 "n" 8
  , Category 15 "o" 9
  , Category 16 "p" 12
  , Category 17 "q" 12
  , Category 18 "r" 14
  , Category 19 "s" 15
  , Category 20 "t" 15
  ]

toCat :: Category -> Cat
toCat (Category _ a b) =  Cat a b

