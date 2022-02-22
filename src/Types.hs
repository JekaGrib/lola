{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Types where

import Data.Text (Text)
--import Data.Map as Map
import Database.PostgreSQL.Simple.ToField (ToField(..))

type Id = Integer

type UserId = Id

type AuthorId = Id

type PostId = Id

type DraftId = Id

type CommentId = Id

type PictureId = Id

type CategoryId = Id

type TagId = Id

type QueryParamKey = Text

type JsonParamKey = Text

type QueryTxtParam = Text

type Port = Int

type Table = String

type DbParamKey = String

type DbReturnParamKey = DbParamKey

type DbCheckParamKey = DbParamKey

type DbSelectParamKey = DbParamKey

type DbInsertParamKey = DbParamKey

type DbParamValue = Text

type DbNumValue = Integer

type ToUpdate = String

type Param = String

type ReturnParam = Param

type CheckParam = Param

type Where = String

type Set = String

type OrderBy = String

type Page = Int

type Limit = Int

--type WhereMap = Map.Map DbParamKey DbParamValue

data DbValue = Txt Text | Num Integer

instance Show DbValue where
  show (Num a) = show a
  show (Txt a) = show a

instance ToField DbValue where
  toField (Num a) = toField a
  toField (Txt a) = toField a
  

type WhereValue = (Where, DbValue)

data AndWhereVal = AndWhereVal WhereValue | AndOrWhere [OrWhereValue]

newtype OrWhereValue = OrWhereValue WhereValue

--type WhereValueS = WhereValue :| [AndWhereVal]
