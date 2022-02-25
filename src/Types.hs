{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Types where

import Data.Text (Text)
--import Data.Map as Map
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.Types (PGArray,In)
import Data.Int (Int64)
import Data.Time.Calendar ( Day)


type Id = Int64


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

type DbKey = String

type Predicate = String

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

data DbValue = Txt Text | Num Integer | IdArray (PGArray Id) | IdIn (In [Id]) | Id Id | Day Day | Bool Bool

instance Show DbValue where
  show (Num a) = show a
  show (Txt a) = show a
  show (IdArray a) = show a
  show (IdIn a) = show a
  show (Id a) = show a
  show (Day a) = show a
  show (Bool a) = show a





instance ToField DbValue where
  toField (Num a) = toField a
  toField (Txt a) = toField a
  toField (IdArray a) = toField a
  toField (IdIn a) = toField a
  toField (Id a) = toField a
  toField (Day a) = toField a
  toField (Bool a) = toField a



 

type WhereValue = (Where, DbValue)

data AndWhereVal = AndWhereVal WhereValue | AndOrWhere [OrWhereValue]

newtype OrWhereValue = OrWhereValue WhereValue

--type WhereValueS = WhereValue :| [AndWhereVal]
