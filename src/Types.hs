module Types where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.Types (Binary, In, PGArray)

type Id = Int64

type UserId = Id

type AuthorId = Id

type PostId = Id

type DraftId = Id

type CommentId = Id

type PictureId = Id

type CategoryId = Id

type SubCategoryId = Id

type SuperCatId = Id

type TagId = Id

type QueryParamKey = Text

type JsonParamKey = Text

type QueryTxtParam = Text

type Port = Int

type Table = String

type JoinTable = String

type DbKey = String

type DbReturnKey = DbKey

type Predicate = String

type ToUpdate = String

type Param = String

type ReturnParam = Param

type CheckParam = Param

type ResourceIdText = Text

type ResourceName = Text

type Page = Int

type Limit = Int

type AdminBool = Bool

type TokenKey = String

type DbTxtValue = Text

type TagName = DbTxtValue

type AuthorInfo = DbTxtValue

type AuthorName = DbTxtValue

type CatName = DbTxtValue

type CommentText = DbTxtValue

type Key = DbTxtValue

type Pwd = DbTxtValue

type SortPriority = Int

data SortOrd = ASC | DESC
  deriving (Eq, Show)

data DbValue
  = Txt Text
  | Num Int
  | IdArray (PGArray Id)
  | IdIn (In [Id])
  | Id Id
  | Day Day
  | Bool Bool
  | BS (Binary ByteString)
  | Str String

instance Show DbValue where
  show (Num a) = show a
  show (Txt a) = show a
  show (IdArray a) = show a
  show (IdIn a) = show a
  show (Id a) = show a
  show (Day a) = show a
  show (Bool a) = show a
  show (BS a) = show a
  show (Str a) = show a

instance ToField DbValue where
  toField (Num a) = toField a
  toField (Txt a) = toField a
  toField (IdArray a) = toField a
  toField (IdIn a) = toField a
  toField (Id a) = toField a
  toField (Day a) = toField a
  toField (Bool a) = toField a
  toField (BS a) = toField a
  toField (Str a) = toField a

data InsertUser
  = InsertUser Text Text Text PictureId Day AdminBool TokenKey
  deriving (Eq, Show)

data InsertDraft
  = InsertDraft (Maybe PostId) AuthorId Text CategoryId Text PictureId
  deriving (Eq, Show)

data InsertPost
  = InsertPost AuthorId Text Day CategoryId Text PictureId
  deriving (Eq, Show)

data UpdateDbDraft
  = UpdateDbDraft Text CategoryId Text PictureId
  deriving (Eq, Show)

data UpdateDbPost
  = UpdateDbPost Text CategoryId Text PictureId
  deriving (Eq, Show)
