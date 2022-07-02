module Spec.Types where

import Logger
import Spec.Admin.Types (AdminMock)
import Spec.Auth.Types (AuthMock)
import Spec.Author.Types (AuthorMock)
import Spec.Category.Types (CatMock)
import Spec.Comment.Types (CommMock)
import Spec.DeleteMany.Types (DeleteManyMock)
import Spec.Draft.Types (DraftMock)
import Spec.Exist.Types (ExistMock)
import Spec.MakeCatResp.Types (MakeCatRMock)
import Spec.Picture.Types (PicMock)
import Spec.Post.Types (PostMock)
import Spec.Tag.Types (TagMock)
import Spec.User.Types (UserMock)

data MockAction
  = LOG Priority
  | TRANSACTIONOPEN
  | TRANSACTIONCLOSE
  | GETDay
  | AdminMock AdminMock
  | AuthMock AuthMock
  | AuthorMock AuthorMock
  | CatMock CatMock
  | CommMock CommMock
  | DeleteManyMock DeleteManyMock
  | DraftMock DraftMock
  | ExistMock ExistMock
  | MakeCatRMock MakeCatRMock
  | PicMock PicMock
  | PostMock PostMock
  | TagMock TagMock
  | UserMock UserMock
  deriving (Eq, Show)
