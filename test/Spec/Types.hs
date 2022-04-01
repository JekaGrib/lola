{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Types where

import Logger
import Spec.Auth.Types (AuthMock)
import Spec.Author.Types (AuthorMock)
import Spec.Exist.Types (ExistMock)
import Spec.Tag.Types (TagMock)
import Spec.Picture.Types (PicMock)
import Spec.Post.Types (PostMock)
import Spec.User.Types (UserMock)
import Spec.DeleteMany.Types (DeleteManyMock)
import Spec.MakeCatResp.Types (MakeCatRMock)
import Spec.Category.Types (CatMock)
import Spec.Comment.Types (CommMock)
import Spec.Draft.Types (DraftMock)


data MockAction
  = LOG Priority
  | TRANSACTIONOPEN
  | TRANSACTIONCLOSE
  | GETDay
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


