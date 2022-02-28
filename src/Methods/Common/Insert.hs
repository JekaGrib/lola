{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Methods.Common.Selecty where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Types

data InsertUser =
  InsertUser Text Text Text PictureId Day AdminBool TokenKey
