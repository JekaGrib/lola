{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Exist.Types where


import Methods.Common.Exist.UncheckedExId (UncheckedExId)


newtype ExistMock = IsExist UncheckedExId
  deriving (Eq, Show)



