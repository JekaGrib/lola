{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Auth.Types where


import Types

newtype AuthMock = SelectTokenKeyForUser UserId

  deriving (Eq, Show)



