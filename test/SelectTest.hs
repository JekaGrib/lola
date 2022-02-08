--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE FlexibleInstances #-}


module SelectTest where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text)
import           Data.Time.Calendar             ( Day)
import Methods.Handle.Select 




class (Select a) => FromSelectTest a where
  fromSelTest :: SelectTest -> a



data SelectTest =
  OnlyInt Integer
  | OnlyDay Day
  | OnlyTxt Text
  | OnlyBinary (Binary ByteString)
  | TwoIdsTest Integer Integer
  | AuthTest Text  Bool
  | CatTest Text Integer
  | TagTest Integer Text
  | AuthorTest  Integer Text Integer
  | CommentTest Integer Integer Text
  | UserTest Text Text Integer Day
  | PostInfoTest Integer Text Text Integer Text Integer
  | DraftTest Integer Text Integer Text Integer Text Integer
  | PostTest  Integer Integer Text Integer Text Day Integer Text Integer


instance FromSelectTest (Only Integer) where
  fromSelTest (OnlyInt x) = Only x

instance FromSelectTest (Only Day) where
  fromSelTest (OnlyDay x) = Only x

instance FromSelectTest (Only Text) where
  fromSelTest (OnlyTxt x) = Only x
 
instance FromSelectTest (Only (Binary ByteString)) where
  fromSelTest (OnlyBinary bb) = Only bb



    
instance FromSelectTest TwoIds where
  fromSelTest (TwoIdsTest a b) = TwoIds a b
  


instance FromSelectTest Auth where
  fromSelTest (AuthTest a b) = Auth a b
  



instance FromSelectTest Cat where
  fromSelTest (CatTest a b) = Cat a b
  


instance FromSelectTest Tag where
  fromSelTest (TagTest a b) = Tag a b
  



instance FromSelectTest Author where
  fromSelTest (AuthorTest a b c) = Author a b c
  



instance FromSelectTest Comment where
  fromSelTest (CommentTest a b c) = Comment a b c
  


instance FromSelectTest User where
  fromSelTest (UserTest a b c d) = User a b c d
  


instance FromSelectTest PostInfo where
  fromSelTest (PostInfoTest a b c d e f) = PostInfo a b c d e f
  




instance FromSelectTest Draft where
  fromSelTest (DraftTest a b c d e f g) = Draft a b c d e f g
  

 

instance FromSelectTest Post where
  fromSelTest (PostTest a b c d e f g h i) = Post a b c d e f g h i
  

















    

