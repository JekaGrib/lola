--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}




module Conf where

import ConnectDB (ConnDB)
import Types
          


data Config = Config 
  { cConnDB      :: ConnDB,    
    cDefPicId    :: Integer,
    cDefUsId     :: UserId,
    cDefAuthId   :: Integer,
    cDefCatId    :: Integer,
    cCommLimit   :: Integer,
    cDraftsLimit :: Integer,
    cPostsLimit  :: Integer
    }

