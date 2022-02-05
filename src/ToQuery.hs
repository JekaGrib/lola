{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}




module ToQuery (toSelQ,toSelLimQ,toUpdQ,toDelQ,toExQ,toInsRetQ,toInsManyQ) where
          

import           Types

import           Database.PostgreSQL.Simple (Query)

import           Data.String                    ( fromString )
import           Data.List                      ( intercalate )




toSelQ :: Table -> [Param] -> Where -> Query
toSelQ table params where' = 
  fromString $ "SELECT " ++ intercalate ", " params ++ " FROM " ++ table ++ " WHERE " ++ where'

toSelLimQ :: Table -> OrderBy -> Page -> Limit -> [Param] -> Where -> Query
toSelLimQ table orderBy page limitNumber params where' = 
  fromString $ "SELECT " ++ intercalate ", " params ++ " FROM " ++ table ++ " WHERE " ++ where' ++ " ORDER BY " ++ orderBy ++ " OFFSET " ++ show ((page -1)*limitNumber) ++ " LIMIT " ++ show (page*limitNumber)

toUpdQ :: Table -> Set -> Where -> Query
toUpdQ table set where' = 
  fromString $ "UPDATE " ++ table ++ " SET " ++ set ++ " WHERE " ++ where' 

toDelQ :: Table -> Where -> Query
toDelQ table where' =
  fromString $ "DELETE FROM " ++ table ++ " WHERE " ++ where'

toExQ :: Table -> CheckParam -> Where -> Query
toExQ table checkName where' =
  fromString $ "SELECT EXISTS (SELECT " ++ checkName ++ " FROM " ++ table ++ " WHERE " ++ where' ++ ")"

toInsRetQ :: Table -> ReturnParam -> [Param] -> Query
toInsRetQ table returnName insNames =
  fromString $ "INSERT INTO " ++ table ++ " ( " ++ intercalate "," insNames ++ " ) VALUES ( " ++ (intercalate "," . fmap (const "?") $ insNames) ++ " ) RETURNING " ++ returnName

toInsManyQ :: Table -> [Param] -> Query
toInsManyQ table insNames =
  fromString $ "INSERT INTO " ++ table ++ " ( " ++ intercalate "," insNames ++ " ) VALUES ( " ++ (intercalate "," . fmap (const "?") $ insNames) ++ " ) "
