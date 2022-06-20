module Psql.Migration where

import Control.Exception (SomeException, catch, throw)
import Control.Exception.Safe (throwString)
import Data.Char (toLower)
import Data.List (isPrefixOf, sort)
import Database.PostgreSQL.Simple (Connection, execute_, withTransaction)
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand
      ( MigrationDirectory,
        MigrationInitialization
      ),
    MigrationResult (..),
    runMigrations,
  )
import Error (MigrationException (..))
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)

data Migrate = Migrate | NotMigrate
  deriving (Eq, Show)

migrate :: Connection -> IO ()
migrate conn = catchMigrationEx $ do
  result <- withTransaction conn $ runMigrations False conn cmds
  case result of
    MigrationError err -> do
      dropTableShemaMigrations conn
      throw $ MigrationException err
    _ -> do
      scriptsInDirectory "./migrations" >>= mapM_ (putStrLn . ("Execute migrations: " ++))
      dropTableShemaMigrations conn
  where
    cmds =
      [ MigrationInitialization,
        MigrationDirectory "./migrations"
      ]

dropTableShemaMigrations :: Connection -> IO ()
dropTableShemaMigrations conn = catchMigrationEx $ do
  _ <- execute_ conn "DROP TABLE schema_migrations"
  return ()

catchMigrationEx :: IO () -> IO ()
catchMigrationEx m = m `catch` (\e -> throw $ MigrationException (show (e :: SomeException)))

readMigrateArg :: IO Migrate
readMigrateArg = do
  args <- getArgs
  case map parseArg args of
    (MigrateArg : _) -> return Migrate
    [] -> return NotMigrate
    (UnknownArg arg : _) -> throwString $ unKnownArgErrMsg arg

unKnownArgErrMsg :: String -> String
unKnownArgErrMsg str =
  "Error. Unknown argument in program's command line: \"" ++ str
    ++ "\".\nIf you want execute migrations, use argument - <migrate>"

data Arg = MigrateArg | UnknownArg String

parseArg :: String -> Arg
parseArg arg = case map toLower arg of
  "migrate" -> MigrateArg
  _ -> UnknownArg arg

scriptsInDirectory :: FilePath -> IO [String]
scriptsInDirectory dir =
  fmap
    (sort . filter (\x -> not $ "." `isPrefixOf` x))
    (getDirectoryContents dir)
