{-# LANGUAGE OverloadedStrings #-}

module Psql.Migration where

import Control.Exception (SomeException, catch, throw)
import Database.PostgreSQL.Simple (Connection, execute_, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory, MigrationInitialization, MigrationScript, MigrationValidation), MigrationContext (MigrationContext), MigrationResult (..), runMigration, runMigrations)
import Oops (MigrationException (..))

data Migrate = Migrate | NotMigrate
  deriving (Eq, Show)

migrate :: Connection -> IO ()
migrate conn = catchMigrationEx $ do
  result <- withTransaction conn $ runMigrations False conn cmds
  case result of
    MigrationError err -> do
      dropTableShemaMigrations conn
      throw $ MigrationException err
    _ -> dropTableShemaMigrations conn
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
