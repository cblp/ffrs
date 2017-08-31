{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Exception (tryJust)
import           Data.Foldable (for_)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Database.Persist.Sqlite (Entity, runMigration, runSqlite,
                                          selectList)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           Database.Sqlite (Error (ErrorCan'tOpen), SqliteException (..))
import           System.Environment.XDG.BaseDir (getUserDataDir)
import           System.FilePath ((</>))

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
        Note
            text Text
            deriving Show
    |]

main :: IO ()
main = do
    dataDir <- getUserDataDir "ff"
    let dbFile = dataDir </> "notes.sql"
    notes <-
        fmap fromEither $
        tryJust handleDbFileAbsence $
        runSqlite (Text.pack dbFile) $ do
            runMigration migrateAll
            selectList [] []
    for_ (notes :: [Entity Note]) print

handleDbFileAbsence :: SqliteException -> Maybe [a]
handleDbFileAbsence = \case
    SqliteException{seError = ErrorCan'tOpen} -> Just []
    _ -> Nothing

fromEither :: Either a a -> a
fromEither = either id id
