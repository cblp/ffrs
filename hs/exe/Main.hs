{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Applicative ((<|>))
import           Control.Exception (tryJust)
import           Data.Foldable (for_)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Database.Persist.Sqlite (Entity, runMigration, runSqlite,
                                          selectList)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           Database.Sqlite (Error (ErrorCan'tOpen), SqliteException (..))
import           Options.Applicative (ParserInfo, command, execParser, fullDesc,
                                      header, help, helper, hsubparser, info,
                                      metavar, progDesc, strArgument)
import           System.Environment.XDG.BaseDir (getUserDataDir)
import           System.FilePath ((</>))

programDescription :: Text
programDescription = "ff - note taker and task manager"

data Options = Add Text | List
    deriving Show

optionsParser :: ParserInfo Options
optionsParser =
    info (helper <*> programOptions) $
        fullDesc <> progDesc "ff - description" <> header "ff - header"
  where
    programOptions = hsubparser (addCommand <> listCommand) <|> listOptions
    addCommand  = command "add"   (info addOptions  (progDesc "Add a note"))
    listCommand = command "list"  (info listOptions (progDesc "List notes"))
    addOptions  = Add . Text.pack <$> strArgument (metavar "TEXT" <> help "note text")
    listOptions = pure List

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
        Note
            text Text
            deriving Show
    |]

main :: IO ()
main = do
    options <- execParser optionsParser
    print options

    dataDir <- getUserDataDir "ff"
    let dbFile = dataDir </> "notes.sql"
    notes :: [Entity Note] <-
        fmap fromEither $
        tryJust handleDbFileAbsence $
        runSqlite (Text.pack dbFile) $ do
            runMigration migrateAll
            selectList [] []
    for_ notes print

handleDbFileAbsence :: SqliteException -> Maybe [a]
handleDbFileAbsence = \case
    SqliteException{seError = ErrorCan'tOpen} -> Just []
    _ -> Nothing

fromEither :: Either a a -> a
fromEither = either id id
