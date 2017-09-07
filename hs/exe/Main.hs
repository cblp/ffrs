{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Applicative ((<|>))
import           Control.Exception (tryJust)
import           Control.Monad (when)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Foldable (for_)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Database.Persist.Sqlite (Entity (..), SqlBackend, insert,
                                          runMigration, runSqlite, selectList)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           Database.Sqlite (Error (ErrorCan'tOpen), SqliteException (..))
import           Options.Applicative (ParserInfo, command, execParser, fullDesc,
                                      header, help, helper, hsubparser, info,
                                      metavar, progDesc, strArgument)
import           System.Directory (createDirectoryIfMissing)
import           System.Environment.XDG.BaseDir (getUserDataDir)
import           System.FilePath (takeDirectory, (</>))

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
    addOptions  =
        Add . Text.pack <$> strArgument (metavar "TEXT" <> help "note text")
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

    dataDir <- getUserDataDir "ff"
    let dbFile = dataDir </> "notes.sql"

    case options of
        Add text -> do
            let note = Note text
            key <- runDB (CreateDbFile True) dbFile $ insert note
            printNote $ Entity key note
        List -> do
            notes <-
                fmap fromEither $
                tryJust handleDbFileAbsence $
                runDB (CreateDbFile False) dbFile $
                selectList [] []
            for_ notes printNote

newtype CreateDbFile = CreateDbFile Bool

runDB
    :: CreateDbFile
    -> FilePath
    -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a
    -> IO a
runDB (CreateDbFile createDbFile) dbFile action = do
    when createDbFile $
        createDirectoryIfMissing True $ takeDirectory dbFile
    runSqlite (Text.pack dbFile) $ do
        runMigration migrateAll
        action

handleDbFileAbsence :: SqliteException -> Maybe [a]
handleDbFileAbsence = \case
    SqliteException{seError = ErrorCan'tOpen} -> Just []
    _ -> Nothing

fromEither :: Either a a -> a
fromEither = either id id

printNote :: Entity Note -> IO ()
printNote Entity{entityKey = NoteKey key, entityVal = Note text} =
    Text.putStrLn $ tshow (toInteger key) <> ". " <> text

tshow :: Show a => a -> Text
tshow = Text.pack . show
