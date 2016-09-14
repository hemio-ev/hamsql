-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.Option where

import Data.Semigroup
import Options.Applicative

-- helper functions
boolFlag :: Mod FlagFields Bool -> Parser Bool
boolFlag = flag False True

val xs = value xs <> metavar ("\"" ++ xs ++ "\"")

-- Global
parserInfoHamsql :: ParserInfo Command
parserInfoHamsql =
  info
    (helper <*> parserCommand)
    (fullDesc <> progDesc "A YamSql interpreter and smart executor." <>
     header "hamsql - YamSql interperter written in Haskell")

-- Command
data Command
  = Install OptCommon
            OptCommonDb
            OptInstall
  | Upgrade OptCommon
            OptCommonDb
            OptUpgrade
  | Doc OptCommon
        OptDoc
  deriving (Show)

parserCommand :: Parser Command
parserCommand =
  subparser
    (command
       "install"
       (info
          (parserCmdInstall <**> helper)
          (progDesc "Installs the setup on a database from scratch.")) <>
     command
       "upgrade"
       (info
          (parserCmdUpgrade <**> helper)
          (progDesc "Upgrades an existing setup on a database.")) <>
     command
       "doc"
       (info
          (parserCmdDoc <**> helper)
          (progDesc "Produces a documentation of the setup.")))

parserCmdInstall :: Parser Command
parserCmdInstall = Install <$> parserOptCommon <*> parserOptCommonDb <*> parserOptInstall

parserCmdUpgrade :: Parser Command
parserCmdUpgrade = Upgrade <$> parserOptCommon <*> parserOptCommonDb <*> parserOptUpgrade

parserCmdDoc :: Parser Command
parserCmdDoc = Doc <$> parserOptCommon <*> parserOptDoc

-- Commons
data OptCommon = OptCommon
  { optSetup :: FilePath
  , optVerbose :: Bool
  } deriving (Show)

parserOptCommon :: Parser OptCommon
parserOptCommon =
  OptCommon <$>
  strOption
    (long "setup" <> short 's' <> help "Setup file (yaml)" <> val "setup.yml" <>
     action "file -X '!*.yml'" <>
     action "directory") <*>
  boolFlag (long "verbose" <> short 'v' <> help "Verbose")

-- Commons Execute
data OptCommonDb = OptCommonDb
  { optEmulate :: Bool
  , optPrint :: Bool
  , optConnection :: String
  } deriving (Show)

parserOptCommonDb :: Parser OptCommonDb
parserOptCommonDb =
  OptCommonDb <$>
  boolFlag (long "emulate" <> short 'e' <> help "Perform changes but rollback") <*>
  boolFlag
    (long "print" <> short 'p' <> help "Print SQL code instead of executing") <*>
  strOption (long "connection" <> short 'c' <> val "postgresql://")

-- Command Install
data OptInstall = OptInstall
  { optDeleteExistingDatabase :: Bool
  } deriving (Show)

parserOptInstall :: Parser OptInstall
parserOptInstall =
  OptInstall <$>
  boolFlag
    (long "delete-existing-database" <> short 'd' <>
     help "Delete database if it allready exists")

-- Command Upgrade
data OptUpgrade = OptUpgrade
  { optPermitDataDeletion :: Bool
  } deriving (Show)

parserOptUpgrade :: Parser OptUpgrade
parserOptUpgrade =
  OptUpgrade <$>
  boolFlag
    (long "perimit-data-deletion" <> help "Permit deletion of columns or tables")

-- Command Doc
data OptDoc = OptDoc
  { optOutputDir :: FilePath
  , optTemplate :: FilePath
  } deriving (Show)

parserOptDoc :: Parser OptDoc
parserOptDoc =
  OptDoc <$>
  strOption (long "output-dir" <> short 'o' <> val "doc/" <> action "directory") <*>
  strOption
    (long "template" <> short 't' <> val "DEFAULT.rst" <>
     action "file -X '!*.html'" <>
     action "file -X '!*.md'" <>
     action "file -X '!*.rst'" <>
     action "directory")
