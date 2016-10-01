-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.Option where

import Control.Monad.Trans.Reader
import Data.Semigroup hiding (option)
import Options.Applicative
import Options.Applicative.Builder.Internal (HasMetavar, HasValue)
import Options.Applicative.Types

-- helper functions
boolFlag :: Mod FlagFields Bool -> Parser Bool
boolFlag = flag False True

val
  :: (HasMetavar f, HasValue f)
  => String -> Mod f String
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
  | Doc OptCommon
        OptDoc
  | NoCommand OptNoCommand
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
          (progDesc "Produces a documentation of the setup."))) <|>
  parserOptNoCommand

parserCmdInstall :: Parser Command
parserCmdInstall = Install <$> parserOptCommon <*> parserOptCommonDb <*> parserOptInstall

parserCmdUpgrade :: Parser Command
parserCmdUpgrade = Upgrade <$> parserOptCommon <*> parserOptCommonDb

parserCmdDoc :: Parser Command
parserCmdDoc = Doc <$> parserOptCommon <*> parserOptDoc

-- Commons
data OptCommon = OptCommon
  { optSetup   :: FilePath
  , optVerbose :: Bool
  , optDebug   :: Bool
  } deriving (Show)

parserOptCommon :: Parser OptCommon
parserOptCommon =
  OptCommon <$>
  strOption
    (long "setup" <> short 's' <> help "Setup file (yaml)" <> val "setup.yml" <>
     action "file -X '!*.yml'" <>
     action "directory") <*>
  boolFlag (long "verbose" <> short 'v' <> help "Verbose") <*>
  boolFlag (long "debug" <> help "Debug")

-- Commons Execute
data OptCommonDb = OptCommonDb
  { optEmulate             :: Bool
  , optPrint               :: Bool
  , optConnection          :: String
  , optPermitDataDeletion  :: Bool
  , optSqlLog              :: Maybe FilePath
  , optSqlLogHideRollbacks :: Bool
  } deriving (Show)

justStr :: ReadM (Maybe String)
justStr = Just <$> ReadM ask

parserOptCommonDb :: Parser OptCommonDb
parserOptCommonDb =
  OptCommonDb <$>
  boolFlag (long "emulate" <> short 'e' <> help "Perform changes but rollback") <*>
  boolFlag
    (long "print" <> short 'p' <> help "Print SQL code instead of executing") <*>
  strOption (long "connection" <> short 'c' <> val "postgresql://") <*>
  boolFlag
    (long "permit-data-deletion" <> help "Permit deletion of columns and tables") <*>
  option
    justStr
    (long "sql-log" <>
     help
       ("If specified, log SQL statements to given file. " <>
        "Existing logfiles will be extended, not deleted.") <>
     value Nothing <>
     metavar "<log file>") <*>
  boolFlag
    (long "sql-log-hide-rollbacks" <>
     help
       "Hide ROLLBACK and SAVEPOINT statements. Useful for creating migration code via --log-sql.")

-- Command Install
data OptInstall = OptInstall
  { optDeleteExistingDatabase :: Bool
  , optDeleteResidualRoles    :: Bool
  } deriving (Show)

parserOptInstall :: Parser OptInstall
parserOptInstall =
  OptInstall <$>
  boolFlag
    (long "delete-existing-database" <> short 'd' <>
     help "Delete database if it allready exists") <*>
  boolFlag (long "delete-residual-roles" <> help "Delete residual roles")

-- Command NoCommand
data OptNoCommand = OptNoCommand
  { optVersion :: Bool
  } deriving (Show)

parserOptNoCommand :: Parser Command
parserOptNoCommand =
  NoCommand . OptNoCommand <$>
  flag' True (long "version" <> help "Prints program version")

-- Command Doc
data OptDoc = OptDoc
  { optOutputDir :: FilePath
  , optTemplate  :: FilePath
  } deriving (Show)

parserOptDoc :: Parser OptDoc
parserOptDoc =
  OptDoc <$>
  strOption
    (long "output-dir" <> short 'o' <> val "docs/" <> action "directory") <*>
  strOption
    (long "template" <> short 't' <> val "DEFAULT.rst" <>
     action "file -X '!*.html'" <>
     action "file -X '!*.md'" <>
     action "file -X '!*.rst'" <>
     action "directory")
