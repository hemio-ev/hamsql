-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Cli
  ( run
  , parseArgv
  , parseThisArgv
  ) where

import Control.Monad (void, when)
import Control.Monad.Trans.Reader (runReaderT)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Version (showVersion)
import Database.PostgreSQL.Simple (close)
import Network.URI
import Options.Applicative hiding (info)
import System.Environment (getArgs)

import Paths_hamsql (version)

import Database.HamSql
import Database.HamSql.Internal.InquireDeployed
import Database.HamSql.Internal.Stmt.Database
import Database.HamSql.Setup
import Database.HamSql.Write
import Database.YamSql

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs {prefShowHelpOnEmpty = True}

parseArgv :: IO Command
parseArgv = getArgs >>= parseThisArgv

parseThisArgv :: [String] -> IO Command
parseThisArgv xs =
  handleParseResult $ execParserPure parserPrefs parserInfoHamsql xs

run :: Command -> IO ()
-- Install
run (Install optCommon optDb optInstall)
  | optPermitDataDeletion optDb /= optDeleteExistingDatabase optInstall =
    err $
    "For installs either both --permit-data-deletion and --delete-existing-database" <->
    "must be supplied or non of them."
  | otherwise = do
    setup <- loadSetup (optSetup optCommon)
    let dbname = SqlName $ T.pack $ tail $ uriPath $ getConUrl optDb
    if not (optEmulate optDb || optPrint optDb)
      then close =<<
           pgsqlExecWithoutTransact
             optDb
             ((getConUrl optDb) {uriPath = "/postgres"})
             (catMaybes $
              stmtsCreateDatabase (optDeleteExistingDatabase optInstall) dbname)
      else when (optDeleteExistingDatabase optInstall) $
           warn' $
           "In --emulate and --print mode the DROP/CREATE DATABASE" <->
           "statements are skipped. You have to ensure that an empty" <->
           "database exists for those commands to make sense."
    dropRoleStmts <-
      if optDeleteResidualRoles optInstall
        then pgsqlConnectUrl (getConUrl optDb) >>=
             runReaderT (pgsqlDropAllRoleStmts setup)
        else return []
    useSqlStmts optCommon optDb $ sort $ (stmtsInstall setup) ++ dropRoleStmts
-- Upgrade
run (Upgrade optCommon optDb) = do
  sourceSetup' <- loadSetup (optSetup optCommon)
  conn <- pgsqlConnectUrl (getConUrl optDb)
  sourceSetup <- runReaderT (normalizeOnline sourceSetup') conn
  targetSetup <- runReaderT (inquireSetup $ setupRolePrefix sourceSetup') conn
  let stmts = upgradeStmts sourceSetup targetSetup
  --deleteStmts <- pgsqlDeleteAllStmt conn
  --fragile <- pgsqlUpdateFragile setup conn (stmtsInstall setup)
  --let stmts = sort deleteStmts ++ Data.List.filter allowInUpgrade (sort fragile)
  useSqlStmts optCommon optDb stmts
run (Yamsql optDb OptYamsql {optYamsqlDir = p}) = do
  conn <- pgsqlConnectUrl (getConUrl optDb)
  setup <- runReaderT (inquireSetup Nothing) conn
  void $ doWrite p $ setupToDirTree "db" setup
-- Doc
run (Doc optCommon optDoc) = do
  setup <- loadSetup (optSetup optCommon)
  docWrite optDoc setup
run (NoCommand opt)
  | optVersion opt = putStrLn $ "hamsql " ++ showVersion version
  | otherwise = err "UNEXPECTED: You supplied an unsupported option."

useSqlStmts :: OptCommon -> OptCommonDb -> [SqlStmt] -> IO ()
useSqlStmts optCommon optDb unfilteredStmts
  | optPrint optDb = T.IO.putStrLn $ sqlPrinter $ sqlAddTransact stmts
  | optEmulate optDb = close =<< pgsqlExec optDb (getConUrl optDb) stmts
  | otherwise = close =<< pgsqlExec optDb (getConUrl optDb) stmts
  where
    warnOnDiff xs =
      case unfilteredStmts \\ xs of
        [] -> xs
        ys ->
          warn
            ("A total of" <-> tshow (length ys) <->
             "objects will not be deleted. You must supply the" <->
             "--permit-data-deletion argument if you want to delete them.") $
          info
            optCommon
            ("The following objects are not deleted:" <\>
             showCode (T.intercalate "\n" (map stmtDesc ys)))
            xs
    stmts
      | optPermitDataDeletion optDb = unfilteredStmts
      | otherwise =
        warnOnDiff
          [x | x <- unfilteredStmts, not $ stmtRequiresPermitDeletion x]
