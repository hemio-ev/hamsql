-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Main where

import Control.Monad (void, when)
import Data.List
import Data.Maybe

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Network.URI
import Options.Applicative (execParser)

import Database.HamSql
import Database.YamSql

main :: IO ()
main = execParser parserInfoHamsql >>= run

run :: Command -> IO ()
-- Install
run (Install optCommon optDb optInstall)
  | optPermitDataDeletion optDb /= optDeleteExistingDatabase optInstall =
    err $
    "For installs either both --permit-data-deletion and --delete-existing-database" <->
    "must be supplied or non of them."
  | otherwise = do
    let dbname = SqlName $ T.pack $ tail $ uriPath $ getConUrl optDb
    if not (optEmulate optDb || optPrint optDb)
      then void $
           pgsqlExecWithoutTransact
             ((getConUrl optDb)
              { uriPath = "/postgres"
              })
             (catMaybes $ sqlCreateDatabase (optDeleteExistingDatabase optInstall) dbname)
      else when (optDeleteExistingDatabase optInstall) $
           warn' $
           "In --emulate and --print mode the DROP/CREATE DATABASE" <->
           "statements are skipped. You have to ensure that an empty" <->
           "database exists for those commands to make sense."
    setup <- loadSetup optCommon (optSetup optCommon)
    stmts <- pgsqlGetFullStatements optCommon optDb setup
    -- TODO: Own option for this
    dropRoleStmts <- if optDeleteExistingDatabase optInstall then     
        pgsqlDropAllRoleStmts optDb setup
      else
        return []
    useSqlStmts optCommon optDb $ sort $ stmts ++ dropRoleStmts
-- Upgrade
run (Upgrade optCommon optDb _) = do
  setup <- loadSetup optCommon (optSetup optCommon)
  conn <- pgsqlConnectUrl (getConUrl optDb)
  deleteStmts <- pgsqlDeleteAllStmt conn
  createStmts <- pgsqlGetFullStatements optCommon optDb setup
  fragile <- pgsqlUpdateFragile setup conn createStmts
  let stmts = sort deleteStmts ++ Data.List.filter allowInUpgrade (sort fragile)
  useSqlStmts optCommon optDb stmts
-- Doc
run (Doc optCommon optDoc) = do
  setup <- loadSetup optCommon (optSetup optCommon)
  docWrite optDoc setup

useSqlStmts :: OptCommon -> OptCommonDb -> [SqlStmt] -> IO ()
useSqlStmts optCommon optDb unfilteredStmts
  | optPrint optDb = T.IO.putStrLn $ sqlPrinter $ sqlAddTransact stmts
  | optEmulate optDb = void $ pgsqlExec (getConUrl optDb) stmts
  | otherwise = void $ pgsqlExec (getConUrl optDb) stmts
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
          [ x
          | x <- unfilteredStmts 
          , not $ stmtRequiresPermitDeletion x ]
