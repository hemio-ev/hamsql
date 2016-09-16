-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void, when)
import Data.List

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Network.URI
import Options.Applicative (execParser)

import Database.HamSql
import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.Sql
import Database.HamSql.Internal.Utils
import Database.HamSql.SqlStmt
import Database.YamSql

main :: IO ()
main = execParser parserInfoHamsql >>= run

run :: Command -> IO ()
-- Install
run (Install optCommon optDb optInstall) = do
  let dbname = SqlName $ T.pack $ tail $ uriPath $ getConUrl optDb
  if not (optEmulate optDb || optPrint optDb)
    then void $
         pgsqlExecWithoutTransact
           ((getConUrl optDb)
            { uriPath = "/postgres"
            })
           (sqlCreateDatabase (optDeleteExistingDatabase optInstall) dbname)
    else when (optDeleteExistingDatabase optInstall) $
         warn' $
         "In --emulate and --print mode the" <->
         "DROP/CREATE DATABASE statements are skipped. You have to ensure that an empty" <->
         "database exists for those commands to make sense."
  setup <- loadSetup optCommon (optSetup optCommon)
  return ()
  statements <- pgsqlGetFullStatements optCommon optDb setup
  useSqlStmts optCommon optDb (sort statements)
-- Upgrade
run (Upgrade optCommon optDb optUpgrade) = do
  setup <- loadSetup optCommon (optSetup optCommon)
  conn <- pgsqlConnectUrl (getConUrl optDb)
  deleteStmts <- pgsqlDeleteAllStmt conn
  createStmts <- pgsqlGetFullStatements optCommon optDb setup
  fragile <- pgsqlUpdateFragile optUpgrade conn createStmts
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
             "objects will not be deleted." <->
             "You must supply the --permit-data-deletion if you want to delete them.") $
          info
            optCommon
            ("The following objects are not deleted:" <>
             showCode (T.cons '\n' $ T.intercalate "\n" (map stmtDesc ys)))
            xs
    stmts
      | optPermitDataDeletion optDb = unfilteredStmts
      | otherwise =
        warnOnDiff
          [ x
          | x <- unfilteredStmts 
          , not $ stmtRequiresPermitDeletion x ]
