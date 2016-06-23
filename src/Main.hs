-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings   #-}

import Control.Monad       (void, when)
import Data.List

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Network.URL
import Options.Applicative (execParser)

import Documentation
import Load
import Option
import PostgresCon
import Sql
import Parser.Basic
import Sql.Statement.Create
import Utils

main :: IO ()
main = execParser parserInfoHamsql >>= run

run :: Command -> IO ()

-- Install
run (Install opt optDb optInstall) = do
  let dbname = SqlName $ T.pack $ url_path $ getConUrl optDb

  if not (optEmulate optDb || optPrint optDb) then
    void $ pgsqlExecWithoutTransact
      ((getConUrl optDb) { url_path = "postgres" })
      (sqlCreateDatabase (optDeleteExistingDatabase optInstall) dbname)
  else
    when (optDeleteExistingDatabase optInstall) $
      warn' $ "In --emulate and --print mode the" <->
        "DROP/CREATE DATABASE statements are skipped. You have to ensure that a empty" <->
        "database exists for those commands to make sense."

  setup <- loadSetup opt (optSetup opt)

  statements <- pgsqlGetFullStatements opt optDb setup

  useSqlStmts optDb (sort statements)

-- Upgrade
run (Upgrade opt optDb optUpgrade) = do
    setup <- loadSetup opt (optSetup opt)

    conn <- pgsqlConnectUrl (getConUrl optDb)

    deleteStmts <- pgsqlDeleteAllStmt conn
    createStmts <- pgsqlGetFullStatements opt optDb setup

    fragile <- pgsqlUpdateFragile optUpgrade conn createStmts

    let stmts = sort deleteStmts ++ sort (fragile ++ Data.List.filter afterDelete createStmts)

    useSqlStmts optDb stmts

-- Doc
run (Doc opt optDoc) =
  case optFormat optDoc of
    "html" -> do
      setup <- loadSetup opt (optSetup opt)
      return ()

useSqlStmts :: OptCommonDb -> [SqlStatement] -> IO ()
useSqlStmts optDb stmts
  | optPrint optDb = T.IO.putStrLn $ sqlPrinter $ sqlAddTransact stmts
  | optEmulate optDb = void $ pgsqlExec (getConUrl optDb) stmts
  | otherwise = void $ pgsqlExec (getConUrl optDb) stmts

