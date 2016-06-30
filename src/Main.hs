-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void, when)
import Data.List

import qualified Data.Text           as T
import qualified Data.Text.IO        as T.IO
import           Network.URI
import           Options.Applicative (execParser)

import Database.YamSql
import Database.HamSql
import Database.HamSql.Internal.Utils
import Database.HamSql.Internal.Sql
import Database.HamSql.SqlStatement

main :: IO ()
main = execParser parserInfoHamsql >>= run

run :: Command -> IO ()

-- Install
run (Install opt optDb optInstall) = do
  let dbname = SqlName $ T.pack $ tail $ uriPath $ getConUrl optDb

  if not (optEmulate optDb || optPrint optDb) then
    void $ pgsqlExecWithoutTransact
      ((getConUrl optDb) { uriPath = "/postgres" })
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
  do
    setup <- loadSetup opt (optSetup opt)
    docWrite optDoc setup

useSqlStmts :: OptCommonDb -> [SqlStatement] -> IO ()
useSqlStmts optDb stmts
  | optPrint optDb = T.IO.putStrLn $ sqlPrinter $ sqlAddTransact stmts
  | optEmulate optDb = void $ pgsqlExec (getConUrl optDb) stmts
  | otherwise = void $ pgsqlExec (getConUrl optDb) stmts

