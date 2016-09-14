-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.HamSql.Internal.DbUtils where

import Control.Exception
import Control.Monad (void, when)
import qualified Data.ByteString.Char8 as B
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Network.URI (URI, parseAbsoluteURI, uriToString)

import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Sql
import Database.YamSql

toQry :: Text -> Query
toQry = fromString . T.unpack

getConUrl :: OptCommonDb -> URI
getConUrl xs =
  fromJustReason "Not a valid URI" (parseAbsoluteURI $ optConnection xs)

pgsqlExecStmt :: Connection -> SqlStmt -> IO ()
pgsqlExecStmt _ SqlStmtEmpty = return ()
pgsqlExecStmt conn stmt = do
  let code = toSqlCode stmt
  _ <- execute_ conn (toQry code)
  return ()

pgsqlExecStmtHandled :: Connection -> SqlStmt -> IO ()
pgsqlExecStmtHandled conn stmt =
  pgsqlExecStmt conn stmt `catch` pgsqlHandleErr stmt

data PgSqlMode
  = PgSqlWithoutTransaction
  | PgSqlWithTransaction
  deriving (Eq)

data Status
  = Init
  | Changed
  | Unchanged

pgsqlConnectUrl :: URI -> IO Connection
pgsqlConnectUrl url = do
  connResult <- try $ connectPostgreSQL (B.pack $ uriToString id url "")
  let conn = getConn connResult
  _ <- execute_ conn "SET client_min_messages TO WARNING"
  return conn
  where
    getConn res =
      case res of
        Left e@SqlError {} ->
          err $
          "Connection to SQL-Server failed" <>
          showCode (decodeUtf8 (sqlErrorMsg e))
        Right conn -> conn

pgsqlHandleErr :: SqlStmt -> SqlError -> IO ()
pgsqlHandleErr code e = do
  _ <-
    err $
    "An SQL error occured while executing the following statement" <>
    showCode (toSqlCode code) <\>
    "The SQL-Server reported" <>
    showCode (decodeUtf8 (sqlErrorMsg e)) <->
    "(Error Code: " <>
    decodeUtf8 (sqlState e) <>
    ")" <\>
    "\nAll statements have been rolled back if possible."
  return ()
