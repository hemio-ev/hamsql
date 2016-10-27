-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.HamSql.Internal.DbUtils where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as TIO
import Database.PostgreSQL.Simple
import Network.URI (URI (..), parseAbsoluteURI, uriToString)

import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Utils
import Database.YamSql

sqlErrObjectInUse :: B.ByteString
sqlErrObjectInUse = "55006"

toQry :: Text -> Query
toQry = fromString . T.unpack

logStmt :: OptCommonDb -> Text -> IO ()
logStmt opt x =
  case optSqlLog opt of
    Nothing -> return ()
    Just filename -> TIO.appendFile filename (x <> "\n")

getConUrl :: OptCommonDb -> URI
getConUrl optDb = appendQuery "application_name=hamsql" uri
  where
    uri =
      fromJustReason "Not a valid URI" (parseAbsoluteURI $ optConnection optDb)
    appendQuery v u =
      u
      { uriQuery =
        (case maybeHead $ uriQuery u of
           Just '?' -> "&"
           Just _ -> err $ "invalid URI" <-> tshow u
           Nothing -> "?") <>
        v
      }

pgsqlExecStmt :: Connection -> SqlStmt -> IO ()
pgsqlExecStmt conn stmt = do
  let code = toSqlCode stmt
  _ <- execute_ conn (toQry code)
  return ()

pgsqlExecStmtHandled :: Connection -> SqlStmt -> IO ()
pgsqlExecStmtHandled conn stmt =
  pgsqlExecStmt conn stmt `catch` pgsqlHandleErr stmt conn

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

pgsqlHandleErr :: SqlStmt -> Connection -> SqlError -> IO ()
pgsqlHandleErr stmt conn e = do
  extraMsg <-
    if sqlState e == sqlErrObjectInUse && stmtIdType stmt == SqlDropDatabase
      then do
        xs :: [(SqlName, SqlName, Text)] <-
          query_
            conn
            "SELECT datname, usename, application_name FROM pg_stat_activity WHERE pid <> pg_backend_pid()"
        return $
          "The following existing connection(s) might have caused the error:" <\>
          T.intercalate
            "\n"
            (map showConnected $
             filter (\(db, _, _) -> toSqlCode db == sqlIdCode stmt) xs)
      else return ""
  _ <-
    err $
    "An SQL error occured while executing the following statement" <>
    showCode (toSqlCode stmt) <\>
    "The SQL-Server reported" <\>
    "Message:" <>
    showCode (decodeUtf8 (sqlErrorMsg e)) <\>
    "Code: " <>
    showCode (decodeUtf8 (sqlState e)) <\>
    errDetail <\>
    errHint <\>
    extraMsg <\>
    "\nAll statements have been rolled back if possible."
  return ()
  where
    showConnected (_, role, app) = " - role" <-> toSqlCode role <> appOut app
    appOut "" = ""
    appOut x = " via application '" <> x <> "'"
    errDetail =
      case sqlErrorDetail e of
        "" -> ""
        x -> "Detail:" <> showCode (decodeUtf8 x)
    errHint =
      case sqlErrorHint e of
        "" -> ""
        x -> "Hint:" <> showCode (decodeUtf8 x)
