-- This file is part of HamSql
--
-- Copyright 2014-2015 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.Stmt.Create where

import Data.Maybe

import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Stmt.Basic
import Database.HamSql.Internal.Stmt.Commons ()
import Database.HamSql.Internal.Stmt.Domain ()
import Database.HamSql.Internal.Stmt.Function ()
import Database.HamSql.Internal.Stmt.Role ()
import Database.HamSql.Internal.Stmt.Schema ()
import Database.HamSql.Internal.Stmt.Sequence ()
import Database.HamSql.Internal.Stmt.Table ()
import Database.HamSql.Internal.Stmt.Type ()

fa
  :: Show b
  => Maybe b -> Schema -> [SetupElement]
fa source schema =
  [toSetupElement $ SqlContext schema] ++
  toElemList' schemaRoles schema ++
  toElemList schemaDomains schema ++
  toElemList schemaFunctions schema ++
  toElemList schemaSequences schema ++
  toElemList schemaTables schema ++
  toElemList schemaTypes schema ++
  concat
    [ map (toSetupElement . (\x -> SqlContext (schema, table, x))) $
     tableColumns table
    | table <- fromMaybe [] $ schemaTables schema ]
  where
    toSetupElement x = SetupElement x source
    toElemList y =
      maybeMap (toSetupElement . (\x -> SqlContext (schema, x))) . y
    toElemList' y = maybeMap (toSetupElement . SqlContext) . y

fb :: SetupContext -> [SetupElement] -> [Maybe SqlStmt]
fb x = concatMap (toSqlStmts x)

data SQL_OTHER =
  SQL_OTHER
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_OTHER where
  toSqlCode = const "SQL_OTHER"

data SQL_DATABASE =
  SQL_DATABASE
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_DATABASE where
  toSqlCode = const "DATABASE"

emptyName :: SqlId
emptyName = SqlId $ SqlObj SQL_OTHER $ SqlName ""

sqlAddTransact :: [SqlStmt] -> [SqlStmt]
sqlAddTransact xs =
  catMaybes [newSqlStmt SqlUnclassified emptyName "BEGIN TRANSACTION"] ++
  xs ++ catMaybes [newSqlStmt SqlUnclassified emptyName "COMMIT"]

-- | create database
sqlCreateDatabase :: Bool -> SqlName -> [Maybe SqlStmt]
sqlCreateDatabase deleteDatabase dbName =
  [ sqlDelete deleteDatabase
  , newSqlStmt SqlCreateDatabase (SqlId $ SqlObj SQL_DATABASE dbName) $
    "CREATE DATABASE " <> toSqlCode dbName
  , newSqlStmt
      SqlCreateDatabase
      (SqlId $ SqlObj SQL_DATABASE dbName)
      "ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON FUNCTIONS FROM PUBLIC"
  ]
  where
    sqlDelete True =
      newSqlStmt SqlDropDatabase (SqlId $ SqlObj SQL_DATABASE dbName) $
      "DROP DATABASE IF EXISTS" <-> toSqlCode dbName
    sqlDelete False = Nothing

-- | Setup
getSetupStatements :: OptCommon -> Setup -> [Maybe SqlStmt]
getSetupStatements opts s =
  debug opts "stmtInstallSetup" $
  [getStmt $ setupPreCode s] ++ schemaStatements ++ [getStmt $ setupPostCode s]
  where
    schemaStatements =
      concat $ maybeMap (getSchemaStatements opts s) (setupSchemaData s)
    getStmt (Just code) = newSqlStmt SqlPre emptyName code
    getStmt Nothing = Nothing

getSchemaStatements :: OptCommon -> Setup -> Schema -> [Maybe SqlStmt]
getSchemaStatements _ setup s =
  fb (SetupContext setup) $ fa (Just ("src" :: String)) s
