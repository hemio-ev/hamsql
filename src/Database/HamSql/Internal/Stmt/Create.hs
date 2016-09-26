-- This file is part of HamSql
--
-- Copyright 2014-2015 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.Stmt.Create where

import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Stmt.Basic
import Database.HamSql.Internal.Stmt.Commons  ()
import Database.HamSql.Internal.Stmt.Domain   ()
import Database.HamSql.Internal.Stmt.Function ()
import Database.HamSql.Internal.Stmt.Role     ()
import Database.HamSql.Internal.Stmt.Schema   ()
import Database.HamSql.Internal.Stmt.Sequence ()
import Database.HamSql.Internal.Stmt.Table    ()
import Database.HamSql.Internal.Stmt.Trigger  ()
import Database.HamSql.Internal.Stmt.Type     ()

fa
  :: Show b
  => Maybe b -> Schema -> [SetupElement]
fa source schema =
  [toSetupElement $ SqlContextObj schema] ++
  toElemList' SqlContextObj schemaRoles schema ++
  toElemList SqlContextSqo schemaDomains schema ++
  toElemList SqlContextSqoArgtypes schemaFunctions schema ++
  toElemList SqlContextSqo schemaSequences schema ++
  toElemList SqlContextSqo schemaTables schema ++
  toElemList SqlContextSqo schemaTypes schema ++
  concat
    [ map (toSetupElement . SqlContextSqoObj schema table) $ tableColumns table
    | table <- maybeList $ schemaTables schema
    ]
  where
    toSetupElement x = SetupElement x source
    toElemList x y = map (toSetupElement . x schema) . maybeList . y
    toElemList' x y = map (toSetupElement . x) . maybeList . y

fb :: SetupContext -> [SetupElement] -> [SqlStmt]
fb x = concatMap (toSqlStmts x)

emptyName :: SqlId
emptyName = SqlId $ SqlIdContentObj "?" $ SqlName ""

sqlAddTransact :: [SqlStmt] -> [SqlStmt]
sqlAddTransact xs =
  [newSqlStmt SqlUnclassified emptyName "BEGIN TRANSACTION"] ++
  xs ++ [newSqlStmt SqlUnclassified emptyName "COMMIT"]

-- | create database
sqlCreateDatabase :: Bool -> SqlName -> [SqlStmt]
sqlCreateDatabase deleteDatabase dbName =
  [ sqlDelete deleteDatabase
  , newSqlStmt SqlCreateDatabase (SqlId $ SqlIdContentObj "DATABASE" dbName) $
    "CREATE DATABASE " <> toSqlCode dbName
  , newSqlStmt
      SqlCreateDatabase
      (SqlId $ SqlIdContentObj "DATABASE" dbName)
      "ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON FUNCTIONS FROM PUBLIC"
  ]
  where
    sqlDelete True =
      newSqlStmt SqlDropDatabase (SqlId $ SqlIdContentObj "DATABASE" dbName) $
      "DROP DATABASE IF EXISTS" <-> toSqlCode dbName
    sqlDelete False = SqlStmtEmpty

-- | Setup
getSetupStatements :: OptCommon -> Setup -> [SqlStmt]
getSetupStatements opts s =
  debug opts "stmtInstallSetup" $
  [getStmt $ setupPreCode s] ++ schemaStatements ++ [getStmt $ setupPostCode s]
  where
    schemaStatements =
      concatMap (getSchemaStatements opts s) (maybeList $ setupSchemaData s)
    getStmt (Just code) = newSqlStmt SqlPre emptyName code
    getStmt Nothing = SqlStmtEmpty

getSchemaStatements :: OptCommon -> Setup -> Schema -> [SqlStmt]
getSchemaStatements _ setup s =
  fb (SetupContext setup) $ fa (Just ("src" :: String)) s
