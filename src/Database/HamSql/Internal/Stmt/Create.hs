-- This file is part of HamSql
--
-- Copyright 2014-2015 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Create where

import           Data.Maybe
import qualified Data.Text  as T

import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Sql
import Database.HamSql.Internal.Stmt.Commons
import Database.HamSql.Internal.Stmt.Domain
import Database.HamSql.Internal.Stmt.Function
import Database.HamSql.Internal.Stmt.Role
import Database.HamSql.Internal.Stmt.Sequence
import Database.HamSql.Internal.Stmt.Table
import Database.HamSql.Internal.Stmt.Trigger
import Database.HamSql.Internal.Stmt.Type
import Database.HamSql.Setup
import Database.YamSql
--import Database.HamSql

emptyName :: SqlName
emptyName = SqlName ""

sqlAddTransact :: [SqlStatement] -> [SqlStatement]
sqlAddTransact xs =
  [ SqlStmt SqlUnclassified emptyName "BEGIN TRANSACTION" ] ++
  xs ++
  [ SqlStmt SqlUnclassified emptyName "COMMIT" ]

-- create database

sqlCreateDatabase :: Bool -> SqlName -> [SqlStatement]
sqlCreateDatabase deleteDatabase name = [
        sqlDelete deleteDatabase,
        SqlStmt SqlCreateDatabase name $
          "CREATE DATABASE " <> toSql name,
        SqlStmt SqlCreateDatabase name
          "ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON FUNCTIONS FROM PUBLIC"
    ]
  where
    sqlDelete True = SqlStmt SqlDropDatabase name $
      "DROP DATABASE IF EXISTS" <-> toSql name
    sqlDelete False = SqlStmtEmpty

-- Setup

getSetupStatements :: OptCommon -> Setup -> [SqlStatement]
getSetupStatements opts s = debug opts "stmtInstallSetup" $
  [ getStmt $ setupPreCode s ] ++ schemaStatements ++ [ getStmt $ setupPostCode s ]
  where
    schemaStatements =
      concatMap (getSchemaStatements opts s) (setupSchemaData $ setupInternal s)
    getStmt (Just code) = SqlStmt SqlPreInstall emptyName code
    getStmt Nothing = SqlStmtEmpty

-- Schema

getSchemaStatements :: OptCommon -> Setup -> Schema -> [SqlStatement]
getSchemaStatements opts s m = debug opts "stmtCreateSchema" $
  [
    SqlStmt SqlCreateSchema (schemaName m) $ "CREATE SCHEMA IF NOT EXISTS" <-> toSql (schemaName m),
    postInst $ schemaExecPostInstall m,
    stmtCommentOn "schema" (schemaName m) (schemaDescription m)
  ] ++
  maybeMap privUsage (schemaPrivUsage m) ++
  maybeMap privSelectAll (schemaPrivSelectAll m) ++
  maybeMap privInsertAll (schemaPrivInsertAll m) ++
  maybeMap privUpdateAll (schemaPrivUpdateAll m) ++
  maybeMap privDeleteAll (schemaPrivDeleteAll m) ++
  maybeMap privSequenceAll (schemaPrivSequenceAll m) ++
  maybeMap privExecuteAll (schemaPrivExecuteAll m) ++
  concat (maybeMap privAllAll (schemaPrivAllAll m)) ++
  concat (maybeMap (stmtsDeployDomain opts s m) (schemaDomains m)) ++
  concat (maybeMap (stmtsDeployType opts s m) (schemaTypes m)) ++
  concat (maybeMap (stmtsDeployRole opts s) (schemaRoles m)) ++
  concat (maybeMap (stmtsDeployFunction opts s m) (schemaFunctions m)) ++
  concat (maybeMap (stmtsDeployTable opts s m) (schemaTables m)) ++
  concat (maybeMap (stmtsDeploySequence opts s m) (schemaSequences m)) ++
  concat (maybeMap (stmtsDeployTrigger opts s m) (schemaTriggers m))

  where
    postInst Nothing = SqlStmtEmpty
    postInst (Just xs) = SqlStmt SqlPostInstall emptyName xs

    priv :: Text -> SqlName -> SqlStatement
    priv p r = SqlStmt SqlPriv r $
      "GRANT " <> p <> " " <> toSql (schemaName m) <> " TO " <> prefixedRole s r

    privUsage = priv "USAGE ON SCHEMA"
    privSelectAll = priv "SELECT ON ALL TABLES IN SCHEMA"
    privInsertAll = priv "INSERT ON ALL TABLES IN SCHEMA"
    privUpdateAll = priv "UPDATE ON ALL TABLES IN SCHEMA"
    privDeleteAll = priv "DELETE ON ALL TABLES IN SCHEMA"
    privSequenceAll = priv "USAGE ON ALL SEQUENCES IN SCHEMA"
    privExecuteAll = priv "EXECUTE ON ALL FUNCTIONS IN SCHEMA"
    privAllAll d = map (\x -> x d)
      [
        privUsage,
        privSelectAll,
        privInsertAll,
        privUpdateAll,
        privDeleteAll,
        privSequenceAll,
        privExecuteAll
      ]

