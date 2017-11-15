-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.Stmt.Database where

import Database.HamSql.Internal.Stmt.Basic
import Database.HamSql.Internal.Stmt.Schema

data SQL_DATABASE =
  SQL_DATABASE
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_DATABASE where
  toSqlCode = const "DATABASE"

-- | create database
stmtsCreateDatabase :: Bool -> SqlName -> [Maybe SqlStmt]
stmtsCreateDatabase deleteDatabase dbName =
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
