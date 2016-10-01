-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Role where

import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic

stmtsDropRole :: Setup -> SqlObj SQL_ROLE SqlName -> [Maybe SqlStmt]
stmtsDropRole setup role@(SqlObj _ roleName) =
  [newSqlStmt SqlDropRole role $ "DROP ROLE " <> prefixedRole setup roleName]

stmtsDropAllPrivileges :: Setup -> SqlObj SQL_ROLE SqlName -> [Maybe SqlStmt]
stmtsDropAllPrivileges setup x@(SqlObj _ n)
  | schemas == [] = [Nothing]
  | otherwise =
    [ newSqlStmt SqlRevokePrivilege x $
     "REVOKE ALL PRIVILEGES ON ALL" <-> objType <-> "IN SCHEMA" <->
     T.intercalate ", " (map toSqlCode schemas) <->
     "FROM" <->
     prefixedRole setup n
    | objType <- ["TABLES", "SEQUENCES", "FUNCTIONS"] ]
  where
    schemas = maybeMap schemaName (setupSchemaData setup)

instance ToSqlStmts (SqlContext Role) where
  toSqlStmts SetupContext {setupContextSetup = setup} obj@(SqlContext r) =
    [stmtCreateRole, stmtAlterRole, stmtCommentRole] ++
    maybeMap sqlRoleMembership (roleMemberIn r)
    where
      stmtCreateRole =
        newSqlStmt SqlCreateRole obj $ "CREATE ROLE" <-> prefix (roleName r)
      stmtAlterRole =
        newSqlStmt SqlAlterRole obj $
        "ALTER ROLE" <-> prefix (roleName r) <-> "WITH" <->
        sqlLogin (roleLogin r) <->
        sqlPassword (rolePassword r)
      stmtCommentRole =
        newSqlStmt SqlComment obj $
        "COMMENT ON ROLE" <-> prefix (roleName r) <-> "IS" <->
        toSqlCodeString (roleDescription r)
      sqlRoleMembership group =
        newSqlStmt SqlRoleMembership obj $
        "GRANT" <-> prefix group <-> "TO" <-> prefix (roleName r)
      sqlLogin (Just True) = "LOGIN"
      sqlLogin _ = "NOLOGIN"
      sqlPassword Nothing = "PASSWORD NULL"
      sqlPassword (Just p) = "ENCRYPTED PASSWORD '" <> p <> "' "
      prefix role = prefixedRole setup role
