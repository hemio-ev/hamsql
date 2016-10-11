-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Role where

import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic

stmtsDropRole :: Setup -> SqlObj SQL_ROLE SqlName -> [Maybe SqlStmt]
stmtsDropRole setup role@(SqlObj _ roleSqlName) =
  [newSqlStmt SqlDropRole role $ "DROP ROLE " <> prefixedRole setup roleSqlName]

stmtsDropAllPrivileges :: Setup
                       -> [SqlName]
                       -> SqlObj SQL_ROLE SqlName
                       -> [Maybe SqlStmt]
stmtsDropAllPrivileges setup schemas x@(SqlObj _ n)
  | null schemas = [Nothing]
  | otherwise =
    [ newSqlStmt SqlRevokePrivilege x $
     "REVOKE ALL PRIVILEGES ON ALL" <-> objType <-> "IN SCHEMA" <->
     T.intercalate ", " (map toSqlCode schemas) <->
     "FROM" <->
     prefixedRole setup n
    | objType <- ["TABLES", "SEQUENCES", "FUNCTIONS"] ]

stmtRevokeMembership :: Setup
                     -> SqlObj SQL_ROLE_MEMBERSHIP (SqlName, SqlName)
                     -> [Maybe SqlStmt]
stmtRevokeMembership setup x@(SqlObj _ (role, member)) =
  [ newSqlStmt SqlRevokeMembership x $
    "REVOKE" <-> prefixedRole setup role <-> "FROM" <-> prefixedRole setup member
  ]

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
        newSqlStmt SqlGrantMembership obj $
        "GRANT" <-> prefix group <-> "TO" <-> prefix (roleName r)
      sqlLogin (Just True) = "LOGIN"
      sqlLogin _ = "NOLOGIN"
      sqlPassword Nothing = "PASSWORD NULL"
      sqlPassword (Just p) = "ENCRYPTED PASSWORD '" <> p <> "' "
      prefix role = prefixedRole setup role
