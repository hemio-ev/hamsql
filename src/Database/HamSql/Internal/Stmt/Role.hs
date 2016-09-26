-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Role where

import Database.HamSql.Internal.Stmt.Basic

stmtsDropRole :: Setup -> SqlIdContentObj -> [Maybe SqlStmt]
stmtsDropRole setup role =
  [ newSqlStmt SqlDropRole role $
    "DROP ROLE " <> prefixedRole setup (sqlObjId role)
  ]

instance ToSqlStmts (SqlContextObj Role) where
  toSqlStmts SetupContext {setupContextSetup = setup} obj@SqlContextObj {sqlObjectObject = r} =
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
