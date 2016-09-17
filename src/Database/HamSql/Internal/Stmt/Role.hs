-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Role where

--import Database.HamSql
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Stmt.Commons
import Database.HamSql.Setup
import Database.YamSql

instance ToSqlStmts (SqlContextObj Role) where
  toSqlStmts = stmtsDeployRole

stmtsDeployRole :: SetupContext -> SqlContextObj Role -> [SqlStmt]
stmtsDeployRole SetupContext {setupContextSetup = setup} obj@SqlContextObj {sqlObjectObject = r} =
  newSqlStmt SqlCreateRole obj sqlCreateRole :
  stmtCommentOn
    "ROLE"
    --(setupRolePrefix' setup // roleName r)
    obj -- TODO: THIS IS WRONG.
    (roleDescription r) :
  maybeMap sqlRoleMembership (roleMemberIn r)
  where
    sqlCreateRole =
      "CREATE ROLE" <-> prefix (roleName r) <-> sqlLogin (roleLogin r) <->
      sqlPassword (rolePassword r)
    sqlRoleMembership group =
      newSqlStmt SqlRoleMembership obj $
      "GRANT" <-> prefix group <-> "TO" <-> prefix (roleName r)
    sqlLogin (Just True) = "LOGIN"
    sqlLogin _ = "NOLOGIN"
    sqlPassword Nothing = ""
    sqlPassword (Just p) = "ENCRYPTED PASSWORD '" <> p <> "' "
    prefix role = prefixedRole setup role
