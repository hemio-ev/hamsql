-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Role where

import Database.HamSql.Internal.Stmt.Basic

stmtsDropRole :: Setup -> SqlIdContentObj -> [SqlStmt]
stmtsDropRole setup role = [newSqlStmt SqlDropRole role $ "DROP ROLE " <> prefixedRole setup (sqlObjId role)]

instance ToSqlStmts (SqlContextObj Role) where
  toSqlStmts SetupContext {setupContextSetup = setup} obj@SqlContextObj {sqlObjectObject = r} =
    newSqlStmt SqlCreateRole obj sqlCreateRole :
    newSqlStmt SqlComment obj (
      "COMMENT ON ROLE" <-> prefix (roleName r) <-> "IS" <> toSqlCodeString (roleDescription r)) :
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
