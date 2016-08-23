-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Role where

--import Database.HamSql
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Sql
import Database.HamSql.Internal.Stmt.Commons
import Database.HamSql.Setup
import Database.YamSql

stmtsDeployRole :: OptCommon -> Setup -> Role -> [SqlStatement]
stmtsDeployRole _ setup r =
    SqlStmt SqlCreateRole (roleName r) sqlCreateRole:
    (stmtCommentOn "ROLE" (setupRolePrefix' setup // roleName r) (roleDescription r)):
    maybeMap sqlRoleMembership (roleMemberIn r)

    where
        sqlCreateRole =
          "CREATE ROLE" <-> prefix (roleName r)
          <-> sqlLogin (roleLogin r)
          <-> sqlPassword (rolePassword r)

        sqlRoleMembership group =
            SqlStmt SqlRoleMembership (roleName r) $
            "GRANT" <-> prefix group <-> "TO" <-> prefix (roleName r);

        sqlLogin (Just True) = "LOGIN"
        sqlLogin _           = "NOLOGIN"

        sqlPassword Nothing = ""
        sqlPassword (Just p) = "ENCRYPTED PASSWORD '" <> p <> "' "

        prefix role = prefixedRole setup role

