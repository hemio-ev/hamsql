-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Domain where

import Database.HamSql.Internal.Stmt.Basic

stmtsDropDomain :: SqlObj SQL_DOMAIN SqlName -> [Maybe SqlStmt]
stmtsDropDomain x = [newSqlStmt SqlDropDomain x $ "DROP DOMAIN" <-> toSqlCode x]

stmtsDropDomainConstr :: SqlObj SQL_DOMAIN_CONSTRAINT (SqlName, SqlName)
                      -> [Maybe SqlStmt]
stmtsDropDomainConstr obj@(SqlObj _ (d, c)) =
  [ newSqlStmt SqlDropDomainConstr obj $
    "ALTER DOMAIN" <-> toSqlCode d <-> "DROP CONSTRAINT" <-> toSqlCode c
  ]

instance ToSqlStmts (SqlContext (Schema, Domain)) where
  toSqlStmts _ obj@(SqlContext (_, d)) =
    stmtCreateDomain :
    sqlDefault (domainDefault d) :
    stmtCommentOn obj (domainDescription d) :
    maybeMap sqlCheck (domainChecks d)
    where
      stmtCreateDomain =
        newSqlStmt SqlCreateDomain obj $
        "CREATE DOMAIN" <-> sqlIdCode obj <-> "AS" <-> toSqlCode (domainType d)
      sqlCheck :: Check -> Maybe SqlStmt
      sqlCheck c =
        newSqlStmt SqlCreateCheckConstr obj $
        "ALTER DOMAIN" <-> sqlIdCode obj <-> "ADD CONSTRAINT" <->
        toSqlCode (checkName c) <->
        "CHECK (" <>
        checkCheck c <>
        ")"
      sqlDefault Nothing =
        newSqlStmt SqlDomainSetDefault obj $
        "ALTER DOMAIN" <-> sqlIdCode obj <-> "DROP DEFAULT"
      sqlDefault (Just def) =
        newSqlStmt SqlDomainSetDefault obj $
        "ALTER DOMAIN" <-> sqlIdCode obj <-> "SET DEFAULT" <-> def
