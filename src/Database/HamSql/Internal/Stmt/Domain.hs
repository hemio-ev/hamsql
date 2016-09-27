-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Domain where

import Database.HamSql.Internal.Stmt.Basic

stmtsDropDomain :: SqlIdContentSqo -> [Maybe SqlStmt]
stmtsDropDomain x = [newSqlStmt SqlDropDomain x $ "DROP DOMAIN" <-> toSqlCode x]

stmtsDropDomainConstr :: SqlIdContentSqoObj -> [Maybe SqlStmt]
stmtsDropDomainConstr x =
  [ newSqlStmt SqlDropDomainConstr x $
    "ALTER DOMAIN" <-> sqlSqoIdCode x <-> "DROP CONSTRAINT" <->
    sqlSqoObjIdCode x
  ]

instance ToSqlStmts (SqlContextSqo Domain) where
  toSqlStmts = stmtsDeployDomain

stmtsDeployDomain :: SetupContext -> SqlContextSqo Domain -> [Maybe SqlStmt]
stmtsDeployDomain _ obj@SqlContextSqo {sqlSqoObject = d} =
  stmtCreateDomain :
  sqlDefault (domainDefault d) : maybeMap sqlCheck (domainChecks d)
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
      newSqlStmt SqlAddDefault obj $
      "ALTER DOMAIN" <-> sqlIdCode obj <-> "DROP DEFAULT"
    sqlDefault (Just def) =
      newSqlStmt SqlAddDefault obj $
      "ALTER DOMAIN" <-> sqlIdCode obj <-> "SET DEFAULT" <-> def
