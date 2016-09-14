-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Domain where

--import qualified Data.Text as T
import Database.HamSql.Internal.Stmt.Basic

stmtsDropDomain :: SqlIdContentSqo -> [SqlStmt]
stmtsDropDomain x = [newSqlStmt SqlDropDomain x $ "DROP DOMAIN" <-> toSqlCode x]

stmtsDropDomainConstraint :: SqlIdContentSqoObj -> [SqlStmt]
stmtsDropDomainConstraint x =
  [ newSqlStmt SqlDropDomainConstraint x $
    "ALTER DOMAIN" <-> sqlSqoIdCode x <-> "DROP CONSTRAINT" <->
    sqlSqoObjIdCode x
  ]

instance ToSqlStmts (SqlContextSqo Domain) where
  toSqlStmts = stmtsDeployDomain

stmtsDeployDomain :: SetupContext -> SqlContextSqo Domain -> [SqlStmt]
stmtsDeployDomain _ obj@SqlContextSqo {sqlSqoObject = d} =
  stmtCreateDomain :
  sqlDefault (domainDefault d) : maybeMap sqlCheck (domainChecks d)
--stmtCommentOn "DOMAIN" fullName (domainDescription d)
  where
    stmtCreateDomain =
      newSqlStmt SqlCreateDomain obj $
      "CREATE DOMAIN" <-> sqlIdCode obj <-> "AS" <-> toSqlCode (domainType d)
    sqlCheck :: Check -> SqlStmt
    sqlCheck c =
      newSqlStmt SqlCreateCheckConstr obj $
      "ALTER DOMAIN" <-> sqlIdCode obj <-> "ADD CONSTRAINT" <->
      toSqlCode (constrName (checkName c)) <->
      "CHECK (" <>
      checkCheck c <>
      ")"
    sqlDefault Nothing =
      newSqlStmt SqlAddDefault obj $
      "ALTER DOMAIN" <-> sqlIdCode obj <-> "DROP DEFAULT"
    sqlDefault (Just def) =
      newSqlStmt SqlAddDefault obj $
      "ALTER DOMAIN" <-> sqlIdCode obj <-> "SET DEFAULT" <-> def
    constrName a = SqlName "DOMAIN_" // domainName d // SqlName "__" // a
