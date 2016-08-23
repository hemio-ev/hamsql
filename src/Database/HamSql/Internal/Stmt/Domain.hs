-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Domain where

--import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic

stmtsDeployDomain :: OptCommon -> Setup -> Schema -> Domain -> [SqlStatement]
stmtsDeployDomain opt _ m d = debug opt "stmtCreateDomain" $

  stmtCreateDomain
  :sqlDefault (domainDefault d)
  :maybeMap sqlCheck (domainChecks d)

  --stmtCommentOn "DOMAIN" fullName (domainDescription d)

    where
    fullName = schemaName m <.> domainName d

    stmtCreateDomain = SqlStmt SqlCreateDomain fullName $
      "CREATE DOMAIN" <-> toSql fullName <-> "AS" <-> toSql (domainType d)

    sqlCheck :: Check -> SqlStatement
    sqlCheck c = SqlStmt SqlCreateCheckConstr fullName $
      "ALTER DOMAIN" <-> toSql fullName
      <-> "ADD CONSTRAINT" <-> toSql (name (checkName c))
      <-> "CHECK (" <> checkCheck c <> ")"

    sqlDefault Nothing = SqlStmtEmpty
    sqlDefault (Just def) = SqlStmt SqlAddDefault fullName $
      "ALTER DOMAIN" <-> toSql fullName <-> "SET DEFAULT" <-> def

    name a = SqlName "DOMAIN_" // domainName d // SqlName "__" // a




