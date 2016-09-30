-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Schema where

import Database.HamSql.Internal.Stmt.Basic

instance ToSqlStmts (SqlContext Schema) where
  toSqlStmts SetupContext {setupContextSetup = setup} obj@(SqlContext s) =
    [ newSqlStmt SqlCreateSchema obj $
      "CREATE SCHEMA IF NOT EXISTS" <-> sqlIdCode obj
    , postInst $ schemaExecPostInstall s
    , postInstallAndUpgrade $ schemaExecPostInstallAndUpgrade s
    , stmtCommentOn obj (schemaDescription s)
    ] ++
    maybeMap privUsage (schemaPrivUsage s) ++
    maybeMap privSelectAll (schemaPrivSelectAll s) ++
    maybeMap privInsertAll (schemaPrivInsertAll s) ++
    maybeMap privUpdateAll (schemaPrivUpdateAll s) ++
    maybeMap privDeleteAll (schemaPrivDeleteAll s) ++
    maybeMap privSequenceAll (schemaPrivSequenceAll s) ++
    maybeMap privExecuteAll (schemaPrivExecuteAll s) ++
    concat (maybeMap privAllAll (schemaPrivAllAll s))
    where
      postInst Nothing = Nothing
      postInst (Just xs) = newSqlStmt SqlPostInstall obj xs
      postInstallAndUpgrade Nothing = Nothing
      postInstallAndUpgrade (Just xs) =
        newSqlStmt SqlPostInstallAndUpgrade obj xs
      priv p r =
        newSqlStmt SqlPriv obj $
        "GRANT " <> p <> " " <> sqlIdCode obj <> " TO " <> prefixedRole setup r
      privUsage = priv "USAGE ON SCHEMA"
      privSelectAll = priv "SELECT ON ALL TABLES IN SCHEMA"
      privInsertAll = priv "INSERT ON ALL TABLES IN SCHEMA"
      privUpdateAll = priv "UPDATE ON ALL TABLES IN SCHEMA"
      privDeleteAll = priv "DELETE ON ALL TABLES IN SCHEMA"
      privSequenceAll = priv "USAGE ON ALL SEQUENCES IN SCHEMA"
      privExecuteAll = priv "EXECUTE ON ALL FUNCTIONS IN SCHEMA"
      privAllAll d =
        map
          (\x -> x d)
          [ privUsage
          , privSelectAll
          , privInsertAll
          , privUpdateAll
          , privDeleteAll
          , privSequenceAll
          , privExecuteAll
          ]
