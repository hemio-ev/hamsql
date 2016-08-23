-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Type where

import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic

stmtsDeployType :: OptCommon -> Setup -> Schema -> Type -> [SqlStatement]
stmtsDeployType _ _ m t =
  SqlStmt SqlCreateType fullName (
    "CREATE TYPE" <-> toSql fullName <-> "AS (" <>
    T.intercalate ", " (map sqlElement (typeElements t)) <> ")"
  ):
  stmtCommentOn "TYPE" fullName (typeDescription t)
  :[]

  -- ALTER TYPE name ALTER ATTRIBUTE attribute_name [ SET DATA ] TYPE data_type

  where
    fullName = (schemaName m) <.> typeName t
    sqlElement e = toSql (typeelementName e) <-> toSql(typeelementType e)

