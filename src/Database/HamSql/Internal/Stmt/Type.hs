-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Type where

import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic

stmtsDropType :: SqlObj SQL_TYPE SqlName -> [Maybe SqlStmt]
stmtsDropType t = [newSqlStmt SqlDropType t $ "DROP TYPE" <-> toSqlCode t]

instance ToSqlStmts (SqlContext (Schema, Type)) where
  toSqlStmts _ obj@(SqlContext (_, t)) =
    [ newSqlStmt SqlCreateType obj $
      "CREATE TYPE" <-> sqlIdCode obj <-> "AS (" <>
      T.intercalate ", " (map sqlElement (_typeElements t)) <>
      ")"
    , stmtCommentOn obj (typeDescription t)
    ]
  -- ALTER TYPE name ALTER ATTRIBUTE attribute_name [ SET DATA ] TYPE data_type
    where
      sqlElement e =
        toSqlCode (typeelementName e) <-> toSqlCode (_typeelementType e)
