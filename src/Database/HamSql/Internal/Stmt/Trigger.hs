-- This file is part of HamSql
--
-- Copyright 2017 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Trigger where

import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic
import Database.HamSql.Internal.Stmt.Function ()

stmtsDropTrigger :: SqlObj SQL_TRIGGER (SqlName, SqlName) -> [Maybe SqlStmt]
stmtsDropTrigger x@(SqlObj _ (tbl, trig)) =
  [ newSqlStmt SqlDropTrigger x $
    "DROP TRIGGER" <-> toSqlCode trig <-> "ON" <-> toSqlCode tbl
  ]

instance ToSqlStmts (SqlContext (Schema, Table, Trigger)) where
  toSqlStmts _ obj@(SqlContext (s, tabl, t)) = [triggerStmt, triggerComment]
    where
      triggerComment =
        newSqlStmt SqlComment obj $
        "COMMENT ON TRIGGER " <> toSqlCode (triggerName t) <> " ON " <>
        sqlIdCode (SqlContext (s, tabl)) <>
        " IS " <>
        toSqlCodeString (triggerDescription t)
      triggerStmt =
        newSqlStmt SqlCreateTrigger obj $
        "CREATE TRIGGER " <> toSqlCode (triggerName t) <> " " <> triggerMoment t <>
        " " <>
        T.intercalate " OR " (triggerEvents t) <>
        " ON " <>
        sqlIdCode (SqlContext (s, tabl)) <>
        " FOR EACH " <>
        triggerForEach t <>
        condition (triggerCondition t) <>
        " EXECUTE PROCEDURE " <>
        triggerExecute t
      condition Nothing = ""
      condition (Just x) = " WHEN (" <> x <> ") "
