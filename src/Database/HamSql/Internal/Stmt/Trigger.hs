-- This file is part of HamSql
--
-- Copyright 2017 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Trigger where

import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic
import Database.HamSql.Internal.Stmt.Function ()

instance ToSqlStmts (SqlContext (Schema, Table, Trigger)) where
  toSqlStmts context obj@(SqlContext (s, tabl, t)) =
    toSqlStmts context (SqlContext (s, triggerFunction)) ++
    map triggerStmt [tableName tabl]
    where
      triggerFunction =
        Function
        { functionName = triggerName t
        , functionDescription = triggerDescription t
        , functionReturns = SqlType "trigger"
        , functionParameters = Nothing
        , functionTemplates = Nothing
        , functionTemplateData = Nothing
        , functionReturnsColumns = Nothing
        , functionVariables = triggerVariables t
      -- TODO: trigger owner?
        , functionPrivExecute = Just []
        , functionSecurityDefiner = Just True
      -- TODO: trigger owner?
        , functionOwner = Nothing
        , functionLanguage = triggerLanguage t
        , functionBody = triggerBody t
        }
      triggerStmt tbl =
        newSqlStmt SqlCreateTrigger obj $
        "CREATE TRIGGER " <> toSqlCode (triggerName t) <> " " <> triggerMoment t <>
        " " <>
        T.intercalate " OR " (triggerEvents t) <>
        " ON " <>
        toSqlCode tbl <>
        " FOR EACH " <>
        triggerForEach t <>
        condition (triggerCondition t) <>
        " EXECUTE PROCEDURE " <>
        sqlIdCode obj <>
        "()"
      condition Nothing = ""
      condition (Just x) = " WHEN " <> x <> " "
