-- This file is part of HamSql
--
-- Copyright 2015-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Sequence where

--import Database.HamSql
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Sql
import Database.HamSql.Setup
import Database.YamSql

stmtsDropSequence :: SqlIdContentSqo -> [SqlStmt]
stmtsDropSequence x =
  [newSqlStmt SqlDropSequence x $ "DROP SEQUENCE " <> toSqlCode x]

instance ToSqlStmts (SqlContextSqo Sequence) where
  toSqlStmts = stmtsDeploySequence

stmtsDeploySequence :: SetupContext -> SqlContextSqo Sequence -> [SqlStmt]
stmtsDeploySequence _ obj@SqlContextSqo {sqlSqoObject = s} =
  [ newSqlStmt SqlCreateSequence obj $ "CREATE SEQUENCE" <-> sqlIdCode obj
  , newSqlStmt SqlAlterSequence obj $
    "ALTER SEQUENCE" <-> sqlIdCode obj <-> incrementBy (sequenceIncrement s) <->
    minValue (sequenceMinValue s) <->
    maxValue (sequenceMaxValue s) <->
    startValue (sequenceStartValue s) <->
    cache (sequenceCache s) <->
    cycled (sequenceCycle s) <->
    ownedByColumn (sequenceOwnedByColumn s)
  ]
  where
    incrementBy Nothing = "INCREMENT BY 1"
    incrementBy (Just i) = "INCREMENT BY " <> tshow i
    minValue Nothing = "NO MINVALUE"
    minValue (Just i) = "MINVALUE " <> tshow i
    maxValue Nothing = "NO MAXVALUE"
    maxValue (Just i) = "MAXVALUE " <> tshow i
    startValue Nothing = ""
    startValue (Just i) = "START WITH " <> tshow i
    cache Nothing = "CACHE 1"
    cache (Just i) = "CACHE " <> tshow i
    cycled Nothing = "NO CYCLE"
    cycled (Just False) = "NO CYCLE"
    cycled (Just True) = "CYCLE"
    ownedByColumn Nothing = "OWNED BY NONE"
    ownedByColumn (Just n) = "OWNED BY " <> toSqlCode n
