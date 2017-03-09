-- This file is part of HamSql
--
-- Copyright 2015-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Database.HamSql.Internal.Stmt.Sequence where

import Database.HamSql.Internal.Stmt.Basic

stmtsDropSequence :: SqlObj SQL_SEQUENCE SqlName -> [Maybe SqlStmt]
stmtsDropSequence x =
  [newSqlStmt SqlDropSequence x $ "DROP SEQUENCE " <> toSqlCode x]

instance ToSqlStmts (SqlContext (Schema, Sequence)) where
  toSqlStmts _ obj@(SqlContext (_, Sequence {..})) =
    [ newSqlStmt SqlCreateSequence obj $ "CREATE SEQUENCE" <-> sqlIdCode obj <-> startValue sequenceStartValue <-> conf
    , newSqlStmt SqlAlterSequence obj $
      "ALTER SEQUENCE" <-> sqlIdCode obj <-> conf
          , stmtCommentOn obj sequenceDescription
    ]
    where
      conf = incrementBy sequenceIncrement <->
              minValue sequenceMinValue <->
              maxValue sequenceMaxValue <->
              cache sequenceCache <->
              cycled sequenceCycle <->
              ownedByColumn sequenceOwnedByColumn

      startValue Nothing = ""
      startValue (Just i) = "START WITH " <> tshow i
      incrementBy Nothing = "INCREMENT BY 1"
      incrementBy (Just i) = "INCREMENT BY " <> tshow i
      minValue Nothing = "NO MINVALUE"
      minValue (Just i) = "MINVALUE " <> tshow i
      maxValue Nothing = "NO MAXVALUE"
      maxValue (Just i) = "MAXVALUE " <> tshow i
      cache Nothing = "CACHE 1"
      cache (Just i) = "CACHE " <> tshow i
      cycled Nothing = "NO CYCLE"
      cycled (Just False) = "NO CYCLE"
      cycled (Just True) = "CYCLE"
      ownedByColumn Nothing = "OWNED BY NONE"
      ownedByColumn (Just n) = "OWNED BY " <> toSqlCode n
