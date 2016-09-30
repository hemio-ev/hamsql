-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Table
  ( stmtsDropTableConstr
  , stmtsDropTableColumn
  ) where

import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic
import Database.HamSql.Internal.Stmt.Sequence

-- | Assuming that CASCADE will only cause other constraints to be deleted.
-- | Required since foreign keys may depend on other keys.
stmtsDropTableConstr :: SqlObj SQL_TABLE_CONSTRAINT (SqlName, SqlName, SqlName)
                     -> [Maybe SqlStmt]
stmtsDropTableConstr x@(SqlObj _ (s, t, c)) =
  [ newSqlStmt SqlDropTableConstr x $
    "ALTER TABLE" <-> toSqlCode (s <.> t) <-> "DROP CONSTRAINT IF EXISTS" <->
    toSqlCode c <->
    "CASCADE"
  ]

stmtsDropTableColumn :: SqlObj SQL_COLUMN (SqlName, SqlName) -> [Maybe SqlStmt]
stmtsDropTableColumn x@(SqlObj _ (t, c)) =
  [ newSqlStmt SqlDropTableColumn x $
    "ALTER TABLE" <-> toSqlCode t <-> "DROP COLUMN" <-> toSqlCode c
  ]

-- tools
constrName
  :: ToSqlCode a
  => a -> Text
constrName a = toSqlCode a

-- FIXME: fixme
constrId
  :: Schema
  -> Table
  -> SqlName
  -> SqlObj SQL_TABLE_CONSTRAINT (SqlName, SqlName, SqlName)
constrId s t c =
  SqlObj SQL_TABLE_CONSTRAINT (schemaName s, tableName t, tableName t <> c)

stmtCheck
  :: ToSqlId a
  => a -> Check -> Maybe SqlStmt
stmtCheck obj c =
  newSqlStmt SqlCreateCheckConstr obj $
  "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <>
  constrName (checkName c) <>
  " CHECK (" <>
  checkCheck c <>
  ")"

instance ToSqlStmts (SqlContext (Schema, Table, Column)) where
  toSqlStmts context obj@(SqlContext (schema, table, rawColumn)) =
    [ stmtAddColumn
    , stmtAlterColumnType
    , stmtDropDefault
    , stmtAddColumnDefault
    , stmtAlterColumnNull
    , stmtCommentOn obj (columnDescription c)
    , stmtAddForeignKey
    , stmtColumnUnique
    ] ++
    stmtsSerialSequence ++ stmtsAddColumnCheck
  -- ADD COLUMN
    where
      stmtAddColumn =
        newSqlStmt SqlAddColumn obj $
        "ALTER TABLE" <-> tblId <-> "ADD COLUMN" <-> toSqlCode (columnName c) <->
        toSqlCode (columnType c)
      -- UNIQUE
      stmtColumnUnique
        | columnUnique c == Just True =
          newSqlStmt
            SqlCreateUniqueConstr
            (constrId schema table (columnName c <> SqlName "key")) $
          "ALTER TABLE " <> tblId <> " ADD CONSTRAINT " <>
          toSqlCode (tableName table <> columnName c <> SqlName "key") <>
          " UNIQUE (" <>
          toSqlCode (columnName c) <>
          ")"
        | otherwise = Nothing
      -- NOT NULL
      stmtAlterColumnNull =
        stmtAlterColumn SqlAlterColumn $
        if columnNull c == Just True
          then "DROP NOT NULL"
          else "SET NOT NULL"
      -- SET DATA TYPE
      stmtAlterColumnType =
        stmtAlterColumn SqlAlterColumn $
        "SET DATA TYPE " <> toSqlCode (columnType c)
      -- DROP DEFAULT
      stmtDropDefault = stmtAlterColumn SqlDropColumnDefault "DROP DEFAULT"
      -- SET DEFAULT
      stmtAddColumnDefault = columnDefault c >>= sqlDefault
        where
          sqlDefault d = stmtAlterColumn SqlAddDefault $ "SET DEFAULT " <> d
      -- [CHECK]
      stmtsAddColumnCheck = maybeMap (stmtCheck obj) (columnChecks c)
      -- FOREIGN KEY
      stmtAddForeignKey =
        case columnReferences c of
          Nothing -> Nothing
          (Just ref) ->
            newSqlStmt
              SqlCreateForeignKeyConstr
              (constrId schema table (columnName c)) $
            "ALTER TABLE" <-> sqlIdCode obj <-> "ADD CONSTRAINT" <->
            constrName (columnName c) <->
            "FOREIGN KEY (" <>
            toSqlCode (columnName c) <>
            ")" <->
            "REFERENCES" <->
            toSqlCode' (init $ expSqlName ref) <->
            "(" <>
            toSqlCode (last $ expSqlName ref) <>
            ")" <>
            maybePrefix " ON UPDATE " (columnOnRefUpdate c) <>
            maybePrefix " ON DELETE " (columnOnRefDelete c)
      -- CREATE SEQUENCE (for type SERIAL)
      stmtsSerialSequence
        | columnIsSerial = toSqlStmts context serialSequenceContext
        | otherwise = [Nothing]
      -- Helpers
      stmtAlterColumn t x =
        newSqlStmt t obj $
        "ALTER TABLE " <> tblId <> " ALTER COLUMN " <> toSqlCode (columnName c) <>
        " " <>
        x
      columnIsSerial = toSqlCode (columnType rawColumn) == "SERIAL"
      c
        | columnIsSerial =
          rawColumn
          { columnType = SqlType "integer"
          , columnDefault =
            Just $
            "nextval('" <> toSqlCode (sqlId serialSequenceContext) <> "')"
          }
        | otherwise = rawColumn
      tblId = toSqlCode $ schemaName schema <.> tableName table
      serialSequenceContext =
        SqlContext
          ( schema
          , Sequence
            -- sequenceName follows PostgreSQL internal convention
            { sequenceName = tableName table <> columnName c <> SqlName "_seq"
            , sequenceIncrement = Nothing
            , sequenceMinValue = Nothing
            , sequenceMaxValue = Nothing
            , sequenceStartValue = Nothing
            , sequenceCache = Nothing
            , sequenceCycle = Nothing
            , sequenceOwnedByColumn = Just $ SqlName $ sqlIdCode obj
            })

instance ToSqlStmts (SqlContext (Schema, Table)) where
  toSqlStmts SetupContext {setupContextSetup = setup} obj@(SqlContext (s, t)) =
    [ stmtCreateTable
      -- table comment
    , stmtCommentOn obj (tableDescription t)
    ] ++
    maybeMap (stmtCheck obj) (tableChecks t) ++
    -- grant rights to roles
    maybeMap (sqlGrant "SELECT") (tablePrivSelect t) ++
    maybeMap (sqlGrant "UPDATE") (tablePrivUpdate t) ++
    maybeMap (sqlGrant "INSERT") (tablePrivInsert t) ++
    maybeMap (sqlGrant "DELETE") (tablePrivDelete t) ++
    -- primary key
    [sqlAddPrimaryKey (tablePrimaryKey t)] ++
    -- mult column unique
    maybeMap sqlUniqueConstr (tableUnique t) ++
    -- inheritance
    maybeMap sqlAddInheritance (tableInherits t) ++
    -- multi column FKs
    maybeMap sqlAddForeignKey' (tableForeignKeys t)
    where
      stmtCreateTable =
        newSqlStmt SqlCreateTable obj $
        "CREATE TABLE IF NOT EXISTS" <-> sqlIdCode obj <> " ()"
      -- PRIMARY KEY
      sqlAddPrimaryKey :: [SqlName] -> Maybe SqlStmt
      sqlAddPrimaryKey [] = Nothing
      sqlAddPrimaryKey ks =
        newSqlStmt SqlCreatePrimaryKeyConstr (constrId s t (SqlName "pkey")) $
        "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <>
        toSqlCode (tableName t <> SqlName "pkey") <>
        " PRIMARY KEY (" <>
        T.intercalate ", " (map toSqlCode ks) <>
        ")"
      sqlUniqueConstr :: UniqueKey -> Maybe SqlStmt
      sqlUniqueConstr ks =
        newSqlStmt SqlCreateUniqueConstr (constrId s t (uniquekeyName ks)) $
        "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <>
        constrName (uniquekeyName ks) <>
        " UNIQUE (" <>
        T.intercalate ", " (map toSqlCode (uniquekeyColumns ks)) <>
        ")"
      --sqlCheck c =
      --    " CONSTRAINT " <> name (checkName c) <> " CHECK (" <> checkCheck c <> ")"
      sqlAddForeignKey' :: ForeignKey -> Maybe SqlStmt
      sqlAddForeignKey' fk =
        newSqlStmt SqlCreateForeignKeyConstr (constrId s t (foreignkeyName fk)) $
        "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <>
        constrName (foreignkeyName fk) <>
        " FOREIGN KEY (" <>
        T.intercalate ", " (map toSqlCode (foreignkeyColumns fk)) <>
        ")" <>
        " REFERENCES " <>
        toSqlCode (foreignkeyRefTable fk) <>
        " (" <>
        T.intercalate ", " (map toSqlCode $ foreignkeyRefColumns fk) <>
        ")" <>
        maybePrefix " ON UPDATE " (foreignkeyOnUpdate fk) <>
        maybePrefix " ON DELETE " (foreignkeyOnDelete fk)
      sqlGrant right role =
        newSqlStmt
          SqlPriv
          obj
          ("GRANT " <> right <> " ON TABLE " <> toSqlCode (tableName t) <>
           " TO " <>
           prefixedRole setup role)
      sqlAddInheritance :: SqlName -> Maybe SqlStmt
      sqlAddInheritance n =
        newSqlStmt SqlAlterTable obj $
        "ALTER TABLE " <> sqlIdCode obj <> " INHERIT " <> toSqlCode n
