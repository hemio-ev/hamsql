-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Table
  ( stmtsDropTable
  , stmtsDropTableConstr
  , stmtsDropTableColumn
  ) where

import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic
import Database.HamSql.Internal.Stmt.Sequence ()

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

stmtsDropTable :: SqlObj SQL_TABLE SqlName -> [Maybe SqlStmt]
stmtsDropTable t = [newSqlStmt SqlDropTable t $ "DROP TABLE" <-> toSqlCode t]

stmtsDropTableColumn :: SqlObj SQL_COLUMN (SqlName, SqlName) -> [Maybe SqlStmt]
stmtsDropTableColumn x@(SqlObj _ (t, c)) =
  [ newSqlStmt SqlDropTableColumn x $
    "ALTER TABLE" <-> toSqlCode t <-> "DROP COLUMN" <-> toSqlCode c
  ]

constrId
  :: Schema
  -> Table
  -> SqlName
  -> SqlObj SQL_TABLE_CONSTRAINT (SqlName, SqlName, SqlName)
constrId s t c = SqlObj SQL_TABLE_CONSTRAINT (schemaName s, tableName t, c)

-- TODO: prefix with table name
stmtCheck
  :: ToSqlId a
  => a -> Check -> Maybe SqlStmt
stmtCheck obj c =
  newSqlStmt SqlCreateCheckConstr obj $
  "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <>
  toSqlCode (checkName c) <>
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
          let constr = tableName table <> columnName c <> SqlName "key"
          in newSqlStmt SqlCreateUniqueConstr (constrId schema table constr) $
             "ALTER TABLE " <> tblId <> " ADD CONSTRAINT " <> toSqlCode constr <>
             " UNIQUE (" <>
             toSqlCode (columnName c) <>
             ")"
        | otherwise = Nothing
      -- NOT NULL
      stmtAlterColumnNull =
        stmtAlterColumn SqlColumnSetNull $
        if columnNull c == Just True
          then "DROP NOT NULL"
          else "SET NOT NULL"
      -- SET DATA TYPE
      stmtAlterColumnType =
        stmtAlterColumn SqlColumnSetType $
        "SET DATA TYPE " <> toSqlCode (columnType c)
      -- DROP DEFAULT
      stmtDropDefault = stmtAlterColumn SqlColumnSetDefault "DROP DEFAULT"
      -- SET DEFAULT
      stmtAddColumnDefault = columnDefault c >>= sqlDefault
        where
          sqlDefault d = stmtAlterColumn SqlColumnSetDefault $ "SET DEFAULT " <> d
      -- [CHECK]
      stmtsAddColumnCheck = maybeMap (stmtCheck tbl) (columnChecks c)
      -- FOREIGN KEY
      stmtAddForeignKey =
        case columnReferences c of
          Nothing -> Nothing
          (Just ref) ->
            let constr = tableName table <> columnName c <> SqlName "fkey"
            in newSqlStmt
                 SqlCreateForeignKeyConstr
                 (constrId schema table constr) $
               "ALTER TABLE" <-> tblId <-> "ADD CONSTRAINT" <-> toSqlCode constr <->
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
      tblId = sqlIdCode tbl
      tbl = SqlContext (schema, table)
      serialSequenceContext =
        SqlContext
          ( schema
          , Sequence
            -- sequenceName follows PostgreSQL internal convention
            { sequenceName = tableName table <> columnName c <> SqlName "seq"
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
        let constr = tableName t <> SqlName "pkey"
        in newSqlStmt SqlCreatePrimaryKeyConstr (constrId s t constr) $
           "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <> toSqlCode constr <>
           " PRIMARY KEY (" <>
           T.intercalate ", " (map toSqlCode ks) <>
           ")"
      -- TODO: allow empty name with "mconcat (uniquekeyColumns ks)"
      sqlUniqueConstr :: UniqueKey -> Maybe SqlStmt
      sqlUniqueConstr ks =
        let constr = tableName t <> uniquekeyName ks
        in newSqlStmt SqlCreateUniqueConstr (constrId s t constr) $
           "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <> toSqlCode constr <>
           " UNIQUE (" <>
           T.intercalate ", " (map toSqlCode (uniquekeyColumns ks)) <>
           ")"
      sqlAddForeignKey' :: ForeignKey -> Maybe SqlStmt
      sqlAddForeignKey' fk =
        let constr = tableName t <> foreignkeyName fk
        in newSqlStmt SqlCreateForeignKeyConstr (constrId s t constr) $
           "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <> toSqlCode constr <>
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
