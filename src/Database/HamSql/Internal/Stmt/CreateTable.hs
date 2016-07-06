-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.CreateTable where

import qualified Data.Text as T

--import Database.HamSql
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Sql
import Database.HamSql.Internal.Stmt.Commons
import Database.HamSql.Internal.Stmt.CreateSequence
import Database.HamSql.Setup
import Database.YamSql

createTable :: OptCommon -> Setup -> Schema -> Table -> [SqlStatement]
createTable opts setup m t = debug opts "stmtCreateTable" $
    [
    -- table with columns
    stmtCreateTable,
    -- table comment
    stmtCommentOn "TABLE" intName (tableDescription t)
    ] ++
    map stmtAddColumn columns ++
    map stmtAlterColumnType columns ++
    map stmtDropDefault columns ++
    map stmtAddColumnDefault columns ++
    map stmtAlterColumnNull columns ++
    concat (map stmtAddColumnCheck columns) ++
    concat (sequences (tableColumns t)) ++
    maybeMap stmtCheck (tableChecks t) ++

    -- column comments
    map (\c -> stmtCommentOn "COLUMN"
            (intName <.> columnName c)
            (columnDescription c)) columns ++
    -- grant rights to roles
    maybeMap (sqlGrant "SELECT") (tablePrivSelect t) ++
    maybeMap (sqlGrant "UPDATE") (tablePrivUpdate t) ++
    maybeMap (sqlGrant "INSERT") (tablePrivInsert t) ++
    maybeMap (sqlGrant "DELETE") (tablePrivDelete t) ++
    -- primary key
    [sqlAddPrimaryKey (tablePrimaryKey t)] ++
    -- mult column unique
    maybeMap sqlUniqueConstraint (tableUnique t) ++
    -- single column FKs (references)
    map sqlAddForeignKey columns ++
    -- inheritance
    maybeMap sqlAddInheritance (tableInherits t) ++
    -- multi column unique constraints
    map sqlColumnUnique columns ++
    -- multi column FKs
    maybeMap sqlAddForeignKey' (tableForeignKeys t)

    where
        intName = (schemaName m) <.> tableName t

        stmtCreateTable = SqlStmt SqlCreateTable intName $
          "CREATE TABLE " <> toSql intName <> " ()"

        stmtCheck c = SqlStmt SqlAddTableContraint intName $
          "ALTER TABLE " <> toSql intName <>
          " ADD CONSTRAINT " <> name (checkName c) <> " CHECK (" <> checkCheck c <> ")"

        -- COLUMNS

        sqlAlterColumn c@(Column {}) =
            "ALTER TABLE " <> toSql intName <>
            " ALTER COLUMN " <> toSql (columnName c) <> " "
        sqlAlterColumn _ = err "ColumnTemplates should not be present in SQL parsing"

        stmtAddColumn c@(Column {}) = SqlStmt SqlAddColumn (intName <.> columnName c) $
            "ALTER TABLE " <> toSql intName <>
            " ADD COLUMN " <> toSql (columnName c) <> " " <> toSql (columnType c)

        stmtAlterColumnType c = SqlStmt SqlAlterColumn intName $
            sqlAlterColumn c <> "SET DATA TYPE " <> toSql (columnType c)

        stmtDropDefault c = SqlStmt SqlDropColumnDefault intName $
          sqlAlterColumn c <> "DROP DEFAULT"

        stmtAddColumnCheck c = maybeMap stmtCheck (columnChecks c)

        stmtAlterColumnNull c = SqlStmt SqlAlterColumn intName $
            sqlAlterColumn c <> sqlSetNull (columnNull c)
          where
            sqlSetNull Nothing = "SET NOT NULL"
            sqlSetNull (Just False) = "SET NOT NULL"
            sqlSetNull (Just True) = "DROP NOT NULL"

        stmtAddColumnDefault c = sqlDefault (columnDefault c)
         where
          sqlDefault Nothing     = SqlStmtEmpty
          sqlDefault (Just d)    = SqlStmt SqlAddDefault (intName <.> columnName c) $
            sqlAlterColumn c <> "SET DEFAULT " <> d

        -- SERIAL

        columns = map injectSerialParameters (tableColumns t)

        injectSerialParameters c
          | columnIsSerial c = c {
              columnType = SqlType "integer",
              columnDefault = Just $
                "nextval('" <> toSql (schemaName m <.> serialSequenceName c) <> "')"
            }
          | otherwise = c

        columnIsSerial c = toSql (columnType c) == "SERIAL"
        -- do not change this, it is PostgreSQL internal convention
        serialSequenceName c =
          tableName t // SqlName "_" // columnName c // SqlName "_seq"

        sequences cs = map serial (filter columnIsSerial cs)
          where
            serial c = createSequence opts setup m $ Sequence {
              sequenceName = serialSequenceName c,
              sequenceIncrement   = Nothing,
              sequenceMinValue    = Nothing,
              sequenceMaxValue    = Nothing,
              sequenceStartValue  = Nothing,
              sequenceCache       = Nothing,
              sequenceCycle       = Nothing,
              sequenceOwnedByColumn = Just $ intName <.> columnName c
            }

        -- PRIMARY KEY

        sqlAddPrimaryKey :: [SqlName] -> SqlStatement
        sqlAddPrimaryKey [] = SqlStmtEmpty
        sqlAddPrimaryKey ks = SqlStmt SqlCreatePrimaryKeyConstr intName $
          "ALTER TABLE " <> toSql intName <>
          " ADD CONSTRAINT " <> name (SqlName "primary_key") <>
          " PRIMARY KEY (" <> T.intercalate ", " (map toSql ks) <> ")"

        sqlUniqueConstraint :: UniqueKey -> SqlStatement
        sqlUniqueConstraint ks = SqlStmt SqlCreateUniqueConstr intName $
          "ALTER TABLE " <> toSql intName <>
          " ADD CONSTRAINT " <> name (uniquekeyName ks) <>
          " UNIQUE (" <> T.intercalate ", " (map toSql (uniquekeyColumns ks)) <> ")"

        --sqlCheck c =
        --    " CONSTRAINT " <> name (checkName c) <> " CHECK (" <> checkCheck c <> ")"

        sqlAddForeignKey :: Column -> SqlStatement
        sqlAddForeignKey (Column { columnReferences = Nothing }) =
          SqlStmtEmpty
        sqlAddForeignKey c@(Column { columnReferences = (Just ref) }) =
          SqlStmt SqlCreateForeignKeyConstr intName $
            "ALTER TABLE " <> toSql intName <>
            " ADD CONSTRAINT " <> name (columnName c) <>
            " FOREIGN KEY (" <> toSql (columnName c) <> ")" <>
            " REFERENCES " <> toSql' (init $ expSqlName ref) <>
            " (" <> toSql (last $ expSqlName ref) <> ")" <>
            sqlOnRefUpdate (columnOnRefUpdate c) <>
            sqlOnRefDelete (columnOnRefDelete c)

        sqlAddForeignKey' :: ForeignKey -> SqlStatement
        sqlAddForeignKey' fk = SqlStmt SqlCreateForeignKeyConstr intName $
            "ALTER TABLE " <> toSql intName <>
            " ADD CONSTRAINT " <> name (foreignkeyName fk) <>
            " FOREIGN KEY (" <> T.intercalate ", " (map toSql (foreignkeyColumns fk)) <> ")" <>
            " REFERENCES " <> toSql (foreignkeyRefTable fk) <>
            " (" <> T.intercalate ", " (map toSql $ foreignkeyRefColumns fk) <> ")" <>
            sqlOnRefUpdate (foreignkeyOnUpdate fk) <>
            sqlOnRefDelete (foreignkeyOnDelete fk)

        sqlOnRefUpdate Nothing = ""
        sqlOnRefUpdate (Just a) = " ON UPDATE " <> a
        sqlOnRefDelete Nothing = ""
        sqlOnRefDelete (Just a) = " ON DELETE " <> a

        sqlGrant right role = SqlStmt SqlPriv intName ("GRANT " <> right <> " ON TABLE " <>
            toSql (tableName t) <> " TO " <> prefixedRole setup role)

        sqlAddInheritance :: SqlName -> SqlStatement
        sqlAddInheritance n = SqlStmt SqlAlterTable intName $
          "ALTER TABLE " <> toSql intName <> " INHERIT " <> toSql n

        sqlColumnUnique c@(Column{ columnUnique = (Just True) }) = SqlStmt SqlCreateUniqueConstr intName $
          "ALTER TABLE " <> toSql intName <>
            " ADD CONSTRAINT " <> name (columnName c) <>
            " UNIQUE (" <> toSql (columnName c) <> ")"
        sqlColumnUnique _ = SqlStmtEmpty

        -- tools

        name a = toSql (tableName t // SqlName "-" // a)

