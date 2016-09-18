-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Table where

import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic
import Database.HamSql.Internal.Stmt.Sequence

-- | Assuming that CASCADE will only cause other constraints to be deleted
-- | Required since foreign keys may depend on other keys
stmtsDropTableConstr :: SqlIdContentSqoObj -> [SqlStmt]
stmtsDropTableConstr x =
  [ newSqlStmt SqlDropTableConstr x $
    "ALTER TABLE" <-> sqlSqoIdCode x <-> "DROP CONSTRAINT IF EXISTS" <->
    sqlSqoObjIdCode x <->
    "CASCADE"
  ]

stmtsDropTableColumn :: SqlIdContentSqoObj -> [SqlStmt]
stmtsDropTableColumn x =
  [ newSqlStmt SqlDropTableColumn x $
    "ALTER TABLE" <-> sqlSqoIdCode x <-> "DROP COLUMN" <-> sqlSqoObjIdCode x
  ]

columnObj :: SqlContextSqo Table -> Column -> SqlContextSqoObj Table Column
columnObj x y =
  SqlContextSqoObj
  { sqlSqoObjectSchema = sqlSqoSchema x
  , sqlSqoObject1 = sqlSqoObject x
  , sqlSqoObject2 = y
  }

instance ToSqlStmts (SqlContextSqoObj Table Column) where
  toSqlStmts _ _ = []

instance ToSqlStmts (SqlContextSqo Table) where
  toSqlStmts = stmtsDeployTable

stmtsDeployTable :: SetupContext -> SqlContextSqo Table -> [SqlStmt]
stmtsDeployTable context@SetupContext {setupContextSetup = setup} obj@SqlContextSqo {sqlSqoSchema = m
                                                                                    ,sqlSqoObject = t}
                                                                  -- table with columns
 =
  [ stmtCreateTable
    -- table comment
  , stmtCommentOn "TABLE" obj (tableDescription t)
  ] ++
  map stmtAddColumn columns ++
  map stmtAlterColumnType columns ++
  map stmtDropDefault columns ++
  map stmtAddColumnDefault columns ++
  map stmtAlterColumnNull columns ++
  concatMap stmtAddColumnCheck columns ++
  concat (sequences (tableColumns t)) ++
  maybeMap stmtCheck (tableChecks t) ++
  -- column comments
  map (\c -> stmtCommentOn "COLUMN" (columnObj obj c) (columnDescription c)) columns ++
  -- grant rights to roles
  maybeMap (sqlGrant "SELECT") (tablePrivSelect t) ++
  maybeMap (sqlGrant "UPDATE") (tablePrivUpdate t) ++
  maybeMap (sqlGrant "INSERT") (tablePrivInsert t) ++
  maybeMap (sqlGrant "DELETE") (tablePrivDelete t) ++
  -- primary key
  [sqlAddPrimaryKey (tablePrimaryKey t)] ++
  -- mult column unique
  maybeMap sqlUniqueConstr (tableUnique t) ++
  -- single column FKs (references)
  map sqlAddForeignKey columns ++
  -- inheritance
  maybeMap sqlAddInheritance (tableInherits t) ++
  -- multi column unique constraints
  map sqlColumnUnique columns ++
  -- multi column FKs
  maybeMap sqlAddForeignKey' (tableForeignKeys t)
  where
    stmtCreateTable =
      newSqlStmt SqlCreateTable obj $
      "CREATE TABLE IF NOT EXISTS" <-> sqlIdCode obj <> " ()"
    stmtCheck c =
      newSqlStmt SqlCreateCheckConstr obj $
      "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <>
      constrName (checkName c) <>
      " CHECK (" <>
      checkCheck c <>
      ")"
    -- COLUMNS
    sqlAlterColumn c =
      "ALTER TABLE " <> sqlIdCode obj <> " ALTER COLUMN " <>
      toSqlCode (columnName c) <>
      " "
    stmtAddColumn c =
      newSqlStmt SqlAddColumn (columnObj obj c) $
      "ALTER TABLE" <-> sqlIdCode obj <-> "ADD COLUMN" <->
      toSqlCode (columnName c) <->
      toSqlCode (columnType c)
    stmtAlterColumnType c =
      newSqlStmt SqlAlterColumn obj $
      sqlAlterColumn c <> "SET DATA TYPE " <> toSqlCode (columnType c)
    stmtDropDefault c =
      newSqlStmt SqlDropColumnDefault obj $ sqlAlterColumn c <> "DROP DEFAULT"
    stmtAddColumnCheck c = maybeMap stmtCheck (columnChecks c)
    stmtAlterColumnNull c =
      newSqlStmt SqlAlterColumn obj $
      sqlAlterColumn c <> sqlSetNull (columnNull c)
      where
        sqlSetNull Nothing = "SET NOT NULL"
        sqlSetNull (Just False) = "SET NOT NULL"
        sqlSetNull (Just True) = "DROP NOT NULL"
    stmtAddColumnDefault c = sqlDefault (columnDefault c)
      where
        sqlDefault Nothing = SqlStmtEmpty
        sqlDefault (Just d) =
          newSqlStmt SqlAddDefault (columnObj obj c) $
          sqlAlterColumn c <> "SET DEFAULT " <> d
    -- SERIAL
    columns = map injectSerialParameters (tableColumns t)
    injectSerialParameters c
      | columnIsSerial c =
        c
        { columnType = SqlType "integer"
        , columnDefault =
          Just $ "nextval('" <> sqlIdCode (serialSqlContext c) <> "')"
        }
      | otherwise = c
    columnIsSerial c = toSqlCode (columnType c) == "SERIAL"
    -- do not change this, it is PostgreSQL internal convention
    serialSequenceName c =
      tableName t // SqlName "_" // columnName c // SqlName "_seq"
    sequences cs = map serial (filter columnIsSerial cs)
      where
        serial c = stmtsDeploySequence context (serialSqlContext c)
    serialSqlContext c =
      SqlContextSqo
      { sqlSqoSchema = m
      , sqlSqoObject =
        Sequence
        { sequenceName = serialSequenceName c
        , sequenceIncrement = Nothing
        , sequenceMinValue = Nothing
        , sequenceMaxValue = Nothing
        , sequenceStartValue = Nothing
        , sequenceCache = Nothing
        , sequenceCycle = Nothing
        , sequenceOwnedByColumn = Just $ SqlName $ sqlIdCode (columnObj obj c)
        }
      }
    -- PRIMARY KEY
    sqlAddPrimaryKey :: [SqlName] -> SqlStmt
    sqlAddPrimaryKey [] = SqlStmtEmpty
    sqlAddPrimaryKey ks =
      newSqlStmt SqlCreatePrimaryKeyConstr (constrId (SqlName "primary_key")) $
      "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <>
      constrName (SqlName "primary_key") <>
      " PRIMARY KEY (" <>
      T.intercalate ", " (map toSqlCode ks) <>
      ")"
    sqlUniqueConstr :: UniqueKey -> SqlStmt
    sqlUniqueConstr ks =
      newSqlStmt SqlCreateUniqueConstr (constrId (uniquekeyName ks)) $
      "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <>
      constrName (uniquekeyName ks) <>
      " UNIQUE (" <>
      T.intercalate ", " (map toSqlCode (uniquekeyColumns ks)) <>
      ")"
    --sqlCheck c =
    --    " CONSTRAINT " <> name (checkName c) <> " CHECK (" <> checkCheck c <> ")"
    sqlAddForeignKey :: Column -> SqlStmt
    sqlAddForeignKey Column {columnReferences = Nothing} = SqlStmtEmpty
    sqlAddForeignKey c@Column {columnReferences = (Just ref)} =
      newSqlStmt SqlCreateForeignKeyConstr (constrId (columnName c)) $
      "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <>
      constrName (columnName c) <>
      " FOREIGN KEY (" <>
      toSqlCode (columnName c) <>
      ")" <>
      " REFERENCES " <>
      toSqlCode' (init $ expSqlName ref) <>
      " (" <>
      toSqlCode (last $ expSqlName ref) <>
      ")" <>
      sqlOnRefUpdate (columnOnRefUpdate c) <>
      sqlOnRefDelete (columnOnRefDelete c)
    sqlAddForeignKey' :: ForeignKey -> SqlStmt
    sqlAddForeignKey' fk =
      newSqlStmt SqlCreateForeignKeyConstr (constrId (foreignkeyName fk)) $
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
      sqlOnRefUpdate (foreignkeyOnUpdate fk) <>
      sqlOnRefDelete (foreignkeyOnDelete fk)
    sqlOnRefUpdate Nothing = ""
    sqlOnRefUpdate (Just a) = " ON UPDATE " <> a
    sqlOnRefDelete Nothing = ""
    sqlOnRefDelete (Just a) = " ON DELETE " <> a
    sqlGrant right role =
      newSqlStmt
        SqlPriv
        obj
        ("GRANT " <> right <> " ON TABLE " <> toSqlCode (tableName t) <> " TO " <>
         prefixedRole setup role)
    sqlAddInheritance :: SqlName -> SqlStmt
    sqlAddInheritance n =
      newSqlStmt SqlAlterTable obj $
      "ALTER TABLE " <> sqlIdCode obj <> " INHERIT " <> toSqlCode n
    sqlColumnUnique c@Column {columnUnique = (Just True)} =
      newSqlStmt SqlCreateUniqueConstr (constrId (columnName c)) $
      "ALTER TABLE " <> sqlIdCode obj <> " ADD CONSTRAINT " <>
      constrName (columnName c) <>
      " UNIQUE (" <>
      toSqlCode (columnName c) <>
      ")"
    sqlColumnUnique _ = SqlStmtEmpty
    -- tools
    constrName a = toSqlCode (tableName t // SqlName "-" // a)
    constrId a =
      SqlIdContentSqoObj
        "TABLE-CONSTRAINT"
        (SqlName $ sqlIdCode obj)
        (SqlName $ constrName a)
