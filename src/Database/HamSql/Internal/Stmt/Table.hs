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
stmtsDropTableConstr ::
     SqlObj SQL_TABLE_CONSTRAINT (SqlName, SqlName) -> [Maybe SqlStmt]
stmtsDropTableConstr x@(SqlObj _ (tbl, c)) =
  [ newSqlStmt SqlDropTableConstr x $
    "ALTER TABLE" <-> toSqlCode tbl <-> "DROP CONSTRAINT" <-> toSqlCode c <->
    "CASCADE"
  ]

stmtsDropTable :: SqlObj SQL_TABLE SqlName -> [Maybe SqlStmt]
stmtsDropTable t = [newSqlStmt SqlDropTable t $ "DROP TABLE" <-> toSqlCode t]

stmtsDropTableColumn :: SqlObj SQL_COLUMN (SqlName, SqlName) -> [Maybe SqlStmt]
stmtsDropTableColumn x@(SqlObj _ (t, c)) =
  [ newSqlStmt SqlDropTableColumn x $
    "ALTER TABLE" <-> toSqlCode t <-> "DROP COLUMN" <-> toSqlCode c
  ]

constrId ::
     Schema
  -> Table
  -> SqlName
  -> SqlObj SQL_TABLE_CONSTRAINT (SqlName, SqlName)
constrId s t c = SqlObj SQL_TABLE_CONSTRAINT (schemaName s <.> tableName t, c)

-- TODO: prefix with table name
stmtCheck :: (Schema, Table) -> Check -> [Maybe SqlStmt]
stmtCheck (s, t) c =
  let x =
        SqlObj SQL_TABLE_CONSTRAINT (schemaName s <.> tableName t, checkName c)
      obj = (schemaName s, tableName t)
   in [ newSqlStmt SqlCreateTableCheckConstr x $
        "ALTER TABLE " <>
        toSqlCode obj <>
        " ADD CONSTRAINT " <>
        toSqlCode (checkName c) <> " CHECK (" <> checkCheck c <> ")"
      , newSqlStmt SqlComment x $
        "COMMENT ON CONSTRAINT" <-> toSqlCode (checkName c) <-> "ON" <->
        toSqlCode obj <->
        "IS" <->
        toSqlCodeString (checkDescription c)
      ]

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
        toSqlCode (_columnType c)
      -- UNIQUE
      stmtColumnUnique
        | columnUnique c == Just True =
          let constr = tableName table <> columnName c <> SqlName "key"
           in newSqlStmt SqlCreateUniqueConstr (constrId schema table constr) $
              "ALTER TABLE " <>
              tblId <>
              " ADD CONSTRAINT " <>
              toSqlCode constr <> " UNIQUE (" <> toSqlCode (columnName c) <> ")"
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
        "SET DATA TYPE " <> toSqlCode (_columnType c)
      -- DROP DEFAULT
      stmtDropDefault = stmtAlterColumn SqlColumnSetDefault "DROP DEFAULT"
      -- SET DEFAULT
      stmtAddColumnDefault = columnDefault c >>= sqlDefault
        where
          sqlDefault d =
            stmtAlterColumn SqlColumnSetDefault $ "SET DEFAULT " <> d
      -- [CHECK]
      stmtsAddColumnCheck =
        concat $ maybeMap (stmtCheck (schema, table)) (columnChecks c)
      -- FOREIGN KEY
      stmtAddForeignKey =
        case columnReferences c of
          Nothing -> Nothing
          (Just ref) ->
            let constr = tableName table <> columnName c <> SqlName "fkey"
             in newSqlStmt
                  SqlCreateForeignKeyConstr
                  (constrId schema table constr) $
                "ALTER TABLE" <-> tblId <-> "ADD CONSTRAINT" <->
                toSqlCode constr <->
                "FOREIGN KEY (" <>
                toSqlCode (columnName c) <>
                ")" <-> "REFERENCES" <-> toSqlCode' (init $ expSqlName ref) <->
                "(" <>
                toSqlCode (last $ expSqlName ref) <>
                ")" <>
                maybePrefix " ON UPDATE " (columnOnRefUpdate c) <>
                maybePrefix " ON DELETE " (columnOnRefDelete c)
      -- CREATE SEQUENCE (for type SERIAL)
      stmtsSerialSequence
        | isJust columnIsSerial = toSqlStmts context serialSequenceContext
        | otherwise = [Nothing]
      -- Helpers
      stmtAlterColumn t x =
        newSqlStmt t obj $
        "ALTER TABLE " <>
        tblId <> " ALTER COLUMN " <> toSqlCode (columnName c) <> " " <> x
      columnIsSerial =
        let serialKey = T.toLower $ toSqlCode $ _columnType rawColumn
         in lookup
              serialKey
              [ ("smallserial", "smallint")
              , ("serial", "integer")
              , ("bigserial", "bigint")
              ]
      c =
        case columnIsSerial of
          Just sType ->
            rawColumn
              { _columnType = SqlType sType
              , columnDefault =
                  Just $
                  "nextval('" <>
                  toSqlCode (sqlId serialSequenceContext) <> "'::regclass)"
              }
          Nothing -> rawColumn
      tblId = sqlIdCode tbl
      tbl = SqlContext (schema, table)
      serialSequenceContext =
        SqlContext
          ( schema
          , Sequence
            -- sequenceName follows PostgreSQL internal convention
              { sequenceName = tableName table <> columnName c <> SqlName "seq"
              , sequenceDescription = ""
              , sequenceIncrement = Nothing
              , sequenceMinValue = Nothing
              , sequenceMaxValue = Nothing
              , sequenceStartValue = Nothing
              , sequenceCache = Nothing
              , sequenceCycle = Nothing
              , sequenceOwnedByColumn = Just $ SqlName $ sqlIdCode obj
              })

indexName :: SqlName -> [SqlName] -> SqlName -> Maybe IndexName -> SqlName
indexName t keys s Nothing = mconcat ([t] ++ keys ++ [s])
indexName t _ s (Just k) =
  case k of
    (IndexNameUnprefixed n) -> t <> n <> s
    IndexNamePrefixed {indexnamePrefixed = n} -> n

instance ToSqlStmts (SqlContext (Schema, Table)) where
  toSqlStmts SetupContext {setupContextSetup = setup} obj@(SqlContext (s, t)) =
    [ stmtCreateTable
      -- table comment
    , stmtCommentOn obj (tableDescription t)
    ] ++
    concat (maybeMap (stmtCheck (s, t)) (tableChecks t)) ++
    -- grant rights to roles
    [ sqlGrant p r
    | g <- fromMaybe [] (tableGrant t)
    , r <- grantRole g
    , p <- grantPrivilege g
    ] ++
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
            "ALTER TABLE " <>
            sqlIdCode obj <>
            " ADD CONSTRAINT " <>
            toSqlCode constr <>
            " PRIMARY KEY (" <> T.intercalate ", " (map toSqlCode ks) <> ")"
      -- TODO: allow empty name with "mconcat (uniquekeyColumns ks)"
      sqlUniqueConstr :: Abbr [SqlName] UniqueConstraint -> Maybe SqlStmt
      sqlUniqueConstr ks' =
        let ks =
              case ks' of
                LongForm ko -> ko
                ShortForm xs ->
                  UniqueConstraint
                    { uniqueconstraintName = Nothing
                    , uniqueconstraintColumns = xs
                    }
            constr =
              indexName
                (tableName t)
                (uniqueconstraintColumns ks)
                (SqlName "key")
                (uniqueconstraintName ks)
         in newSqlStmt SqlCreateUniqueConstr (constrId s t constr) $
            "ALTER TABLE " <>
            sqlIdCode obj <>
            " ADD CONSTRAINT " <>
            toSqlCode constr <>
            " UNIQUE (" <>
            T.intercalate ", " (map toSqlCode (uniqueconstraintColumns ks)) <>
            ")"
      sqlAddForeignKey' :: ForeignKey -> Maybe SqlStmt
      sqlAddForeignKey' fk =
        let constr =
              indexName
                (tableName t)
                (foreignkeyColumns fk)
                (SqlName "fkey")
                (foreignkeyName fk)
            refColumns =
              fromMaybe (foreignkeyColumns fk) (foreignkeyRefColumns fk)
         in newSqlStmt SqlCreateForeignKeyConstr (constrId s t constr) $
            "ALTER TABLE " <>
            sqlIdCode obj <>
            " ADD CONSTRAINT " <>
            toSqlCode constr <>
            " FOREIGN KEY (" <>
            T.intercalate ", " (map toSqlCode (foreignkeyColumns fk)) <>
            ")" <>
            " REFERENCES " <>
            toSqlCode (foreignkeyRefTable fk) <>
            " (" <>
            T.intercalate ", " (map toSqlCode refColumns) <>
            ")" <>
            maybePrefix " ON UPDATE " (foreignkeyOnUpdate fk) <>
            maybePrefix " ON DELETE " (foreignkeyOnDelete fk)
      sqlGrant right role =
        newSqlStmt
          SqlPriv
          obj
          ("GRANT " <>
           right <>
           " ON TABLE " <> sqlIdCode obj <> " TO " <> prefixedRole setup role)
      sqlAddInheritance :: SqlName -> Maybe SqlStmt
      sqlAddInheritance n =
        newSqlStmt SqlAlterTable obj $
        "ALTER TABLE " <> sqlIdCode obj <> " INHERIT " <> toSqlCode n
