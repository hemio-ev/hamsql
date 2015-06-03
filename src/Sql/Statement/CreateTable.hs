module Sql.Statement.CreateTable where

import Option
import Parser
import Parser.Basic
import Parser.Check
import Parser.Module
import Parser.Sequence
import Parser.Table
import Sql
import Sql.Statement.Commons
import Sql.Statement.CreateSequence
import Utils

createTable :: OptCommon -> Setup -> Module -> Table -> [SqlStatement]
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
        intName = (moduleName m) <.> tableName t 
      
        stmtCreateTable = SqlStmt SqlCreateTable intName $
          "CREATE TABLE " ++ toSql intName ++ " ()"
              
        stmtCheck c = SqlStmt SqlAddTableContraint intName $
          "ALTER TABLE " ++ toSql intName ++
          " ADD CONSTRAINT " ++ name (checkName c) ++ " CHECK (" ++ checkCheck c ++ ")"
                
        -- COLUMNS

        sqlAlterColumn c@(Column {}) =
            "ALTER TABLE " ++ toSql intName ++
            " ALTER COLUMN " ++ toSql (columnName c) ++ " "
        sqlAlterColumn _ = err "ColumnTemplates should not be present in SQL parsing"
      
        stmtAddColumn c@(Column {}) = SqlStmt SqlAddColumn (intName <.> columnName c) $
            "ALTER TABLE " ++ toSql intName ++
            " ADD COLUMN " ++ toSql (columnName c) ++ " " ++ toSql (columnType c)
            
        stmtAlterColumnType c = SqlStmt SqlAlterColumn intName $
            sqlAlterColumn c ++ "SET DATA TYPE " ++ toSql (columnType c)
        
        stmtDropDefault c = SqlStmt SqlDropColumnDefault intName $
          sqlAlterColumn c ++ "DROP DEFAULT"
        
        stmtAddColumnCheck c = maybeMap stmtCheck (columnChecks c)
        
        stmtAlterColumnNull c = SqlStmt SqlAlterColumn intName $
            sqlAlterColumn c ++ sqlSetNull (columnNull c) 
          where
            sqlSetNull Nothing = "SET NOT NULL"
            sqlSetNull (Just False) = "SET NOT NULL"
            sqlSetNull (Just True) = "DROP NOT NULL"
        
        stmtAddColumnDefault c = sqlDefault (columnDefault c)
         where
          sqlDefault Nothing     = SqlStmtEmpty
          sqlDefault (Just d)    = SqlStmt SqlAddDefault (intName <.> columnName c) $
            sqlAlterColumn c ++ "SET DEFAULT " ++ d

        -- SERIAL

        columns = map injectSerialParameters (tableColumns t)
        
        injectSerialParameters c
          | columnIsSerial c = c {
              columnType = SqlType "integer",
              columnDefault = Just $
                "nextval('" ++ toSql (moduleName m <.> serialSequenceName c) ++ "')" 
            }
          | otherwise = c
          
        columnIsSerial c = toSql (columnType c) == "SERIAL"
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
        sqlAddPrimaryKey ks = SqlStmt SqlCreatePrimaryKeyConstr intName $
          "ALTER TABLE " ++ toSql intName ++
          " ADD CONSTRAINT " ++ name (SqlName "primary_key") ++ 
          " PRIMARY KEY (" ++ join ", " (map toSql ks) ++ ")"
            
        -- TODO: Make the constraint name unique
        sqlUniqueConstraint :: [SqlName] -> SqlStatement
        sqlUniqueConstraint ks = SqlStmt SqlCreateUniqueConstr intName $
          "ALTER TABLE " ++ toSql intName ++
          " ADD CONSTRAINT " ++ name (SqlName "unique") ++
          " UNIQUE (" ++ join ", " (map toSql ks) ++ ")"

        sqlCheck c =
            " CONSTRAINT " ++ name (checkName c) ++ " CHECK (" ++ checkCheck c ++ ")"

        sqlAddForeignKey :: Column -> SqlStatement
        sqlAddForeignKey c@(Column { columnReferences = Nothing }) =
          SqlStmtEmpty
        sqlAddForeignKey c@(Column { columnReferences = (Just ref) }) =
          SqlStmt SqlCreateForeignKeyConstr intName $
            "ALTER TABLE " ++ toSql intName ++
            " ADD CONSTRAINT " ++ name (columnName c) ++
            " FOREIGN KEY (" ++ toSql (columnName c) ++ ")" ++
            " REFERENCES " ++ toSql' (init $ expSqlName ref) ++
            " (" ++ toSql (last $ expSqlName ref) ++ ")" ++
            sqlOnRefUpdate (columnOnRefUpdate c) ++
            sqlOnRefDelete (columnOnRefDelete c)
            
        sqlAddForeignKey' :: ForeignKey -> SqlStatement
        sqlAddForeignKey' fk = SqlStmt SqlCreateForeignKeyConstr intName $
            "ALTER TABLE " ++ toSql intName ++
            " ADD CONSTRAINT " ++ name (tableName t // foreignkeyName fk) ++
            " FOREIGN KEY (" ++ join ", " (map toSql (foreignkeyColumns fk)) ++ ")" ++
            " REFERENCES " ++ toSql (foreignkeyRefTable fk) ++
            " (" ++ join ", " (map toSql $ foreignkeyRefColumns fk) ++ ")" ++
            sqlOnRefUpdate (foreignkeyOnUpdate fk) ++
            sqlOnRefDelete (foreignkeyOnDelete fk)              
                
        sqlOnRefUpdate Nothing = ""
        sqlOnRefUpdate (Just a) = " ON UPDATE " ++ a
        sqlOnRefDelete Nothing = ""
        sqlOnRefDelete (Just a) = " ON DELETE " ++ a

        sqlGrant right role = SqlStmt SqlPriv intName ("GRANT " ++ right ++ " ON TABLE " ++
            toSql (tableName t) ++ " TO " ++ prefixedRole setup role)

        sqlAddInheritance :: SqlName -> SqlStatement
        sqlAddInheritance n = SqlStmt SqlAlterTable intName $
          "ALTER TABLE " ++ toSql intName ++ " INHERIT " ++ toSql n
                
        sqlColumnUnique c@(Column{ columnUnique = (Just True) }) = SqlStmt SqlCreateUniqueConstr intName $
          "ALTER TABLE " ++ toSql intName ++
            " ADD CONSTRAINT " ++ name (columnName c) ++ 
            " UNIQUE (" ++ toSql (columnName c) ++ ")"
        sqlColumnUnique _ = SqlStmtEmpty
        
        -- tools

        name a = toSql (SqlName "TABLE_" // tableName t // SqlName "_" // a)

