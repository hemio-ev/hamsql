module Sql.Statement.Drop where

import Sql
import Parser.Basic
import Utils

stmtDropRole :: SqlName -> SqlStatement
stmtDropRole role = SqlStmt SqlDropRole role $ "DROP ROLE " ++ toSql role

stmtDropFunction :: SqlName -> SqlName -> [SqlType] -> SqlStatement
stmtDropFunction schema function args =
  SqlStmtFunction SqlDropFunction function args $
      "DROP FUNCTION " ++ toSql(schema <.> function) ++
      "(" ++ (join ", " (map toSql args)) ++ ")"
      --" CASCADE"
      
stmtDropTableConstraint schema table constraint = SqlStmt SqlDropTableConstraint
  (SqlName "") $
      "ALTER TABLE " ++ toSql(SqlName $ schema ++ "." ++ table) ++
      " DROP CONSTRAINT IF EXISTS " ++ toSql(SqlName constraint) -- ++ " CASCADE"
      
stmtDropDomainConstraint schema domain constraint = SqlStmt SqlDropDomainConstraint
  (SqlName schema <.> SqlName domain) $
      "ALTER DOMAIN " ++ toSql(SqlName $ schema ++ "." ++ domain) ++
      " DROP CONSTRAINT " ++ toSql(SqlName constraint) ++ ""

  
stmtDropTable :: SqlName -> SqlStatement
stmtDropTable t = SqlStmt SqlDropTable t $
  "DROP TABLE " ++ toSql t

stmtDropTableColumn :: (SqlName, SqlName) -> SqlStatement
stmtDropTableColumn (t, c) = SqlStmt SqlDropColumn (t <.> c) $
  "ALTER TABLE " ++ toSql t ++ " DROP COLUMN " ++ toSql c

stmtDropDomain :: SqlName -> SqlStatement
stmtDropDomain d = SqlStmt SqlDropDomain d $
  "DROP DOMAIN " ++ toSql d
  
  
stmtDropType :: SqlName -> SqlStatement
stmtDropType t = SqlStmt SqlDropType t $
  "DROP TYPE " ++ toSql t
