module Sql.Statement.Drop where

import Sql
import Parser

stmtDropRole :: String -> SqlStatement
stmtDropRole role = SqlStmt SqlDropRole (SqlName role) $ "DROP ROLE \"" ++ role ++ "\""

stmtDropFunction :: String -> String -> String -> SqlStatement
stmtDropFunction schema function args = SqlStmt SqlDropFunction (SqlName "") $
      "DROP FUNCTION " ++ toSql(SqlName $ schema ++ "." ++ function) ++
      "(" ++ args ++ ") CASCADE"
      
stmtDropTableConstraint schema table constraint = SqlStmt SqlDropTableConstraint
  (SqlName "") $
      "ALTER TABLE " ++ toSql(SqlName $ schema ++ "." ++ table) ++
      " DROP CONSTRAINT IF EXISTS " ++ toSql(SqlName constraint) ++ " CASCADE"
      
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