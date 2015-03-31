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
