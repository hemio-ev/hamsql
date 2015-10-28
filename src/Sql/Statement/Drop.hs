module Sql.Statement.Drop where

import Sql
import Parser.Basic
import Utils

-- ROLE

stmtDropRole :: SqlName -> SqlStatement
stmtDropRole role = SqlStmt SqlDropRole role $ "DROP ROLE " ++ toSql role

-- FUNCTION

stmtDropFunction :: (SqlName, SqlName, [SqlType]) -> SqlStatement
stmtDropFunction (schema, function, args) =
  SqlStmtFunction SqlDropFunction function args $
      "DROP FUNCTION " ++ toSql(schema <.> function) ++
      "(" ++ (join ", " (map toSql args)) ++ ")"

-- TABLE

stmtDropTable :: SqlName -> SqlStatement
stmtDropTable t = SqlStmt SqlDropTable t $
  "DROP TABLE " ++ toSql t

stmtDropTableColumn :: (SqlName, SqlName) -> SqlStatement
stmtDropTableColumn (t, c) = SqlStmt SqlDropColumn (t <.> c) $
  "ALTER TABLE " ++ toSql t ++ " DROP COLUMN " ++ toSql c

stmtDropTableConstraint :: (SqlName, SqlName, SqlName) -> SqlStatement
stmtDropTableConstraint (schema, table, constraint) = SqlStmt SqlDropTableConstraint
  (schema <.> table <.> constraint) $
      "ALTER TABLE " ++ toSql (schema <.> table) ++
      " DROP CONSTRAINT IF EXISTS " ++ toSql constraint ++
      -- Assuming that CASCADE will only cause other constraints to be deleted
      -- Required since foreign keys may depend on other keys
      " CASCADE"

-- DOMAIN

stmtDropDomain :: SqlName -> SqlStatement
stmtDropDomain d = SqlStmt SqlDropDomain d $
  "DROP DOMAIN " ++ toSql d
  
stmtDropDomainConstraint :: (SqlName, SqlName, SqlName) -> SqlStatement
stmtDropDomainConstraint (schema, domain, constraint) = SqlStmt SqlDropDomainConstraint
  (schema <.> domain) $
      "ALTER DOMAIN " ++ toSql (schema <.> domain) ++
      " DROP CONSTRAINT " ++ toSql constraint

-- TYPE
  
stmtDropType :: SqlName -> SqlStatement
stmtDropType t = SqlStmt SqlDropType t $
  "DROP TYPE " ++ toSql t

