module Sql.Statement.Commons where

import Parser
import Parser.Basic
import Sql

stmtCommentOn :: SqlCode a => String -> a -> String -> SqlStatement
stmtCommentOn on obj com = SqlStmt SqlComment (SqlName $ toSql obj) $
  "COMMENT ON " ++ on ++ " " ++ toSql obj ++ " IS " ++ toSqlString com

prefixedRole :: Setup -> SqlName -> String
prefixedRole setup role = toSql (setupRolePrefix' setup // role)

