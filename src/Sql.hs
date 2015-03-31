module Sql where

import Option
import Parser
import Utils

import Data.Maybe
import Data.List
import Data.String.Utils (replace)

-- SQL statements

data SqlStatement = 
  SqlStmtPreInstall String |
  SqlStmtSchema String |
  SqlStmtTypeDef String |
  SqlStmtRoleDelete String |
  SqlStmtRoleDef String |
  SqlStmtRoleMembership String |
  SqlStmt String |
  SqlStmtFunc String |
  SqlStmtInherit String |
  SqlStmtCreatePrimaryKeyConstr String |
  SqlStmtCreateUniqueConstr String |
  SqlStmtCreateForeignKeyConstr String |
  SqlStmtCreateCheckConstr String |
  SqlStmtAddDefault String |
  SqlStmtPriv String |
  SqlStmtComment String |
  SqlStmtPostInstall String |
  SqlStmtEmpty
    deriving (Eq, Ord, Show)

afterDelete :: SqlStatement -> Bool
--afterDelete (SqlStmtRoleDef _) = True
--afterDelete (SqlStmtRoleMembership _) = True
afterDelete (SqlStmtFunc _) = True
afterDelete (SqlStmtCreatePrimaryKeyConstr _) = True
afterDelete (SqlStmtCreateUniqueConstr _) = True
afterDelete (SqlStmtCreateForeignKeyConstr _) = True
afterDelete (SqlStmtCreateCheckConstr _) = True
afterDelete (SqlStmtPriv _) = True
afterDelete (SqlStmtComment _) = True
--afterDelete (SqlStmtRoleDelete _) = True
afterDelete _ = False
    
statementTermin = ";\n"
instance SqlCode SqlStatement where
  toSql (SqlStmtEmpty) = "--"
  toSql (SqlStmtPreInstall xs) = xs ++ "\n"
  toSql (SqlStmtPostInstall xs) = xs ++ "\n"
  toSql stmt = termin $ sqlFromStmt stmt
   where
    termin [] = ""
    termin xs = xs ++ statementTermin
  (//) _ _ = undefined

sqlFromStmt :: SqlStatement -> String
sqlFromStmt (SqlStmtSchema x) = x
sqlFromStmt (SqlStmtTypeDef x) = x
sqlFromStmt (SqlStmtRoleDelete x) = x
sqlFromStmt (SqlStmtRoleDef x) = x
sqlFromStmt (SqlStmtRoleMembership x) = x
sqlFromStmt (SqlStmt x) = x
sqlFromStmt (SqlStmtFunc x) = x
sqlFromStmt (SqlStmtInherit x) = x
sqlFromStmt (SqlStmtCreatePrimaryKeyConstr x) = x
sqlFromStmt (SqlStmtCreateUniqueConstr x) = x
sqlFromStmt (SqlStmtCreateForeignKeyConstr x) = x
sqlFromStmt (SqlStmtCreateCheckConstr x) = x
sqlFromStmt (SqlStmtAddDefault x) = x
sqlFromStmt (SqlStmtPriv x) = x
sqlFromStmt (SqlStmtComment x) = x
sqlFromStmt (SqlStmtPostInstall x) = x


sqlPrinter :: [SqlStatement] -> String
sqlPrinter xs = join "" $ map toSql xs

toSqlString :: String -> String
toSqlString xs = "'" ++ replace "'" "''" xs ++ "'"
