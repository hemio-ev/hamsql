-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

module SQL where

import Options
import Parser
import Utils

import Data.Maybe
import Data.List


-- SQL statements

data SqlStatement = 
  SqlStmtSchema String |
  SqlStmtTypeDef String |
  SqlStmtRoleDelete String |
  SqlStmtRoleDef String |
  SqlStmtRoleMembership String |
  SqlStmt String |
  SqlStmtInherit String |
  SqlStmtConstr String |
  SqlStmtPriv String |
  SqlStmtPostInstall String |
  SqlStmtEmpty
    deriving (Eq, Ord, Show)

statementTermin = ";\n"
instance SqlCode SqlStatement where
  toSql (SqlStmtEmpty) = ""
  toSql (SqlStmtSchema x) = x ++ statementTermin
  toSql (SqlStmtTypeDef x) = x ++ statementTermin
  toSql (SqlStmtRoleDelete x) = x ++ statementTermin
  toSql (SqlStmtRoleDef x) = x ++ statementTermin
  toSql (SqlStmtRoleMembership x) = x ++ statementTermin
  toSql (SqlStmt x) = x ++ statementTermin
  toSql (SqlStmtInherit x) = x ++ statementTermin
  toSql (SqlStmtConstr x) = x ++ statementTermin
  toSql (SqlStmtPriv x) = x ++ statementTermin
  toSql (SqlStmtPostInstall x) = x ++ statementTermin
  (//) _ _ = undefined


sqlPrinter :: [SqlStatement] -> String
sqlPrinter xs = join "" $ map toSql xs

sqlAddTransact :: [SqlStatement] -> [SqlStatement]
sqlAddTransact xs = 
  [ SqlStmt "BEGIN TRANSACTION" ] ++ xs ++ [ SqlStmt "COMMIT" ]

-- create database

sqlCreateDatabase :: String -> [SqlStatement]
sqlCreateDatabase name = [ SqlStmt $ "CREATE DATABASE " ++ toSql (SqlName name) ]

-- Setup

getSetupStatements :: Opt -> Setup -> [SqlStatement]
getSetupStatements opts s =
  [ getStmt $ setupPreCode s ] ++ moduleStatements ++ [ getStmt $ setupPostCode s ]
  where
    moduleStatements = 
      sort $ concatMap (getModuleStatements opts s) (setupModuleData $ setupInternal s)
    getStmt (Just code) = SqlStmt code
    getStmt Nothing = SqlStmtEmpty
  
-- Module

getModuleStatements :: Opt -> Setup -> Module -> [SqlStatement]
getModuleStatements opts s m =
  [ SqlStmtSchema $ "CREATE SCHEMA " ++ toSql (moduleName m) ] ++
  [ SqlStmtPostInstall . maybeList $ moduleExecPostInstall m ] ++
  concat (maybeMap (getDomainStatements opts) (moduleDomains m)) ++
  concat (maybeMap (getTypeStatements opts) (moduleTypes m)) ++
  concat (maybeMap (getRoleStatements opts s) (moduleRoles m)) ++
  concat (maybeMap (getFunctionStatements opts s) (moduleFunctions m)) ++
  concat (maybeMap (getTableStatements opts s) (moduleTables m))


-- Table

getTableStatements :: Opt -> Setup -> Table -> [SqlStatement]
getTableStatements opts setup t =
    SqlStmt sqlTable:
    maybeMap (sqlGrant "SELECT") (tablePrivSelect t) ++
    maybeMap (sqlGrant "UPDATE") (tablePrivUpdate t) ++
    maybeMap (sqlGrant "INSERT") (tablePrivInsert t) ++
    maybeMap (sqlGrant "DELETE") (tablePrivDelete t) ++
    map sqlAddForeignKey (tableColumns t) ++
    maybeMap sqlAddInheritance (tableInherits t) ++
    map sqlColumnUnique (tableColumns t) ++
    maybeMap sqlAddForeignKey' (tableForeignKeys t)

    where
        sqlTable =
         "CREATE TABLE " ++ toSql [ moduleName' t, tableName t ] ++ " (\n" ++
         join ",\n" (filter (/= "") (
            map sqlColumn (tableColumns t) ++
            maybeMap sqlCheck (tableChecks t) ++
            [ sqlPrimaryKeyConstraint $ map toSql (tablePrimaryKey t) ] ++
            maybeMap sqlUniqueConstraint (tableUnique t)
         )) ++
         "\n)"

        -- columns
        sqlColumn c@(Column {}) =
            "  " ++ toSql (columnName c) ++ " " ++
            columnType c ++ " " ++
            sqlNull (columnNull c) ++ " " ++
            sqlDefault (columnDefault c)
        sqlColumn _ = err "ColumnTemplates should not be present in SQL parsing"

        sqlNull Nothing        = "NOT NULL"
        sqlNull (Just True)    = "NULL"
        sqlNull (Just False)   = "NOT NULL"
        
        sqlDefault Nothing     = ""
        sqlDefault (Just d)    = "DEFAULT " ++ d
        
        -- constraints
        sqlPrimaryKeyConstraint :: [String] -> String
        sqlPrimaryKeyConstraint k =
            "  CONSTRAINT " ++ name (SqlName "primary_key") ++ " PRIMARY KEY (" ++ join ", " k ++ ")"

        -- TODO: Make the constraint name unique
        sqlUniqueConstraint :: [SqlName] -> String
        sqlUniqueConstraint k =
            "  CONSTRAINT " ++ name (SqlName "unique") ++ " UNIQUE (" ++ join ", " (map toSql k) ++ ")"

        sqlCheck c =
            "  CONSTRAINT " ++ name (checkName c) ++ " CHECK (" ++ checkCheck c ++ ")"

        sqlAddForeignKey :: Column -> SqlStatement
        sqlAddForeignKey c@(Column { columnReferences = Nothing }) =
          SqlStmtEmpty
        sqlAddForeignKey c@(Column { columnReferences = (Just ref) }) =
          SqlStmtConstr $
            "ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
            " ADD CONSTRAINT " ++ name (columnName c) ++
            " FOREIGN KEY (" ++ toSql (columnName c) ++ ")" ++
            " REFERENCES " ++ toSql (init $ expSqlName ref) ++
            " (" ++ toSql (last $ expSqlName ref) ++ ")" ++
            sqlOnRefUpdate (columnOnRefUpdate c) ++
            sqlOnRefDelete (columnOnRefDelete c)
            
        sqlAddForeignKey' :: ForeignKey -> SqlStatement
        sqlAddForeignKey' fk = SqlStmtConstr $
            "ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
            " ADD CONSTRAINT " ++ name (tableName t // foreignkeyName fk) ++
            " FOREIGN KEY (" ++ join ", " (map toSql (foreignkeyColumns fk)) ++ ")" ++
            " REFERENCES " ++ toSql (foreignkeyRefTable fk) ++
            " (" ++ join ", " (map toSql $ foreignkeyRefColumns fk) ++ ")" ++
            sqlOnRefUpdate (foreignkeyOnDelete fk) ++
            sqlOnRefDelete (foreignkeyOnUpdate fk)              
                
        sqlOnRefUpdate Nothing = ""
        sqlOnRefUpdate (Just a) = " ON UPDATE " ++ a
        sqlOnRefDelete Nothing = ""
        sqlOnRefDelete (Just a) = " ON UPDATE " ++ a

        sqlGrant right role = SqlStmtConstr ("GRANT " ++ right ++ " ON TABLE " ++
            toSql (tableName t) ++ " TO " ++ prefixedRole setup role)

        sqlAddInheritance :: SqlName -> SqlStatement
        sqlAddInheritance n = 
                SqlStmtInherit $ "ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
                 " INHERIT " ++ toSql n
                
        sqlColumnUnique c@(Column{ columnUnique = (Just True) }) = SqlStmtConstr $
          "ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
            " ADD CONSTRAINT " ++ name (columnName c) ++ 
            " UNIQUE (" ++ toSql (columnName c) ++ ")"
        sqlColumnUnique _ = SqlStmtEmpty
        
        -- tools

        name a = toSql (SqlName "TABLE_" // tableName t // SqlName "__" // a)

        moduleName' :: Table -> SqlName
        moduleName' t' = moduleName $ tableParentModule $ tableInternal t'
        
-- Function

getFunctionStatements :: Opt -> Setup -> Function -> [SqlStatement]
getFunctionStatements opts setup f =
    SqlStmt sqlCreateFunction:
    SqlStmtPriv (sqlSetOwner (functionOwner f)):
    map sqlStmtGrantExecute (maybeList $ functionPrivExecute f)

    where
        sqlStmtGrantExecute u = SqlStmt $ sqlGrantExecute u
        sqlGrantExecute u = "GRANT EXECUTE ON FUNCTION \n" ++
            sqlFunctionIdentifier ++ "\nTO " ++ prefixedRole setup u

        sqlCreateFunction =
            "CREATE OR REPLACE FUNCTION " ++ sqlFunctionIdentifier ++
            "\n" ++
            "RETURNS " ++ functionReturn f ++ sqlReturnColumns (functionReturnColumns f) ++
            "\nLANGUAGE " ++ sqlLanguage (functionLanguage f) ++
            "\nSECURITY " ++ sqlSecurity (functionSecurityDefiner f) ++
            "\nAS\n$BODY$\n" ++
                sqlBody ++
            "\n$BODY$\n"

        sqlSetOwner (Just o) =
            "ALTER FUNCTION " ++ sqlFunctionIdentifier ++
            "OWNER TO " ++ prefixedRole setup o
        sqlSetOwner Nothing =
            ""

        sqlFunctionIdentifier =
            toSql [moduleName $ functionParentModule $ functionInternal f ,
              functionName f]
                ++ "(\n" ++
                join ",\n" (maybeMap sqlParameter (functionParameters f)) ++
                "\n)"

        -- function parameter
        sqlParameter p = " " ++ toSql(variableName p) ++ " " ++ variableType p
       
        -- If function returns a table, use service for field definition 
        sqlReturnColumns cs
         | upper (functionReturn f) == "TABLE" = 
            " (\n" ++
            join ",\n" (maybeMap sqlReturnColumn cs) ++
            ") "
         | otherwise = ""

        sqlReturnColumn c = toSql (parameterName c) ++ " " ++ parameterType c

        -- If language not defined, use service for variable definitions
        sqlBody 
            | isNothing (functionLanguage f) =
                "DECLARE\n" ++
                sqlVariables (functionVariables f) ++
                "BEGIN\n" ++
                body ++
                "\nEND;"
            | otherwise =
                body
            where
                body =
                    join "\n" preludes ++
                    functionBody f ++
                    join "\n" postludes

                preludes :: [String]
                preludes = catMaybes$maybeMap functiontplBodyPrelude (functionTemplateData f)

                postludes :: [String]
                postludes = catMaybes$maybeMap functiontplBodyPostlude (functionTemplateData f)
    
    
        -- Service for variable definitions
        sqlVariables Nothing = ""
        sqlVariables (Just vs) = join "" (map sqlVariable vs)

        sqlVariable v =
            toSql (variableName v) ++ " " ++ variableType v ++
            sqlVariableDefault (variableDefault v) ++ ";\n"

        sqlVariableDefault Nothing = ""
        sqlVariableDefault (Just d) = " := " ++ d

        -- SECURITY
        sqlSecurity (Just True) = "DEFINER"
        sqlSecurity _           = "INVOKER"

        -- LANGUAGE
        sqlLanguage Nothing     = "plpgsql"
        sqlLanguage (Just lang) = lang

-- Domains

getDomainStatements :: Opt -> Domain -> [SqlStatement]
getDomainStatements opt d = [SqlStmtTypeDef $
    "CREATE DOMAIN " ++
    toSql [ moduleName $ domainParentModule $ domainInternal d , domainName d ]
    ++ " AS " ++ domainType d ++
    sqlDefault (domainDefault d) ++
    join "\n" (maybeMap sqlCheck (domainChecks d))
    ]

    where
    sqlCheck c =
        " CONSTRAINT " ++ toSql (name (checkName c)) ++
        " CHECK (" ++ checkCheck c ++ ")"
    sqlDefault Nothing = ""
    sqlDefault (Just def) = " DEFAULT " ++ def

    name a = SqlName "DOMAIN_" // domainName d // SqlName "__" // a

-- Types

getTypeStatements :: Opt -> Type -> [SqlStatement]
getTypeStatements opt t = [SqlStmtTypeDef $
    "CREATE TYPE " ++ 
    toSql [ moduleName $ typeParentModule $ typeInternal t , typeName t ]  
    ++ " AS (" ++
    join ", " (map sqlElement (typeElements t))
    ]

    where
    sqlElement e = toSql (typeelementName e) ++ " " ++ typeelementType e

-- Role

getRoleStatements :: Opt -> Setup -> Role -> [SqlStatement]
getRoleStatements opts setup r =
    [SqlStmtRoleDef sqlCreateRole] ++
    maybeMap sqlRoleMembership (roleMemberIn r)

    where
        sqlCreateRole = "CREATE ROLE " ++ prefix (roleName r) ++
            " " ++ sqlLogin (roleLogin r) ++
            sqlPassword (rolePassword r)

        sqlRoleMembership group = 
            SqlStmtRoleMembership $
            "GRANT " ++ prefix group ++ " TO " ++ prefix (roleName r);
            
        sqlLogin (Just True) = "LOGIN"
        sqlLogin _           = "NOLOGIN"

        sqlPassword Nothing = ""
        sqlPassword (Just p) = " ENCRYPTED PASSWORD '" ++ p ++ "' "

        prefix role = prefixedRole setup role

prefixedRole :: Setup -> SqlName -> String
prefixedRole setup role = toSql (setupRolePrefix' setup // role)

