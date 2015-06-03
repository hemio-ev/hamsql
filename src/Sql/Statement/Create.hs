-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

module Sql.Statement.Create where

import Option
import Parser
import Parser.Basic
import Parser.Check
import Parser.Commons
import Parser.Domain
import Parser.Function
import Parser.Module
import Parser.Role
import Parser.Table
import Parser.Type
import Utils
import Sql

import Data.Maybe
import Data.List
import Data.String.Utils (replace)

emptyName = SqlName ""

stmtCommentOn :: SqlCode a => String -> a -> String -> SqlStatement
stmtCommentOn on obj com = SqlStmt SqlComment (SqlName $ toSql obj) $
  "COMMENT ON " ++ on ++ " " ++ toSql obj ++ " IS " ++ toSqlString com

sqlAddTransact :: [SqlStatement] -> [SqlStatement]
sqlAddTransact xs = 
  [ SqlStmt SqlUnclassified emptyName "BEGIN TRANSACTION" ] ++
  xs ++
  [ SqlStmt SqlUnclassified emptyName "COMMIT" ]

-- create database

sqlCreateDatabase :: Bool -> String -> [SqlStatement]
sqlCreateDatabase _ "" = err "Please specify a database in the connection URL"
sqlCreateDatabase deleteDatabase name = [
        sqlDelete deleteDatabase,
        SqlStmt SqlCreateDatabase name' $
          "CREATE DATABASE " ++ toSql name',
        SqlStmt SqlCreateDatabase name' 
          "ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON FUNCTIONS FROM PUBLIC"
    ]
  where
    sqlDelete True = SqlStmt SqlDropDatabase name' $
      "DROP DATABASE IF EXISTS " ++ toSql name'
    sqlDelete False = SqlStmtEmpty
    
    name' = SqlName name

-- Setup

getSetupStatements :: OptCommon -> Setup -> [SqlStatement]
getSetupStatements opts s = debug opts "stmtInstallSetup" $
  [ getStmt $ setupPreCode s ] ++ moduleStatements ++ [ getStmt $ setupPostCode s ]
  where
    moduleStatements = 
      concatMap (getModuleStatements opts s) (setupModuleData $ setupInternal s)
    getStmt (Just code) = SqlStmt SqlPreInstall emptyName code
    getStmt Nothing = SqlStmtEmpty
  
-- Module

getModuleStatements :: OptCommon -> Setup -> Module -> [SqlStatement]
getModuleStatements opts s m = debug opts "stmtCreateSchema" $
  [
    SqlStmt SqlCreateSchema (moduleName m) $ "CREATE SCHEMA IF NOT EXISTS " ++ toSql (moduleName m),
    postInst $ moduleExecPostInstall m,
    stmtCommentOn "schema" (moduleName m) (moduleDescription m)
  ] ++
  maybeMap (privUsage) (modulePrivUsage m) ++
  maybeMap (privSelectAll) (modulePrivSelectAll m) ++
  maybeMap (privInsertAll) (modulePrivInsertAll m) ++
  maybeMap (privUpdateAll) (modulePrivUpdateAll m) ++
  maybeMap (privDeleteAll) (modulePrivDeleteAll m) ++
  maybeMap (privSequenceAll) (modulePrivSequenceAll m) ++
  maybeMap (privExecuteAll) (modulePrivExecuteAll m) ++
  concat (maybeMap (privAllAll) (modulePrivAllAll m)) ++
  concat (maybeMap (getDomainStatements opts s m) (moduleDomains m)) ++
  concat (maybeMap (getTypeStatements opts s m) (moduleTypes m)) ++
  concat (maybeMap (getRoleStatements opts s) (moduleRoles m)) ++
  concat (maybeMap (getFunctionStatements opts s m) (moduleFunctions m)) ++
  concat (maybeMap (stmtsCreateTable opts s m) (moduleTables m))

  where
    postInst Nothing = SqlStmtEmpty
    postInst (Just xs) = SqlStmt SqlPostInstall emptyName xs

    priv :: String -> SqlName -> SqlStatement
    priv p r = SqlStmt SqlPriv r $ "GRANT " ++ p ++ " " ++ toSql (moduleName m) ++ " TO " ++ prefixedRole s r

    privUsage = priv "USAGE ON SCHEMA"
    privSelectAll = priv "SELECT ON ALL TABLES IN SCHEMA"
    privInsertAll = priv "INSERT ON ALL TABLES IN SCHEMA"
    privUpdateAll = priv "UPDATE ON ALL TABLES IN SCHEMA"
    privDeleteAll = priv "DELETE ON ALL TABLES IN SCHEMA"
    privSequenceAll = priv "USAGE ON ALL SEQUENCES IN SCHEMA"
    privExecuteAll = priv "EXECUTE ON ALL FUNCTIONS IN SCHEMA"
    privAllAll d = map (\x -> x d)
      [
        privUsage,
        privSelectAll,
        privInsertAll,
        privUpdateAll,
        privDeleteAll,
        privSequenceAll,
        privExecuteAll
      ]

-- Table

stmtsCreateTable :: OptCommon -> Setup -> Module -> Table -> [SqlStatement]
stmtsCreateTable opts setup m t = debug opts "stmtCreateTable" $
    [
    -- table with columns
    stmtCreateTable,
    -- table comment
    stmtCommentOn "TABLE" intName (tableDescription t)
    ] ++
    map stmtAddColumn (tableColumns t) ++
    map stmtAlterColumnType (tableColumns t) ++
    map stmtDropDefault (tableColumns t) ++
    map stmtAddColumnDefault (tableColumns t) ++
    map stmtAlterColumnNull (tableColumns t) ++
    concat (map stmtAddColumnCheck (tableColumns t)) ++
    maybeMap stmtCheck (tableChecks t) ++

    -- column comments
    map (\c -> stmtCommentOn "COLUMN"
            (intName <.> columnName c) 
            (columnDescription c)) (tableColumns t) ++
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
    map sqlAddForeignKey (tableColumns t) ++
    -- inheritance
    maybeMap sqlAddInheritance (tableInherits t) ++
    -- multi column unique constraints
    map sqlColumnUnique (tableColumns t) ++
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
            
        stmtAlterColumnType c
          | toSql (columnType c) == "SERIAL" = SqlStmtEmpty
          | otherwise = SqlStmt SqlAlterColumn intName $
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
        
--        CREATE SEQUENCE tablename_colname_seq;
--CREATE TABLE tablename (
--    colname integer NOT NULL DEFAULT nextval('tablename_colname_seq')
--);
--ALTER SEQUENCE tablename_colname_seq OWNED BY tablename.colname;
        
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

-- Function

getFunctionStatements :: OptCommon -> Setup -> Module -> Function -> [SqlStatement]
getFunctionStatements opts setup m f =
    stmtCreateFunction:
    sqlSetOwner (functionOwner f):
    stmtComment:
    map sqlStmtGrantExecute (maybeList $ functionPrivExecute f)

    where
        name = (moduleName m) <.> functionName f
      
        sqlStmtGrantExecute u = SqlStmt SqlPriv name $ sqlGrantExecute u
        sqlGrantExecute u = "GRANT EXECUTE ON FUNCTION \n" ++
            sqlFunctionIdentifier ++ "\nTO " ++ prefixedRole setup u

        stmtCreateFunction = SqlStmt SqlCreateFunction (functionName f) $
            "CREATE OR REPLACE FUNCTION " ++ sqlFunctionIdentifierDef ++
            "\n" ++
            "RETURNS " ++ toSql (functionReturns f) ++ sqlReturnsColumns (functionReturnsColumns f) ++
            "\nLANGUAGE " ++ sqlLanguage (functionLanguage f) ++
            "\nSECURITY " ++ sqlSecurity (functionSecurityDefiner f) ++
            "\nAS\n$BODY$\n" ++
                sqlBody ++
            "\n$BODY$\n"

        stmtComment = SqlStmt SqlComment (functionName f) $
          "COMMENT ON FUNCTION " ++ sqlFunctionIdentifier ++
          " IS " ++ toSqlString (functionDescription f)
            
        sqlSetOwner (Just o) = SqlStmt SqlPriv name $
            "ALTER FUNCTION " ++ sqlFunctionIdentifier ++
            "OWNER TO " ++ prefixedRole setup o
        sqlSetOwner Nothing = SqlStmtEmpty

        sqlFunctionIdentifierDef =
            toSql name
                ++ "(\n" ++
                join ",\n" (maybeMap sqlParameterDef (functionParameters f)) ++
                "\n)"

        sqlFunctionIdentifier =
            toSql name
                ++ "(\n" ++
                join ",\n" (maybeMap sqlParameter (functionParameters f)) ++
                "\n)"

        -- function parameter
        sqlParameter p = 
            " " ++ toSql(variableName p) ++ 
            " " ++ toSql(variableType p)

        sqlParameterDef p = 
            " " ++ toSql(variableName p) ++ 
            " " ++ toSql(variableType p)
            ++ sqlParamDefault (variableDefault p)
            where
            sqlParamDefault Nothing = ""
            sqlParamDefault (Just x) = " DEFAULT " ++ x 
       
        -- If function returns a table, use service for field definition 
        sqlReturnsColumns cs
         | toSql (functionReturns f) == "TABLE" = 
            " (\n" ++
            join ",\n" (maybeMap sqlReturnsColumn cs) ++
            ") "
         | otherwise = ""

        sqlReturnsColumn c = toSql (parameterName c) ++ " " ++ toSql(parameterType c)

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
            toSql (variableName v) ++ " " ++ toSql(variableType v) ++
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

getDomainStatements :: OptCommon -> Setup -> Module -> Domain -> [SqlStatement]
getDomainStatements opt _ m d = debug opt "stmtCreateDomain" $
 
  stmtCreateDomain
  :sqlDefault (domainDefault d)
  :(maybeMap sqlCheck (domainChecks d))

  --stmtCommentOn "DOMAIN" fullName (domainDescription d)

    where
    fullName = (moduleName m) <.> domainName d
    
    stmtCreateDomain = SqlStmt SqlCreateDomain fullName $
      "CREATE DOMAIN " ++ toSql fullName ++ " AS " ++ toSql (domainType d)
    
    sqlCheck :: Check -> SqlStatement
    sqlCheck c = SqlStmt SqlCreateCheckConstr fullName (
      "ALTER DOMAIN " ++ toSql fullName ++ " ADD" ++
      " CONSTRAINT " ++ toSql (name (checkName c)) ++
      " CHECK (" ++ checkCheck c ++ ")")

    sqlDefault Nothing = SqlStmtEmpty
    sqlDefault (Just def) = SqlStmt SqlAddDefault fullName $
      "ALTER DOMAIN " ++ toSql fullName ++ " SET DEFAULT " ++ def

    name a = SqlName "DOMAIN_" // domainName d // SqlName "__" // a

-- Types

getTypeStatements :: OptCommon -> Setup -> Module -> Type -> [SqlStatement]
getTypeStatements opt s m t =
  SqlStmt SqlCreateType fullName (
    "CREATE TYPE " ++ toSql fullName ++ " AS (" ++
    join ", " (map sqlElement (typeElements t)) ++ ")"
  ):
  stmtCommentOn "TYPE" fullName (typeDescription t)
  :[]
  
  -- ALTER TYPE name ALTER ATTRIBUTE attribute_name [ SET DATA ] TYPE data_type

  where
    fullName = (moduleName m) <.> typeName t
    sqlElement e = toSql (typeelementName e) ++ " " ++ toSql(typeelementType e)

-- Role

getRoleStatements :: OptCommon -> Setup -> Role -> [SqlStatement]
getRoleStatements opts setup r =
    SqlStmt SqlCreateRole (roleName r) sqlCreateRole:
    (stmtCommentOn "ROLE" (setupRolePrefix' setup // roleName r) (roleDescription r)):
    maybeMap sqlRoleMembership (roleMemberIn r)

    where
        sqlCreateRole = "CREATE ROLE " ++ prefix (roleName r) ++
            " " ++ sqlLogin (roleLogin r) ++
            sqlPassword (rolePassword r)

        sqlRoleMembership group = 
            SqlStmt SqlRoleMembership (roleName r) $
            "GRANT " ++ prefix group ++ " TO " ++ prefix (roleName r);
            
        sqlLogin (Just True) = "LOGIN"
        sqlLogin _           = "NOLOGIN"

        sqlPassword Nothing = ""
        sqlPassword (Just p) = " ENCRYPTED PASSWORD '" ++ p ++ "' "

        prefix role = prefixedRole setup role

prefixedRole :: Setup -> SqlName -> String
prefixedRole setup role = toSql (setupRolePrefix' setup // role)
