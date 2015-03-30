-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

module SQL where

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

stmtCommentOn :: SqlCode a => String -> a -> String -> SqlStatement
stmtCommentOn on obj com = SqlStmtComment $
  "COMMENT ON " ++ on ++ " " ++ toSql obj ++ " IS " ++ toSqlString com

sqlAddTransact :: [SqlStatement] -> [SqlStatement]
sqlAddTransact xs = 
  [ SqlStmt "BEGIN TRANSACTION" ] ++ xs ++ [ SqlStmt "COMMIT" ]

-- create database

sqlCreateDatabase :: Bool -> String -> [SqlStatement]
sqlCreateDatabase _ "" = err "Please specify a database in the connection URL"
sqlCreateDatabase deleteDatabase name = [
        sqlDelete deleteDatabase,
        SqlStmt $ "CREATE DATABASE " ++ toSql (SqlName name),
        SqlStmt "ALTER DEFAULT PRIVILEGES REVOKE EXECUTE ON FUNCTIONS FROM PUBLIC"
    ]
  where
    sqlDelete True = SqlStmt $ "DROP DATABASE IF EXISTS " ++ toSql (SqlName name)
    sqlDelete False = SqlStmtEmpty

-- Setup

getSetupStatements :: OptCommon -> Setup -> [SqlStatement]
getSetupStatements opts s =
  [ getStmt $ setupPreCode s ] ++ moduleStatements ++ [ getStmt $ setupPostCode s ]
  where
    moduleStatements = 
      sort $ concatMap (getModuleStatements opts s) (setupModuleData $ setupInternal s)
    getStmt (Just code) = SqlStmtPreInstall code
    getStmt Nothing = SqlStmtEmpty
  
-- Module

getModuleStatements :: OptCommon -> Setup -> Module -> [SqlStatement]
getModuleStatements opts s m =
  [
    SqlStmtSchema $ "CREATE SCHEMA " ++ toSql (moduleName m),
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
  concat (maybeMap (getDomainStatements opts) (moduleDomains m)) ++
  concat (maybeMap (getTypeStatements opts) (moduleTypes m)) ++
  concat (maybeMap (getRoleStatements opts s) (moduleRoles m)) ++
  concat (maybeMap (getFunctionStatements opts s) (moduleFunctions m)) ++
  concat (maybeMap (getTableStatements opts s) (moduleTables m))

  where
    postInst Nothing = SqlStmtEmpty
    postInst (Just xs) = SqlStmtPostInstall xs

    priv :: String -> SqlName -> SqlStatement
    priv p r = SqlStmtPriv ("GRANT " ++ p ++ " " ++ toSql (moduleName m) ++ " TO " ++ prefixedRole s r)

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

getTableStatements :: OptCommon -> Setup -> Table -> [SqlStatement]
getTableStatements opts setup t =
    [
    -- table with columns
    SqlStmt sqlTable,
    -- table comment
    stmtCommentOn "TABLE" [ moduleName' t, tableName t ] (tableDescription t)
    ] ++
    -- column comments
    map (\c -> stmtCommentOn "COLUMN"
            [ moduleName' t, tableName t, columnName c] 
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
    maybeMap sqlAddForeignKey' (tableForeignKeys t) ++
    map sqlColumnDefault (tableColumns t)

    where
        sqlTable =
         "CREATE TABLE " ++ toSql [ moduleName' t, tableName t ] ++ " (\n" ++
         join ",\n" (filter (/= "") (
            map sqlColumn (tableColumns t) ++
            maybeMap sqlCheck (tableChecks t)
         )) ++
         "\n)"

        -- columns
        sqlColumn c@(Column {}) =
            "  " ++ toSql (columnName c) ++ " " ++
            toSql (columnType c) ++ " " ++
            sqlNull (columnNull c) ++ 
            join "" (maybeMap sqlCheck (columnChecks c))
        sqlColumn _ = err "ColumnTemplates should not be present in SQL parsing"

        sqlNull Nothing        = "NOT NULL"
        sqlNull (Just True)    = "NULL"
        sqlNull (Just False)   = "NOT NULL"
        
        sqlColumnDefault c@(Column {}) = sqlDefault (columnDefault c)
         where
          sqlDefault Nothing     = SqlStmtEmpty
          sqlDefault (Just d)    = SqlStmtAddDefault
            ("ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
            " ALTER COLUMN " ++ toSql (columnName c) ++ " SET DEFAULT " ++ d)
        
        sqlAddPrimaryKey :: [SqlName] -> SqlStatement
        sqlAddPrimaryKey ks = SqlStmtCreatePrimaryKeyConstr $
          "ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
          " ADD CONSTRAINT " ++ name (SqlName "primary_key") ++ 
          " PRIMARY KEY (" ++ join ", " (map toSql ks) ++ ")"
            
        -- TODO: Make the constraint name unique
        sqlUniqueConstraint :: [SqlName] -> SqlStatement
        sqlUniqueConstraint ks = SqlStmtCreateUniqueConstr $
          "ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
          " ADD CONSTRAINT " ++ name (SqlName "unique") ++
          " UNIQUE (" ++ join ", " (map toSql ks) ++ ")"

        sqlCheck c =
            " CONSTRAINT " ++ name (checkName c) ++ " CHECK (" ++ checkCheck c ++ ")"

        sqlAddForeignKey :: Column -> SqlStatement
        sqlAddForeignKey c@(Column { columnReferences = Nothing }) =
          SqlStmtEmpty
        sqlAddForeignKey c@(Column { columnReferences = (Just ref) }) =
          SqlStmtCreateForeignKeyConstr $
            "ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
            " ADD CONSTRAINT " ++ name (columnName c) ++
            " FOREIGN KEY (" ++ toSql (columnName c) ++ ")" ++
            " REFERENCES " ++ toSql (init $ expSqlName ref) ++
            " (" ++ toSql (last $ expSqlName ref) ++ ")" ++
            sqlOnRefUpdate (columnOnRefUpdate c) ++
            sqlOnRefDelete (columnOnRefDelete c)
            
        sqlAddForeignKey' :: ForeignKey -> SqlStatement
        sqlAddForeignKey' fk = SqlStmtCreateForeignKeyConstr $
            "ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
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

        sqlGrant right role = SqlStmtPriv ("GRANT " ++ right ++ " ON TABLE " ++
            toSql (tableName t) ++ " TO " ++ prefixedRole setup role)

        sqlAddInheritance :: SqlName -> SqlStatement
        sqlAddInheritance n = 
                SqlStmtInherit $ "ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
                 " INHERIT " ++ toSql n
                
        sqlColumnUnique c@(Column{ columnUnique = (Just True) }) = SqlStmtCreateUniqueConstr $
          "ALTER TABLE " ++ toSql [ moduleName' t, tableName t ] ++
            " ADD CONSTRAINT " ++ name (columnName c) ++ 
            " UNIQUE (" ++ toSql (columnName c) ++ ")"
        sqlColumnUnique _ = SqlStmtEmpty
        
        -- tools

        name a = toSql (SqlName "TABLE_" // tableName t // SqlName "__" // a)

        moduleName' :: Table -> SqlName
        moduleName' t' = moduleName $ tableParentModule $ tableInternal t'
        
-- Function

getFunctionStatements :: OptCommon -> Setup -> Function -> [SqlStatement]
getFunctionStatements opts setup f =
    SqlStmtFunc sqlCreateFunction:
    sqlSetOwner (functionOwner f):
    SqlStmtComment ("COMMENT ON FUNCTION " ++ sqlFunctionIdentifier ++
      " IS " ++ toSqlString (functionDescription f)):
    map sqlStmtGrantExecute (maybeList $ functionPrivExecute f)

    where
        sqlStmtGrantExecute u = SqlStmtPriv $ sqlGrantExecute u
        sqlGrantExecute u = "GRANT EXECUTE ON FUNCTION \n" ++
            sqlFunctionIdentifier ++ "\nTO " ++ prefixedRole setup u

        sqlCreateFunction =
            "CREATE OR REPLACE FUNCTION " ++ sqlFunctionIdentifierDef ++
            "\n" ++
            "RETURNS " ++ toSql (functionReturns f) ++ sqlReturnsColumns (functionReturnsColumns f) ++
            "\nLANGUAGE " ++ sqlLanguage (functionLanguage f) ++
            "\nSECURITY " ++ sqlSecurity (functionSecurityDefiner f) ++
            "\nAS\n$BODY$\n" ++
                sqlBody ++
            "\n$BODY$\n"

        sqlSetOwner (Just o) = SqlStmtPriv $
            "ALTER FUNCTION " ++ sqlFunctionIdentifier ++
            "OWNER TO " ++ prefixedRole setup o
        sqlSetOwner Nothing = SqlStmtEmpty

        sqlFunctionIdentifierDef =
            toSql [moduleName $ functionParentModule $ functionInternal f ,
              functionName f]
                ++ "(\n" ++
                join ",\n" (maybeMap sqlParameterDef (functionParameters f)) ++
                "\n)"

        sqlFunctionIdentifier =
            toSql [moduleName $ functionParentModule $ functionInternal f ,
              functionName f]
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

getDomainStatements :: OptCommon -> Domain -> [SqlStatement]
getDomainStatements opt d =
 
    SqlStmtTypeDef
      ("CREATE DOMAIN " ++ toSql fullName ++ " AS " ++ toSql(domainType d))
    :
    sqlDefault (domainDefault d)
  :(maybeMap sqlCheck (domainChecks d))

  --stmtCommentOn "DOMAIN" fullName (domainDescription d)

    where
    fullName = [ moduleName $ domainParentModule $ domainInternal d , domainName d ]
    sqlCheck c = SqlStmtCreateCheckConstr (
      "ALTER DOMAIN " ++ toSql fullName ++ " ADD" ++
      " CONSTRAINT " ++ toSql (name (checkName c)) ++
      " CHECK (" ++ checkCheck c ++ ")")

    sqlDefault Nothing = SqlStmtEmpty
    sqlDefault (Just def) = SqlStmtAddDefault (
      "ALTER DOMAIN " ++ toSql fullName ++ " SET DEFAULT " ++ def)

    name a = SqlName "DOMAIN_" // domainName d // SqlName "__" // a

-- Types

getTypeStatements :: OptCommon -> Type -> [SqlStatement]
getTypeStatements opt t =
  SqlStmtTypeDef (
    "CREATE TYPE " ++ toSql fullName ++ " AS (" ++
    join ", " (map sqlElement (typeElements t)) ++ ")"
  ):
  stmtCommentOn "TYPE" fullName (typeDescription t)
  :[]

  where
    fullName = [ moduleName $ typeParentModule $ typeInternal t , typeName t ]
    sqlElement e = toSql (typeelementName e) ++ " " ++ toSql(typeelementType e)

-- Role

getRoleStatements :: OptCommon -> Setup -> Role -> [SqlStatement]
getRoleStatements opts setup r =
    SqlStmtRoleDef sqlCreateRole:
    (stmtCommentOn "ROLE" (setupRolePrefix' setup // roleName r) (roleDescription r)):
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

