-- This file is part of HamSql
--
-- Copyright 2014-2015 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module PostgresCon where

import Control.Exception
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal
import Network.URL
import qualified Data.ByteString.Char8 as B
import Data.String
import Data.List

import Option
import Parser
import Parser.Basic
import Sql
import Sql.Statement.Create
import Sql.Statement.Drop
import Utils

toQry :: String -> Query
toQry = fromString

getConUrl :: OptCommonDb -> URL
getConUrl xs = fromJustReason "Not a valid URL" (importURL $ optConnection xs)

pgsqlGetFullStatements :: OptCommon -> OptCommonDb -> Setup -> IO [SqlStatement]
pgsqlGetFullStatements opt optDb setup  = do
  role_delete_stmts <- pgsqlDeleteRoleStmt (getConUrl optDb) rolePrefix
  let main_stmts = getSetupStatements opt setup

  return $ main_stmts ++ role_delete_stmts

  where
    rolePrefix :: String
    rolePrefix = getPrefix (setupRolePrefix' setup)
    getPrefix :: SqlName -> String
    getPrefix (SqlName xs) = xs
 
pgsqlConnectUrl :: URL -> IO Connection
pgsqlConnectUrl url = do
  connResult <- try $ connectPostgreSQL $ (B.pack (exportURL url))

  return $ case connResult of
    Left e@(SqlError{}) -> err $ "sql connection failed: " ++ B.unpack (sqlErrorMsg e)
    Right conn -> conn


sqlManageSchemaJoin schemaid =
      " JOIN pg_namespace AS n " ++
      "  ON " ++ schemaid ++ " = n.oid AND " ++
      "  NOT n.nspname LIKE 'pg_%' AND " ++
      "  NOT n.nspname IN ('information_schema', 'public') "    
    
pgsqlDeleteRoleStmt :: URL -> String -> IO [SqlStatement]
pgsqlDeleteRoleStmt url prefix = do
  conn <- pgsqlConnectUrl url

  result :: [Only String] <- query_ conn $ toQry
    ("SELECT rolname FROM pg_roles WHERE rolname LIKE '" ++ prefix ++ "%'")
  let users = result
  
  return $ map statement users
  
  where
    -- statement :: (TextBl.ByteString) -> SqlStatement
    statement (Only user) = stmtDropRole user

pgsqlDeleteFunctionStmt :: Connection -> IO [SqlStatement]
pgsqlDeleteFunctionStmt conn =
  do
    result <- query_ conn $ toQry $
      "SELECT n.nspname, p.proname, oidvectortypes(p.proargtypes) " ++
      "FROM pg_proc AS p " ++
      (sqlManageSchemaJoin "p.pronamespace")
      
    return $ map f result
    
  where
    f :: (String, String, String) -> SqlStatement
    f (schema, function, args) = stmtDropFunction schema function args

      
pgsqlDeleteTableConstraintStmt :: Connection -> IO [SqlStatement]
pgsqlDeleteTableConstraintStmt conn =
  do
    result <- query_ conn $ toQry $
      "SELECT n.nspname, t.relname, c.conname " ++
      "FROM pg_constraint AS c " ++
      "JOIN pg_class AS t " ++
      "  ON c.conrelid = t.oid " ++
      (sqlManageSchemaJoin "c.connamespace")

    return $ map f result
    
  where
    f :: (String, String, String) -> SqlStatement
    f (schema, table, constraint) = stmtDropTableConstraint schema table constraint
      
pgsqlDeleteDomainConstraintStmt :: Connection -> IO [SqlStatement]
pgsqlDeleteDomainConstraintStmt conn =
  do
    result <- query_ conn $ toQry $
      "SELECT n.nspname, d.typname, c.conname " ++
      "FROM pg_constraint AS c " ++
      "JOIN pg_type AS d " ++
      "  ON c.contypid = d.oid " ++
      (sqlManageSchemaJoin "c.connamespace")

    return $ map f result
    
  where
    f :: (String, String, String) -> SqlStatement
    f (schema, domain, constraint) = stmtDropDomainConstraint schema domain constraint
      
pgsqlDeleteAllStmt :: Connection -> IO [SqlStatement]
pgsqlDeleteAllStmt conn = do
  function <- pgsqlDeleteFunctionStmt conn
  table_constraints <- pgsqlDeleteTableConstraintStmt conn
  domain_constraints <- pgsqlDeleteDomainConstraintStmt conn
  
  return $ function ++ table_constraints ++ domain_constraints

-- List existing objects

pgsqlListTables :: Connection -> IO [SqlName]
pgsqlListTables conn = do
  dat :: [(String,String)] <- query_ conn $ toQry $
    "SELECT table_schema, table_name" ++
    " FROM information_schema.tables" ++
    " WHERE table_type = 'BASE TABLE'" ++
    " AND table_schema NOT IN ('information_schema', 'pg_catalog')"
  return $ map toSqlName dat
  
  where
    toSqlName (s,t) = (SqlName s) <.> SqlName t


pgsqlListTableColumns :: Connection -> IO [(SqlName, SqlName)]
pgsqlListTableColumns conn = do
  dat :: [(String, String, String)] <- query_ conn $ toQry $
    "SELECT table_schema, table_name, column_name" ++
    " FROM information_schema.columns" ++
    --" WHERE table_type = 'BASE TABLE'" ++
    " WHERE table_schema NOT IN ('information_schema', 'pg_catalog')"
  return $ map toSqlName dat
  
  where
    toSqlName (s,t,u) = ((SqlName s) <.> SqlName t, SqlName u)

    
pgsqlListDomains :: Connection -> IO [SqlName]
pgsqlListDomains conn = do
  dat :: [(String,String)] <- query_ conn $ toQry $
    "SELECT domain_schema, domain_name" ++
    " FROM information_schema.domains" ++
    " WHERE domain_schema NOT IN ('information_schema', 'pg_catalog')"

  return $ map toSqlName dat
  
  where
    toSqlName (s,t) = (SqlName s) <.> SqlName t

pgsqlListTypes :: Connection -> IO [SqlName]
pgsqlListTypes conn = do
  dat :: [(String,String)] <- query_ conn $ toQry $
    "SELECT user_defined_type_schema, user_defined_type_name" ++
    " FROM information_schema.user_defined_types" ++
    " WHERE user_defined_type_schema NOT IN ('information_schema', 'pg_catalog')"
     
  return $ map toSqlName dat
  
  where
    toSqlName (s,t) = (SqlName s) <.> SqlName t
    
-- Fix missing or spare objects
  
pgsqlCorrectTables :: Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlCorrectTables conn stmtsInstall = do
  existingNames <- pgsqlListTables conn
  let expected = filter (SqlCreateTable `typeEq`) stmtsInstall
  let existing = map (SqlCreateTable `replacesTypeOf`) $ map stmtDropTable existingNames
 
  let stmtsCreate =  expected \\ existing
  let stmtsDrop = map (SqlDropTable `replacesTypeOf`) $ existing \\ expected
  
  return $ stmtsCreate ++ stmtsDrop

pgsqlCorrectTableColumns :: Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlCorrectTableColumns conn stmtsInstall = do
  existingNames <- pgsqlListTableColumns conn
  let expected = filter (SqlAddColumn `typeEq`) stmtsInstall
  let existing = map (SqlAddColumn `replacesTypeOf`) $ map stmtDropTableColumn existingNames
 
  let stmtsCreate =  expected \\ existing
  let stmtsDrop = map (SqlDropColumn `replacesTypeOf`) $ existing \\ expected
  
  return $ stmtsCreate ++ stmtsDrop
  
pgsqlCorrectDomains :: Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlCorrectDomains conn stmtsInstall = do
  existingNames <- pgsqlListDomains conn
  let expected = filter (SqlCreateDomain `typeEq`) stmtsInstall
  let existing = map (SqlCreateDomain `replacesTypeOf`) $ map stmtDropDomain existingNames
 
  let stmtsCreate =  expected \\ existing
  let stmtsDrop = map (SqlDropDomain `replacesTypeOf`) $ existing \\ expected
  
  return $ stmtsCreate ++ stmtsDrop

pgsqlCorrectTypes :: Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlCorrectTypes conn stmtsInstall = do
  existingNames <- pgsqlListTypes conn
  let expected = filter (SqlCreateType `typeEq`) stmtsInstall
  let existing = map (SqlCreateType `replacesTypeOf`) $ map stmtDropType existingNames
 
  let stmtsCreate =  expected \\ existing
  let stmtsDrop = map (SqlDropType `replacesTypeOf`) $ existing \\ expected
  
  return $ stmtsCreate ++ stmtsDrop
  
pgsqlUpdateFragile :: Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlUpdateFragile conn stmtsInstall = do
  tables <- pgsqlCorrectTables conn stmtsInstall
  columns <- pgsqlCorrectTableColumns conn stmtsInstall
  domains <- pgsqlCorrectDomains conn stmtsInstall
  types <- pgsqlCorrectTypes conn stmtsInstall
   
  return $ tables ++ columns ++ domains ++ types
  
-- DB Utils
  
pgsqlHandleErr code e@(SqlError{}) =
  err $
    "sql error in following statement:\n" ++
    code ++ "\n" ++
    "sql error: " ++ B.unpack (sqlErrorMsg e) ++ " (Error Code: " ++ B.unpack (sqlState e) ++ ")"
pgsqlHandleQry code e@(QueryError{}) = do
  putStrLn $
    "Information for SQL query:\n" ++
    code ++ "\n" ++
    "Message: '" ++ qeMessage e ++ "'"
  return 0

pgsqlExecWithoutTransact :: URL -> [SqlStatement] -> IO ()
pgsqlExecWithoutTransact connUrl xs = do
    conn <- connectPostgreSQL (B.pack (exportURL connUrl))
       
    _ <- sequence [ 
      do
        let code = (Parser.Basic.toSql stmt)
        execResult <- catch (catch (execute_ conn (toQry code)) (pgsqlHandleErr code)) (pgsqlHandleQry code)

        return ()
      | stmt <- stmtsFilterExecutable xs ]

    return ()

pgsqlExec :: URL -> [SqlStatement] -> IO ()
pgsqlExec = pgsqlExecIntern True

pgsqlExecAndRollback :: URL -> [SqlStatement] -> IO ()
pgsqlExecAndRollback= pgsqlExecIntern False

pgsqlExecIntern :: Bool -> URL -> [SqlStatement] -> IO ()
pgsqlExecIntern doCommit connUrl xs = do
    conn <- connectPostgreSQL (B.pack (exportURL connUrl))
    begin conn
       
    _ <- sequence [ 
      do
        let code = (Parser.Basic.toSql stmt)
        execResult <- catch (catch (execute_ conn (toQry code)) (pgsqlHandleErr code)) (pgsqlHandleQry code)

        return ()
      | stmt <- stmtsFilterExecutable xs ]
    
    if doCommit then
      commit conn
    else
      rollback conn
