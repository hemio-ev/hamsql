-- This file is part of HamSql
--
-- Copyright 2014-2015 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module PostgresCon where

import Control.Exception
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (PGArray, fromPGArray)
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

import Data.Vector (fromList)

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
  connResult <- try $ connectPostgreSQL (B.pack (exportURL url))

  return $ case connResult of
    Left e@SqlError{} -> err $ "sql connection failed: " ++ B.unpack (sqlErrorMsg e)
    Right conn -> conn


sqlManageSchemaJoin schemaid =
      " JOIN pg_namespace AS n " ++
      "  ON " ++ schemaid ++ " = n.oid AND " ++
      "  NOT n.nspname LIKE 'pg_%' AND " ++
      "  NOT n.nspname IN ('information_schema', 'public') "

-- DROP ROLE statements for all roles on the server prefixed with `prefix`
pgsqlDeleteRoleStmt :: URL -> String -> IO [SqlStatement]
pgsqlDeleteRoleStmt url prefix = do
  conn <- pgsqlConnectUrl url

  result <- query conn "SELECT rolname FROM pg_roles WHERE rolname LIKE ?" $
    Only $ prefix ++ "%"
  let users = result

  return $ map toStmt users

  where
    toStmt :: Only String -> SqlStatement
    toStmt (Only user) = stmtDropRole $ SqlName user

-- DROP FUNCTION
pgsqlDeleteFunctionStmt :: Connection -> IO [SqlStatement]
pgsqlDeleteFunctionStmt conn =
  do
    result <- query_ conn $ toQry $
      "SELECT n.nspname, p.proname, " ++
      -- This part of the query includes a workaround for
      -- <https://github.com/lpsmith/postgresql-simple/issues/166>
      "ARRAY(SELECT UNNEST(p.proargtypes::regtype[]::varchar[])) " ++
      "FROM pg_proc AS p " ++
      sqlManageSchemaJoin "p.pronamespace"

    return $ map toStmt result

  where
    toStmt :: (SqlName, SqlName, PGArray SqlType) -> SqlStatement
    toStmt (schema, function, args) = stmtDropFunction (schema, function, fromPGArray args)

-- DROP TABLE CONSTRAINT
pgsqlDeleteTableConstraintStmt :: Connection -> IO [SqlStatement]
pgsqlDeleteTableConstraintStmt conn =
  do
    result <- query_ conn $ toQry $
      "SELECT n.nspname, t.relname, c.conname " ++
      "FROM pg_constraint AS c " ++
      "JOIN pg_class AS t " ++
      "  ON c.conrelid = t.oid " ++
      sqlManageSchemaJoin "c.connamespace"

    return $ map f result

  where
    f :: (SqlName, SqlName, SqlName) -> SqlStatement
    f = stmtDropTableConstraint

-- DROP DOMAIN CONSTRAINT
pgsqlDeleteDomainConstraintStmt :: Connection -> IO [SqlStatement]
pgsqlDeleteDomainConstraintStmt conn =
  do
    result <- query_ conn $ toQry $
      "SELECT n.nspname, d.typname, c.conname " ++
      "FROM pg_constraint AS c " ++
      "JOIN pg_type AS d " ++
      "  ON c.contypid = d.oid " ++
      sqlManageSchemaJoin "c.connamespace"

    return $ map f result

  where
    f :: (SqlName, SqlName, SqlName) -> SqlStatement
    f = stmtDropDomainConstraint

-- All DROP statements
pgsqlDeleteAllStmt :: Connection -> IO [SqlStatement]
pgsqlDeleteAllStmt conn = do
  table_constraints <- pgsqlDeleteTableConstraintStmt conn
  domain_constraints <- pgsqlDeleteDomainConstraintStmt conn

  return $ table_constraints ++ domain_constraints

-- List existing objects

-- List TABLE
pgsqlListTables :: Connection -> IO [SqlName]
pgsqlListTables conn = do
  dat :: [(String,String)] <- query_ conn $ toQry $
    "SELECT table_schema, table_name" ++
    " FROM information_schema.tables" ++
    " WHERE table_type = 'BASE TABLE'" ++
    " AND table_schema NOT IN ('information_schema', 'pg_catalog')"
  return $ map toSqlName dat

  where
    toSqlName (s,t) = SqlName s <.> SqlName t

-- List TABLE COLUMN
pgsqlListTableColumns :: Connection -> IO [(SqlName, SqlName)]
pgsqlListTableColumns conn = do
  dat :: [(String, String, String)] <- query_ conn $ toQry $
    "SELECT table_schema, table_name, column_name" ++
    " FROM information_schema.columns" ++
    --" WHERE table_type = 'BASE TABLE'" ++
    " WHERE table_schema NOT IN ('information_schema', 'pg_catalog')"
  return $ map toSqlName dat

  where
    toSqlName (s,t,u) = (SqlName s <.> SqlName t, SqlName u)

-- List DOMAIN
pgsqlListDomains :: Connection -> IO [SqlName]
pgsqlListDomains conn = do
  dat :: [(String,String)] <- query_ conn $ toQry $
    "SELECT domain_schema, domain_name" ++
    " FROM information_schema.domains" ++
    " WHERE domain_schema NOT IN ('information_schema', 'pg_catalog')"

  return $ map toSqlName dat

  where
    toSqlName (s,t) = SqlName s <.> SqlName t

pgsqlListTypes :: Connection -> IO [SqlName]
pgsqlListTypes conn = do
  dat :: [(String,String)] <- query_ conn $ toQry $
    "SELECT user_defined_type_schema, user_defined_type_name" ++
    " FROM information_schema.user_defined_types" ++
    " WHERE user_defined_type_schema NOT IN ('information_schema', 'pg_catalog')"

  return $ map toSqlName dat

  where
    toSqlName (s,t) = SqlName s <.> SqlName t

-- Fix missing or spare objects

pgsqlCorrectTables :: Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlCorrectTables conn stmtsInstall = do
  existingNames <- pgsqlListTables conn
  let expected = filter (typeEq SqlCreateTable) stmtsInstall
  let existing = map ((SqlCreateTable `replacesTypeOf`) . stmtDropTable) existingNames

  let stmtsCreate =  expected \\ existing
  let stmtsDrop = map (SqlDropTable `replacesTypeOf`) $ existing \\ expected

  return $ stmtsCreate ++ stmtsDrop

normalizedFuncStmt :: Connection -> SqlStatement -> IO SqlStatement
normalizedFuncStmt conn (SqlStmtFunction t n p c) = do
  x :: [(Int,Int)] <- query conn "SELECT ?, ?" (1::Int, 2::Int)
  p' :: [Only (PGArray SqlType)] <- query conn "SELECT ?::regtype[]::varchar[]" (Only $ fromList (map toSql p))

  return $ SqlStmtFunction t n (fromPGArray $ head (map fromOnly p')) c


pgsqlCorrectFunctions :: Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlCorrectFunctions conn xs = do
  -- pgsqlDeleteFunctionStmt conn
  drop <- pgsqlDeleteFunctionStmt conn
  let create = filter (typeEq SqlCreateFunction) xs

  createNormalized <- mapM (normalizedFuncStmt conn) create
  let dropFiltered = drop \\ map (SqlDropFunction `replacesTypeOf`) createNormalized
  -- let d = map (normalizedFuncStmt conn) expected

  return $ create ++ dropFiltered

pgsqlCorrectTableColumns :: Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlCorrectTableColumns conn stmtsInstall = do
  existingNames <- pgsqlListTableColumns conn
  let expected = filter (SqlAddColumn `typeEq`) stmtsInstall
  let existing = map ((SqlAddColumn `replacesTypeOf`) . stmtDropTableColumn) existingNames

  let stmtsCreate =  expected \\ existing
  let stmtsDrop = map (SqlDropColumn `replacesTypeOf`) $ existing \\ expected

  return $ stmtsCreate ++ stmtsDrop

pgsqlCorrectDomains :: Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlCorrectDomains conn stmtsInstall = do
  existingNames <- pgsqlListDomains conn
  let expected = filter (SqlCreateDomain `typeEq`) stmtsInstall
  let existing = map ((SqlCreateDomain `replacesTypeOf`) . stmtDropDomain) existingNames

  let stmtsCreate =  expected \\ existing
  let stmtsDrop = map (SqlDropDomain `replacesTypeOf`) $ existing \\ expected

  return $ stmtsCreate ++ stmtsDrop

pgsqlCorrectTypes :: Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlCorrectTypes conn stmtsInstall = do
  existingNames <- pgsqlListTypes conn
  let expected = filter (SqlCreateType `typeEq`) stmtsInstall
  let existing = map ((SqlCreateType `replacesTypeOf`) . stmtDropType) existingNames

  let stmtsCreate =  expected \\ existing
  let stmtsDrop = map (SqlDropType `replacesTypeOf`) $ existing \\ expected

  return $ stmtsCreate ++ stmtsDrop

pgsqlUpdateFragile :: OptUpgrade -> Connection -> [SqlStatement] -> IO [SqlStatement]
pgsqlUpdateFragile optUpgrade conn stmtsInstall = do
  tables <- pgsqlCorrectTables conn stmtsInstall
  columns <- pgsqlCorrectTableColumns conn stmtsInstall
  domains <- pgsqlCorrectDomains conn stmtsInstall
  types <- pgsqlCorrectTypes conn stmtsInstall
  functions <- pgsqlCorrectFunctions conn stmtsInstall

  return $ if optPermitDataDeletion optUpgrade
    then
      tables ++ columns ++ domains ++ types ++ functions
    else filter (\t -> typeEq SqlDropTable t || typeEq SqlDropColumn t)
      tables ++ columns ++ domains ++ types ++ functions

-- DB Utils

pgsqlHandleErr code e@SqlError{} =
  err $
    "sql error in following statement:\n" ++
    code ++ "\n" ++
    "sql error: " ++ B.unpack (sqlErrorMsg e) ++ " (Error Code: " ++ B.unpack (sqlState e) ++ ")"
pgsqlHandleQry code e@QueryError{} = do
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
        let code = Parser.Basic.toSql stmt
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
        let code = Parser.Basic.toSql stmt
        execResult <- catch (catch (execute_ conn (toQry code)) (pgsqlHandleErr code)) (pgsqlHandleQry code)

        return ()
      | stmt <- stmtsFilterExecutable xs ]

    if doCommit then
      commit conn
    else
      rollback conn

