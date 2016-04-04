-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgresCon where

import           Control.Exception
import           Control.Monad                          (void, when)
import qualified Data.ByteString.Char8                  as B
import           Data.List
import           Data.String
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Transaction
import           Database.PostgreSQL.Simple.Types       (PGArray, fromPGArray)
import           Network.URL

import Option
import Parser
import Parser.Basic
import Sql
import Sql.Statement.Create
import Sql.Statement.Drop
import Utils

import Data.Vector (fromList)

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

sqlManageSchemaJoin schemaid =
      " JOIN pg_namespace AS n " ++
      "  ON " ++ schemaid ++ " = n.oid AND " ++
      "  NOT n.nspname LIKE 'pg_%' AND " ++
      "  n.nspname NOT IN ('information_schema') "

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

  let stmts = tables ++ columns ++ domains ++ types ++ functions

  return $
    if optPermitDataDeletion optUpgrade
      then
        stmts
      else
        filter (\t ->
          not (typeEq SqlDropTable t)
          && not (typeEq SqlDropColumn t))
        stmts

-- DB Utils

toQry :: String -> Query
toQry = fromString

getConUrl :: OptCommonDb -> URL
getConUrl xs = fromJustReason "Not a valid URL" (importURL $ optConnection xs)

pgsqlConnectUrl :: URL -> IO Connection
pgsqlConnectUrl url = do
  connResult <- try $ connectPostgreSQL (B.pack (exportURL url))
  let conn = getConn connResult
  _ <- execute_ conn "SET client_min_messages TO WARNING"

  return conn

 where
  getConn res = case res of
     Left e@SqlError{} -> err $ "sql connection failed: " ++ B.unpack (sqlErrorMsg e)
     Right conn -> conn

pgsqlHandleErr :: SqlStatement -> SqlError -> IO ()
pgsqlHandleErr code e = do
    err $
        "sql error in following statement:\n" ++
        toSql code ++ "\n" ++
        "sql error: " ++ B.unpack (sqlErrorMsg e) ++ " (Error Code: " ++ B.unpack (sqlState e) ++ ")"
    return ()

pgsqlExecWithoutTransact :: URL -> [SqlStatement] -> IO Connection
pgsqlExecWithoutTransact = pgsqlExecIntern PgSqlWithoutTransaction

pgsqlExec :: URL -> [SqlStatement] -> IO Connection
pgsqlExec = pgsqlExecIntern PgSqlWithTransaction

pgsqlExecAndRollback :: URL -> [SqlStatement] -> IO ()
pgsqlExecAndRollback url stmts = do
    conn <- pgsqlExecIntern PgSqlWithTransaction url stmts
    rollback conn

data PgSqlMode = PgSqlWithoutTransaction | PgSqlWithTransaction
 deriving Eq

data Status = Init | Changed | Unchanged

pgsqlExecStmtList :: Status -> [SqlStatement] -> [SqlStatement] -> Connection -> IO ()
pgsqlExecStmtList _         [] []     conn = commit conn
pgsqlExecStmtList Unchanged [] failed conn = pgsqlExecStmtHandled conn (head failed)
pgsqlExecStmtList Changed   [] failed conn = void $ pgsqlExecStmtList Unchanged failed [] conn
pgsqlExecStmtList status (x:xs) failed conn = do
    savepoint <- newSavepoint conn

    do
        pgsqlExecStmt conn x
        proceed savepoint

     `catch` handleSqlError savepoint
     `catch` handleQueryError savepoint

    where
        handleSqlError savepoint SqlError{sqlState=err}
         | err == "42P13" = skipQuery savepoint [stmtDropFunction' x, x]
         | otherwise      = skipQuery savepoint [x]

        handleQueryError savepoint QueryError{} = proceed savepoint

        proceed savepoint = do
            releaseSavepoint conn savepoint
            pgsqlExecStmtList Changed xs failed conn

        skipQuery savepoint stmts = do
            rollbackToSavepoint conn savepoint
            releaseSavepoint conn savepoint
            pgsqlExecStmtList status xs (failed ++ stmts) conn


pgsqlExecStmt :: Connection -> SqlStatement -> IO ()
pgsqlExecStmt conn stmt = do
    let code = Parser.Basic.toSql stmt
    execute_ conn (toQry code)
    return ()

pgsqlExecStmtHandled :: Connection -> SqlStatement -> IO ()
pgsqlExecStmtHandled conn stmt = pgsqlExecStmt conn stmt
    `catch` pgsqlHandleErr stmt

pgsqlExecIntern :: PgSqlMode -> URL -> [SqlStatement] -> IO Connection
pgsqlExecIntern mode connUrl xs = do
    conn <- pgsqlConnectUrl connUrl

    when (mode == PgSqlWithTransaction) $ do
        begin conn
        pgsqlExecStmtList Init (stmtsFilterExecutable xs) [] conn

    when (mode == PgSqlWithoutTransaction) $ void $
        mapM (pgsqlExecStmtHandled conn) (stmtsFilterExecutable xs)

    return conn

