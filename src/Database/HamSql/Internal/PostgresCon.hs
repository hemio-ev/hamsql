-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
= Implementation of Upgrades

Upgrades are tricky.

* DELETE statements must come first

== Tables

__TODO__: Delete tables and columns if the user excplicitly agrees to data loss.

CREATE TABLE IF NOT EXISTS without any columns.

CREATE columns with their type on demand.

Always apply all ALTER COLUMN statments

* SET DATA TYPE
* DROP DEFAULT
* SET DEFAULT
* SET/DROP NOT NULL

__TODO__: Maybe bundle DROP/SET DEFAULT, such that they can't be reordered.

Constraints are completly dropped an recreated.

== Functions

Many things can depend on functions (column defaults, table and domain
constraints). Therefore, they should all be deleted before deleting a function.

Usually @CREATE OR REPLACE FUNCTION@ is used, such that no deletions occure.

However, some function properties cannot be changed via CREATE OR REPLACE. A
known example this the change of the return type. In this case the
'sqlErrInvalidFunctionDefinition' will be catched and the function is deleted
and created again.

== Domains

Constraints are always deleted an recreated.

Defaults are always set explicitly.

Domains are deleted on demand.

/The data type of a domain cannot be changed (at least in PostgreSQL)./

== Roles

Difficult since not schema qualified, but be have prefixes.

__TODO__: LOGIN and PASSWORD are not implemented via ALTER ROLE

__TODO__: Are role memberships revoked?

== Sequences

CREATE only defines name on demand.

Properties all via ALTER SEQUENCE.

-}
module Database.HamSql.Internal.PostgresCon where

import Control.Exception
import Control.Monad (void, when)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Transaction
import Database.PostgreSQL.Simple.Types (PGArray(..), fromPGArray)

import Network.URI (URI, parseAbsoluteURI, uriToString)

import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.InquireDeployed
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Sql
import Database.HamSql.Internal.Stmt.Domain
import Database.HamSql.Internal.Stmt.Function
import Database.HamSql.Internal.Stmt.Table
import Database.HamSql.Setup
import Database.HamSql.SqlStmt
import Database.YamSql

sqlErrInvalidFunctionDefinition :: B.ByteString
sqlErrInvalidFunctionDefinition = "42P13"

pgsqlGetFullStatements :: OptCommon -> OptCommonDb -> Setup -> IO [SqlStmt]
pgsqlGetFullStatements optCom _ setup = return $ getSetupStatements optCom setup

pgsqlDeleteAllStmt :: Connection -> IO [SqlStmt]
pgsqlDeleteAllStmt conn = do
  domainConstraints <- deployedDomainConstraintIds conn
  tableConstraints <- deployedTableConstraintIds conn
  return
    (concatMap stmtsDropDomainConstraint domainConstraints ++
     concatMap stmtsDropTableConstraint tableConstraints)

pgsqlUpdateFragile :: t -> Connection -> [SqlStmt] -> IO [SqlStmt]
pgsqlUpdateFragile _ conn createStmts = correctDomains conn createStmts

correctDomains :: Connection -> [SqlStmt] -> IO [SqlStmt]
correctDomains conn xs =
  correctStatements
    (SqlCreateDomain, xs)
    (stmtsDropDomain, deployedDomainIds conn)

-- DB Utils
pgsqlExecWithoutTransact :: URI -> [SqlStmt] -> IO Connection
pgsqlExecWithoutTransact = pgsqlExecIntern PgSqlWithoutTransaction

pgsqlExec :: URI -> [SqlStmt] -> IO Connection
pgsqlExec = pgsqlExecIntern PgSqlWithTransaction

pgsqlExecAndRollback :: URI -> [SqlStmt] -> IO ()
pgsqlExecAndRollback url stmts = do
  conn <- pgsqlExecIntern PgSqlWithTransaction url stmts
  rollback conn

pgsqlExecStmtList
  :: Status
  -> [SqlStmt] -- ^ Statements that still need to be executed
  -> [SqlStmt] -- ^ Statements that have failed during execution
  -> Connection
  -> IO ()
pgsqlExecStmtList Init _ (_:_) _ =
  err "supplied failed statements to (pgsqlExecStmtList Init)"
-- No remaining statements to execute
pgsqlExecStmtList _ [] [] conn = commit conn
pgsqlExecStmtList Unchanged [] failed conn =
  pgsqlExecStmtHandled conn (head failed)
pgsqlExecStmtList Changed [] failed conn =
  void $ pgsqlExecStmtList Unchanged failed [] conn
pgsqlExecStmtList status (x:xs) failed conn = do
  savepoint <- newSavepoint conn
  tryExec savepoint `catch` handleSqlError savepoint `catch` handleQueryError savepoint
  where
    tryExec savepoint = do
      pgsqlExecStmt conn x
      proceed savepoint
    -- action after execution has not failed
    proceed savepoint = do
      releaseSavepoint conn savepoint
      pgsqlExecStmtList Changed xs failed conn
    handleSqlError savepoint SqlError {sqlState = errCode}
      | errCode == sqlErrInvalidFunctionDefinition =
        skipQuery savepoint (stmtsDropFunction (sqlId x) ++ [x])
      | otherwise = skipQuery savepoint [x]
    handleQueryError savepoint QueryError {} = proceed savepoint
    -- action after execution has failed
    skipQuery savepoint stmts = do
      rollbackToSavepoint conn savepoint
      releaseSavepoint conn savepoint
      pgsqlExecStmtList status xs (failed ++ stmts) conn

pgsqlExecIntern :: PgSqlMode -> URI -> [SqlStmt] -> IO Connection
pgsqlExecIntern mode connUrl xs = do
  conn <- pgsqlConnectUrl connUrl
  when (mode == PgSqlWithTransaction) $
    do begin conn
       pgsqlExecStmtList Init xs [] conn
  when (mode == PgSqlWithoutTransaction) $ mapM_ (pgsqlExecStmtHandled conn) xs
  return conn

addSqlStmtType
  :: ToSqlId a
  => [a] -> SqlStmtType -> [Maybe SqlStmtId]
addSqlStmtType xs y = map (Just . SqlStmtId y . sqlId) xs

filterSqlStmtType :: SqlStmtType -> [SqlStmt] -> [SqlStmt]
filterSqlStmtType t xs = filter (\x -> Just t == stmtIdType x) xs

removeStmtsByTypeAndIds
  :: ToSqlId a
  => SqlStmtType -> [a] -> [SqlStmt] -> [SqlStmt]
removeStmtsByTypeAndIds t is xs =
  filter (\x -> stmtId x `elem` addSqlStmtType is t) xs

removeSqlIdBySqlStmts
  :: ToSqlId a
  => SqlStmtType -> [SqlStmt] -> [a] -> [a]
removeSqlIdBySqlStmts t xs is = filter (\x -> sqlId x `notElem` ids) is
  where
    ids = map sqlId $ filterSqlStmtType t xs

correctStatements
  :: ToSqlId a
  => (SqlStmtType, [SqlStmt]) -> (a -> [SqlStmt], IO [a]) -> IO [SqlStmt]
correctStatements (t, xs) (f, iois) = do
  is <- iois
  return $
    removeStmtsByTypeAndIds t is xs ++
    concatMap f (removeSqlIdBySqlStmts t xs is)
--    if optPermitDataDeletion optUpgrade
--      then stmts
--     else filter
--             (\t -> not (typeEq SqlDropTable t) && not (typeEq SqlDropColumn t))
--             stmts
