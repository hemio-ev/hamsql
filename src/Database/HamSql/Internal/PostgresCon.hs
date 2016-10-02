-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleContexts    #-}
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

Constrs are completly dropped an recreated. Would be cool to create PKs
first, to fix FK requirements right away.

== Functions

Many things can depend on functions (column defaults, table and domain
constraints). Therefore, they should all be deleted before deleting a function.

Usually @CREATE OR REPLACE FUNCTION@ is used, such that no deletions occure.

However, some function properties cannot be changed via CREATE OR REPLACE. A
known example this the change of the return type. In this case the
'sqlErrInvalidFunctionDefinition' will be catched and the function is deleted
and created again.

== Domains

Constrs are always deleted an recreated.

Defaults are always set explicitly.

Domains are deleted on demand.

/The data type of a domain cannot be changed (at least in PostgreSQL)./

__TODO__: Issue error if domain type differs.

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
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Transaction

import Network.URI (URI)

import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.InquireDeployed
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Stmt.Create
import Database.HamSql.Internal.Stmt.Domain
import Database.HamSql.Internal.Stmt.Drop
import Database.HamSql.Internal.Stmt.Function
import Database.HamSql.Internal.Stmt.Role
import Database.HamSql.Internal.Stmt.Sequence
import Database.HamSql.Internal.Stmt.Table
import Database.HamSql.Internal.Utils
import Database.HamSql.Setup
import Database.YamSql

sqlErrInvalidFunctionDefinition :: B.ByteString
sqlErrInvalidFunctionDefinition = "42P13"

pgsqlGetFullStatements :: OptCommon -> OptCommonDb -> Setup -> IO [SqlStmt]
pgsqlGetFullStatements optCom _ setup =
  return $ catMaybes $ getSetupStatements optCom setup

pgsqlDeleteAllStmt :: Connection -> IO [SqlStmt]
pgsqlDeleteAllStmt conn = do
  domainConstrs <- deployedDomainConstrIds conn
  tableConstrs <- deployedTableConstrIds conn
  return $
    catMaybes $
    concatMap stmtsDropDomainConstr domainConstrs ++
    concatMap stmtsDropTableConstr tableConstrs

pgsqlUpdateFragile :: Setup -> Connection -> [SqlStmt] -> IO [SqlStmt]
pgsqlUpdateFragile setup conn stmts =
  correctStmts SqlCreateDomain deployedDomainIds stmtsDropDomain stmts >>=
  correctStmts SqlCreateTable deployedTableIds stmtsDropTable >>=
  correctStmts SqlAddColumn deployedTableColumnIds stmtsDropTableColumn >>=
  correctStmts SqlCreateSequence deployedSequenceIds stmtsDropSequence >>=
  correctStmts SqlCreateRole (deployedRoleIds setup) (stmtsDropRole setup) >>=
  dropResidual SqlCreateFunction deployedFunctionIds stmtsDropFunction >>=
  revokeAllPrivileges setup (deployedRoleIds setup conn)
  where
    correctStmts
      :: ToSqlId a
      => SqlStmtType
      -> (Connection -> IO [a])
      -> (a -> [Maybe SqlStmt])
      -> [SqlStmt]
      -> IO [SqlStmt]
    correctStmts createType existingInquire dropStmtGenerator =
      correctStatements createType (existingInquire conn) dropStmtGenerator
    dropResidual
      :: ToSqlId a
      => SqlStmtType
      -> (Connection -> IO [a])
      -> (a -> [Maybe SqlStmt])
      -> [SqlStmt]
      -> IO [SqlStmt]
    dropResidual t isf f xs = addDropResidual t (isf conn) f xs

revokeAllPrivileges :: Setup
                    -> IO [SqlObj SQL_ROLE SqlName]
                    -> [SqlStmt]
                    -> IO [SqlStmt]
revokeAllPrivileges setup roles stmts =
  (++ stmts) <$> catMaybes <$> concatMap (stmtsDropAllPrivileges setup) <$> roles

pgsqlDropAllRoleStmts :: OptCommonDb -> Setup -> IO [SqlStmt]
pgsqlDropAllRoleStmts optDb setup = do
  conn <- pgsqlConnectUrl $ getConUrl optDb
  addDropResidual
    SqlCreateRole
    (deployedRoleIds setup conn)
    (stmtsDropRole setup)
    []

-- DB Utils
pgsqlExecWithoutTransact :: OptCommonDb -> URI -> [SqlStmt] -> IO Connection
pgsqlExecWithoutTransact opt = pgsqlExecIntern opt PgSqlWithoutTransaction

pgsqlExec :: OptCommonDb -> URI -> [SqlStmt] -> IO Connection
pgsqlExec opt = pgsqlExecIntern opt PgSqlWithTransaction

pgsqlExecAndRollback :: OptCommonDb -> URI -> [SqlStmt] -> IO ()
pgsqlExecAndRollback opt url stmts = do
  conn <- pgsqlExecIntern opt PgSqlWithTransaction url stmts
  rollback conn

pgsqlExecStmtList
  :: OptCommonDb
  -> Status
  -> [SqlStmt] -- ^ Statements that still need to be executed
  -> [SqlStmt] -- ^ Statements that have failed during execution
  -> Connection
  -> IO ()
pgsqlExecStmtList _ Init _ (x:_) _ =
  err $ "supplied failed statements to (pgsqlExecStmtList _ Init): " <> tshow x
-- No remaining statements to execute
pgsqlExecStmtList _ _ [] [] conn = commit conn
pgsqlExecStmtList _ Unchanged [] failed conn =
  pgsqlExecStmtHandled conn (head failed)
pgsqlExecStmtList opt Changed [] failed conn =
  void $ pgsqlExecStmtList opt Unchanged failed [] conn
pgsqlExecStmtList opt status (x:xs) failed conn = do
  savepoint <- newSavepoint conn
  tryExec savepoint `catch` handleSqlError savepoint `catch` handleQueryError savepoint
  where
    tryExec savepoint = do
      logStmt opt $
        "-- Executing " <> tshow (stmtIdType x) <> " for " <> stmtDesc x
      pgsqlExecStmt conn x
      logStmt opt $ toSqlCode x
      proceed savepoint
    -- action after execution has not failed
    proceed savepoint = do
      releaseSavepoint conn savepoint
      pgsqlExecStmtList opt Changed xs failed conn
    handleSqlError savepoint SqlError {sqlState = errCode}
      | errCode == sqlErrInvalidFunctionDefinition =
        skipQuery savepoint (stmtsDropFunction' (sqlId x) ++ [x])
      | otherwise = skipQuery savepoint [x]
    handleQueryError savepoint QueryError {} = proceed savepoint
    -- action after execution has failed
    skipQuery savepoint stmts = do
      unless (optSqlLogHideRollbacks opt) $
        do logStmt opt "SAVEPOINT retry;"
           logStmt opt $ toSqlCode x
           logStmt opt "ROLLBACK TO SAVEPOINT retry;"
      rollbackToSavepoint conn savepoint
      releaseSavepoint conn savepoint
      pgsqlExecStmtList opt forwardStatus xs (failed ++ stmts) conn
    -- Init may not be forwarded to next iteration
    forwardStatus =
      case status of
        Init -> Unchanged
        s -> s

pgsqlExecIntern :: OptCommonDb -> PgSqlMode -> URI -> [SqlStmt] -> IO Connection
pgsqlExecIntern opt mode connUrl xs = do
  conn <- pgsqlConnectUrl connUrl
  when (mode == PgSqlWithTransaction) $
    do begin conn
       pgsqlExecStmtList opt Init xs [] conn
  when (mode == PgSqlWithoutTransaction) $ mapM_ (pgsqlExecStmtHandled conn) xs
  return conn

addSqlStmtType
  :: ToSqlId a
  => SqlStmtType -- ^ statment
  -> [a] -- ^ SQL ids that should become a "SqlStmtId" type to use
  -> [SqlStmtId]
addSqlStmtType t = map (SqlStmtId t . sqlId)

filterSqlStmtType :: SqlStmtType -> [SqlStmt] -> [SqlStmt]
filterSqlStmtType t xs =
  [ x
  | x <- xs
  , stmtIdType x == t ]

removeStmtsMatchingIds
  :: [SqlStmtId] -- ^ Statement ids to remove
  -> [SqlStmt]
  -> [SqlStmt]
removeStmtsMatchingIds ids stmts =
  [ stmt
  | stmt <- stmts
  , stmtId stmt `notElem` ids ]

removeSqlIdBySqlStmts
  :: ToSqlId a
  => SqlStmtType -> [SqlStmt] -> [a] -> [a]
removeSqlIdBySqlStmts t xs is =
  [ x
  | x <- is
  , sqlId x `notElem` ids ]
  where
    ids = map sqlId $ filterSqlStmtType t xs

-- target set of sql ids
correctStatements
  :: ToSqlId a
  => SqlStmtType -- ^ install statements and the stmt type of interest
  -> IO [a] -- ^ deployed (existing) elements
  -> (a -> [Maybe SqlStmt]) -- ^ drop statment generator
  -> [SqlStmt] -- ^ install statements, representing desired state
  -> IO [SqlStmt]
correctStatements t iois f xs = do
  is <- iois
  xs' <- addDropResidual t iois f xs
  return $ removeStmtsMatchingIds (addSqlStmtType t is) xs'

addDropResidual
  :: ToSqlId a
  => SqlStmtType
  -> IO [a]
  -> (a -> [Maybe SqlStmt])
  -> [SqlStmt]
  -> IO [SqlStmt]
addDropResidual t iois f xs = do
  is <- iois
  return $ xs ++ catMaybes (concatMap f (removeSqlIdBySqlStmts t xs is))
