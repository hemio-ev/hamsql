-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad (void, when)
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Transaction

import Network.URI (URI)

import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.InquireDeployed
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Stmt.Domain
import Database.HamSql.Internal.Stmt.Function
import Database.HamSql.Internal.Stmt.Sequence
import Database.HamSql.Internal.Stmt.Table
import Database.HamSql.Internal.Utils
import Database.HamSql.Setup
import Database.HamSql.SqlStmt
import Database.YamSql

sqlErrInvalidFunctionDefinition :: B.ByteString
sqlErrInvalidFunctionDefinition = "42P13"

pgsqlGetFullStatements :: OptCommon -> OptCommonDb -> Setup -> IO [SqlStmt]
pgsqlGetFullStatements optCom _ setup = return $ getSetupStatements optCom setup

pgsqlDeleteAllStmt :: Connection -> IO [SqlStmt]
pgsqlDeleteAllStmt conn = do
  domainConstrs <- deployedDomainConstrIds conn
  tableConstrs <- deployedTableConstrIds conn
  return
    (concatMap stmtsDropDomainConstr domainConstrs ++
     concatMap stmtsDropTableConstr tableConstrs)

pgsqlUpdateFragile :: t -> Connection -> [SqlStmt] -> IO [SqlStmt]
pgsqlUpdateFragile _ conn stmts =
  correctStmts SqlCreateDomain deployedDomainIds stmtsDropDomain stmts >>=
  correctStmts SqlCreateTable deployedTableIds stmtsDropTable >>=
  correctStmts SqlAddColumn deployedTableColumnIds stmtsDropTableColumn >>=
  correctStmts SqlCreateSequence deployedSequenceIds stmtsDropSequence >>=
  dropResidual SqlCreateFunction deployedFunctionIds stmtsDropFunction
  where
    correctStmts
      :: ToSqlId a
      => SqlStmtType
      -> (Connection -> IO [a])
      -> (a -> [SqlStmt])
      -> [SqlStmt]
      -> IO [SqlStmt]
    correctStmts createType existingInquire dropStmtGenerator =
      correctStatements createType (existingInquire conn) dropStmtGenerator
    dropResidual
      :: ToSqlId a
      => SqlStmtType
      -> (Connection -> IO [a])
      -> (a -> [SqlStmt])
      -> [SqlStmt]
      -> IO [SqlStmt]
    dropResidual t isf f xs = do
      is <- isf conn
      return (xs ++ concatMap f (removeSqlIdBySqlStmts t xs is))

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
      logStmt $
        "-- Executing " <> tshow (fromMaybe SqlUnclassified (stmtIdType x)) <>
        " for " <>
        stmtDesc x
      pgsqlExecStmt conn x
      logStmt $ toSqlCode x
      proceed savepoint
    -- action after execution has not failed
    proceed savepoint = do
      releaseSavepoint conn savepoint
      pgsqlExecStmtList Changed xs failed conn
    handleSqlError savepoint SqlError {sqlState = errCode}
      | errCode == sqlErrInvalidFunctionDefinition =
        skipQuery savepoint (stmtsDropFunction' (sqlId x) ++ [x])
      | otherwise = skipQuery savepoint [x]
    handleQueryError savepoint QueryError {} = proceed savepoint
    -- action after execution has failed
    skipQuery savepoint stmts = do
      logStmt "SAVEPOINT retry;"
      logStmt $ toSqlCode x
      logStmt "ROLLBACK TO SAVEPOINT retry;"
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
  => SqlStmtType -- ^ statment
  -> [a] -- ^ SQL ids that should become a "SqlStmtId" type to use
  -> [Maybe SqlStmtId]
addSqlStmtType t = map (Just . SqlStmtId t . sqlId)

filterSqlStmtType :: SqlStmtType -> [SqlStmt] -> [SqlStmt]
filterSqlStmtType t xs =
  [ x
  | x <- xs 
  , stmtIdType x == Just t ]

removeStmtsMatchingIds
  :: [Maybe SqlStmtId] -- ^ Statement ids to remove
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
  -> (a -> [SqlStmt]) -- ^ drop statment generator
  -> [SqlStmt] -- ^ install statements, representing desired state
  -> IO [SqlStmt]
correctStatements t iois f xs = do
  is <- iois
  return $
    removeStmtsMatchingIds (addSqlStmtType t is) xs ++
    concatMap f (removeSqlIdBySqlStmts t xs is)

abc :: Int
abc = 1
--    if optPermitDataDeletion optUpgrade
--      then stmts
--     else filter
--             (\t -> not (typeEq SqlDropTable t) && not (typeEq SqlDropColumn t))
--             stmts
