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

== Sequences

CREATE only defines name on demand.

Properties all via ALTER SEQUENCE.

-}
module Database.HamSql.Internal.PostgresCon
  ( stmtsInstall
  , pgsqlExecWithoutTransact
  , pgsqlExec
  , upgradeStmts
  , normalizeOnline
  , pgsqlDropAllRoleStmts
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as B

--import Data.Function
import Data.Maybe

import Data.List ((\\), sort)

--import Data.Set (fromList, notMember)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Transaction
import Network.URI (URI)

import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.InquireDeployed
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Stmt.Create
import Database.HamSql.Internal.Stmt.Domain
import Database.HamSql.Internal.Stmt.Function
import Database.HamSql.Internal.Stmt.Role
import Database.HamSql.Internal.Stmt.Schema
import Database.HamSql.Internal.Stmt.Sequence
import Database.HamSql.Internal.Stmt.Table
import Database.HamSql.Internal.Stmt.Trigger
import Database.HamSql.Internal.Stmt.Type
import Database.HamSql.Internal.Utils
import Database.HamSql.Setup
import Database.YamSql

sqlErrInvalidFunctionDefinition :: B.ByteString
sqlErrInvalidFunctionDefinition = "42P13"

sqlErrUndefinedTable :: B.ByteString
sqlErrUndefinedTable = "42P01"

stmtsInstall :: Setup -> [SqlStmt]
stmtsInstall setup = catMaybes $ getSetupStatements setup

pgsqlDropAllRoleStmts :: Setup -> SqlT [SqlStmt]
pgsqlDropAllRoleStmts s =
  stmtsUpdateDrop s <$> catMaybes <$> getRoleStmts s <$>
  inquireRoles (setupRolePrefix s)

upgradeStmts :: Setup -> Setup -> [SqlStmt]
upgradeStmts sourceSetup targetSetup =
  let sourceStmts = stmtsInstall sourceSetup
      targetStmts = stmtsInstall targetSetup
  in sort $
     (sourceStmts \\ targetStmts) ++
     stmtsUpdateDrop sourceSetup (targetStmts \\ sourceStmts)

stmtsUpdateDrop :: Setup -> [SqlStmt] -> [SqlStmt]
stmtsUpdateDrop s = catMaybes . concatMap (dropStmt s)

dropStmt :: Setup -> SqlStmt -> [Maybe SqlStmt]
dropStmt setup (SqlStmt (SqlStmtId t i) _) =
  let n = SqlName $ toSqlCode i
      s = expSqlName n
      ncol = ((s !! 0) <.> (s !! 1), s !! 2)
  in case t of
       SqlAddColumn -> stmtsDropTableColumn (SqlObj SQL_COLUMN ncol)
       SqlCreateDomain -> stmtsDropDomain (SqlObj SQL_DOMAIN n)
       SqlCreateSequence -> stmtsDropSequence (SqlObj SQL_SEQUENCE n)
       SqlCreateTable -> stmtsDropTable (SqlObj SQL_TABLE n)
       SqlCreateType -> stmtsDropType (SqlObj SQL_TYPE n)
       SqlCreateFunction -> return <$> stmtsDropFunction' i
       SqlCreateTrigger -> stmtsDropTrigger (SqlObj SQL_TRIGGER ncol)
       SqlCreateTableCheckConstr ->
         stmtsDropTableConstr (SqlObj SQL_TABLE_CONSTRAINT ncol)
       SqlCreateForeignKeyConstr ->
         stmtsDropTableConstr (SqlObj SQL_TABLE_CONSTRAINT ncol)
       SqlCreateRole -> stmtsDropRole setup (SqlObj SQL_ROLE n)
       SqlDropSchema -> stmtsDropSchema (SqlObj SQL_SCHEMA n)
       _ -> []

normalizeOnline :: Setup -> SqlT Setup
normalizeOnline set = applyColumnTypes set >>= applyFunctionTypes
  where
    applyColumnTypes s =
      foldM (\x y -> traverseOf y normalizeColumnTypeOnline x) s lensColumTypes
    applyFunctionTypes s =
      foldM
        (\x y -> traverseOf y normalizeFunctionTypeOnline x)
        s
        lensFunctionTypes

lensFunctionTypes :: Applicative m => [LensLike' m Setup SqlType]
lensFunctionTypes =
  [ eachFunction . functionParameters . _Just . each . variableType
  , eachFunction . functionReturns . _ReturnType
  , eachFunction . functionReturns . _ReturnTypeSetof
  , eachFunction . functionReturns . _ReturnTypeTable . each . parameterType
  ]
  where
    eachFunction =
      setupSchemaData . _Just . each . schemaFunctions . _Just . each

lensColumTypes :: Applicative m => [LensLike' m Setup SqlType]
lensColumTypes =
  [ setupSchemaData . _Just . each . schemaDomains . _Just . each . domainType
  , setupSchemaData .
    _Just .
    each . schemaTypes . _Just . each . typeElements . each . typeelementType
  , setupSchemaData .
    _Just .
    each . schemaTables . _Just . each . tableColumns . each . columnType
  ]

normalizeTypeOnline :: SqlType -> SqlT SqlType
normalizeTypeOnline t
  | t == SqlType "TABLE" = return t
  | otherwise = do
    xs <- psqlQry "SELECT to_regtype(?)::text" (Only $ toSqlCode t)
    return $ fromMaybe t (fromOnly $ head xs)

normalizeColumnTypeOnline :: SqlType -> SqlT SqlType
normalizeColumnTypeOnline t
  | '(' `isIn` (toSqlCode t) = return t
  | otherwise = normalizeTypeOnline t

normalizeFunctionTypeOnline :: SqlType -> SqlT SqlType
normalizeFunctionTypeOnline t
  | '%' `isIn` (toSqlCode t) = return t
  | otherwise = normalizeTypeOnline t

{-
revokeAllPrivileges ::
     Connection
  -> Setup
  -> IO [SqlObj SQL_ROLE SqlName]
  -> [SqlStmt]
  -> IO [SqlStmt]
revokeAllPrivileges conn setup roles stmts = do
  schemas <- map (\(SqlObj SQL_SCHEMA x) -> x) <$> deployedSchemaIds conn
  ((++ stmts) <$> catMaybes) . concatMap (stmtsDropAllPrivileges setup schemas) <$>
    roles

pgsqlDropAllRoleStmts :: OptCommonDb -> Setup -> IO [SqlStmt]
pgsqlDropAllRoleStmts optDb setup = do
  conn <- pgsqlConnectUrl $ getConUrl optDb
  addDropResidual
    SqlCreateRole
    (deployedRoleIds setup conn)
    (stmtsDropRole setup)
    []
-}
-- DB Utils
pgsqlExecWithoutTransact :: OptCommonDb -> URI -> [SqlStmt] -> IO Connection
pgsqlExecWithoutTransact opt = pgsqlExecIntern opt PgSqlWithoutTransaction

pgsqlExec :: OptCommonDb -> URI -> [SqlStmt] -> IO Connection
pgsqlExec opt = pgsqlExecIntern opt PgSqlWithTransaction

pgsqlExecStmtList ::
     OptCommonDb
  -> Status
  -> [SqlStmt] -- ^ Statements that still need to be executed
  -> [SqlStmt] -- ^ Statements that have failed during execution
  -> Connection
  -> IO ()
pgsqlExecStmtList _ Init _ (x:_) _ =
  err $ "supplied failed statements to (pgsqlExecStmtList _ Init): " <> tshow x
-- No remaining statements to execute
pgsqlExecStmtList dbOpt _ [] [] conn
  | optEmulate dbOpt = rollback conn
  | otherwise = commit conn
pgsqlExecStmtList _ Unchanged [] failed conn =
  pgsqlExecStmtHandled conn (head failed)
pgsqlExecStmtList opt Changed [] failed conn =
  void $ pgsqlExecStmtList opt Unchanged failed [] conn
pgsqlExecStmtList opt status (x:xs) failed conn = do
  savepoint <- newSavepoint conn
  tryExec savepoint `catch` handleSqlError savepoint `catch`
    handleQueryError savepoint
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
      -- SEQENCEs might be gone allready
      | errCode == sqlErrUndefinedTable = skipQuery savepoint []
      | otherwise = skipQuery savepoint [x]
    handleQueryError savepoint QueryError {} = proceed savepoint
    -- action after execution has failed
    skipQuery savepoint stmts = do
      unless (optSqlLogHideRollbacks opt) $ do
        logStmt opt "SAVEPOINT retry;"
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
  when (mode == PgSqlWithTransaction) $ do
    begin conn
    pgsqlExecStmtList opt Init xs [] conn
  when (mode == PgSqlWithoutTransaction) $ mapM_ (pgsqlExecStmtHandled conn) xs
  return conn
