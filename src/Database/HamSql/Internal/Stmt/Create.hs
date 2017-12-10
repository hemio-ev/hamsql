-- This file is part of HamSql
--
-- Copyright 2014-2015 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.Stmt.Create where

import Data.Maybe

import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Stmt.Basic
import Database.HamSql.Internal.Stmt.Commons ()
import Database.HamSql.Internal.Stmt.Database ()
import Database.HamSql.Internal.Stmt.Domain ()
import Database.HamSql.Internal.Stmt.Function ()
import Database.HamSql.Internal.Stmt.Role ()
import Database.HamSql.Internal.Stmt.Schema ()
import Database.HamSql.Internal.Stmt.Sequence ()
import Database.HamSql.Internal.Stmt.Table ()
import Database.HamSql.Internal.Stmt.Trigger ()
import Database.HamSql.Internal.Stmt.Type ()

allSchemaElements :: Schema -> [SetupElement]
allSchemaElements schema =
  [SetupElement $ SqlContext schema] ++
  toElemList' schemaRoles schema ++
  toElemList _schemaDomains schema ++
  toElemList _schemaFunctions schema ++
  toElemList _schemaSequences schema ++
  toElemList _schemaTables schema ++
  toElemList _schemaTypes schema ++
  concat
    [ map
      (SetupElement . (\x -> SqlContext (schema, table, x)))
      (_tableColumns table) ++
    map
      (SetupElement . (\x -> SqlContext (schema, table, x)))
      (fromMaybe [] $ tableTriggers table)
    | table <- fromMaybe [] $ _schemaTables schema
    ]
  where
    toElemList y = maybeMap (SetupElement . (\x -> SqlContext (schema, x))) . y
    toElemList' y = maybeMap (SetupElement . SqlContext) . y

elementsToStmts :: SetupContext -> [SetupElement] -> [Maybe SqlStmt]
elementsToStmts setupContext = concatMap (toSqlStmts setupContext)

data SQL_OTHER =
  SQL_OTHER
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_OTHER where
  toSqlCode = const "SQL_OTHER"

emptyName :: SqlId
emptyName = SqlId $ SqlObj SQL_OTHER $ SqlName ""

sqlAddTransact :: [SqlStmt] -> [SqlStmt]
sqlAddTransact xs =
  catMaybes [newSqlStmt SqlUnclassified emptyName "BEGIN TRANSACTION"] ++
  xs ++ catMaybes [newSqlStmt SqlUnclassified emptyName "COMMIT"]

getRoleStmts :: Setup -> [Role] -> [Maybe SqlStmt]
getRoleStmts s r =
  concat $ map (toSqlStmts (SetupContext s)) $ map (SetupElement . SqlContext) r

-- | Setup
getSetupStatements :: Setup -> [Maybe SqlStmt]
getSetupStatements s =
  [getStmt $ setupPreCode s] ++
  schemaStatements ++ myStmts ++ [getStmt $ setupPostCode s]
  where
    schemaStatements =
      concat $ maybeMap (getSchemaStatements s) (_setupSchemaData s)
    myStmts = getRoleStmts s $ fromMaybe [] $ setupRoles s
    getStmt (Just code) = newSqlStmt SqlPre emptyName code
    getStmt Nothing = Nothing

getSchemaStatements :: Setup -> Schema -> [Maybe SqlStmt]
getSchemaStatements setup s =
  elementsToStmts (SetupContext setup) $ allSchemaElements s
