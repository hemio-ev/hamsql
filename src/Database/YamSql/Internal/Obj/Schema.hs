-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE GADTs #-}

module Database.YamSql.Internal.Obj.Schema
  ( Schema(..)
  , SQL_SCHEMA(..)
  , module Database.YamSql.Internal.Obj.Check
  , module Database.YamSql.Internal.Obj.Domain
  , module Database.YamSql.Internal.Obj.Function
  , module Database.YamSql.Internal.Obj.Role
  , module Database.YamSql.Internal.Obj.Sequence
  , module Database.YamSql.Internal.Obj.Table
  , module Database.YamSql.Internal.Obj.Type
  ) where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Commons
import Database.YamSql.Internal.Obj.Check
import Database.YamSql.Internal.Obj.Domain
import Database.YamSql.Internal.Obj.Function
import Database.YamSql.Internal.Obj.Role
import Database.YamSql.Internal.Obj.Sequence
import Database.YamSql.Internal.Obj.Table
import Database.YamSql.Internal.Obj.Type

-- Schema --
data Schema = Schema
  { schemaName :: SqlName
  , schemaDescription :: Text
  , schemaDependencies :: Maybe [SqlName]
  , schemaFunctions :: Maybe [Function]
  , schemaFunctionTemplates :: Maybe [FunctionTpl]
  , schemaTables :: Maybe [Table]
  , schemaTableTemplates :: Maybe [TableTpl]
  , schemaRoles :: Maybe [Role]
  , schemaSequences :: Maybe [Sequence]
  , schemaPrivUsage :: Maybe [SqlName]
  , schemaPrivSelectAll :: Maybe [SqlName]
  , schemaPrivInsertAll :: Maybe [SqlName]
  , schemaPrivUpdateAll :: Maybe [SqlName]
  , schemaPrivDeleteAll :: Maybe [SqlName]
  , schemaPrivSequenceAll :: Maybe [SqlName]
  , schemaPrivExecuteAll :: Maybe [SqlName]
  , schemaPrivAllAll :: Maybe [SqlName]
  , schemaDomains :: Maybe [Domain]
  , schemaTypes :: Maybe [Type]
  , schemaExecPostInstall :: Maybe Text
    -- TODO: rename to execPostAll
  , schemaExecPostInstallAndUpgrade :: Maybe Text
  } deriving (Generic, Show, Data)

instance FromJSON Schema where
  parseJSON = parseYamSql

instance ToJSON Schema where
  toJSON = toYamSqlJson

data SQL_SCHEMA =
  SQL_SCHEMA
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_SCHEMA where
  toSqlCode = const "SCHEMA"

instance ToSqlId (SqlContext Schema) where
  sqlId (SqlContext s) = SqlId $ SqlObj SQL_SCHEMA (schemaName s)

-- Other stuff
instance ToSqlId (SqlContext (Schema, Table)) where
  sqlId (SqlContext (s, x)) =
    SqlId $ SqlObj SQL_TABLE (schemaName s <.> tableName x)

instance ToSqlId (SqlContext (Schema, Table, Column)) where
  sqlId (SqlContext (s, x, y)) =
    SqlId $ SqlObj SQL_COLUMN (schemaName s <.> tableName x, columnName y)

instance ToSqlId (SqlContext (Schema, Domain)) where
  sqlId (SqlContext (s, x)) =
    SqlId $ SqlObj SQL_DOMAIN (schemaName s <.> domainName x)

instance ToSqlId (SqlContext (Schema, Function)) where
  sqlId (SqlContext (s, x)) =
    SqlId $
    SqlObj
      SQL_FUNCTION
      ( schemaName s <.> functionName x
      , maybeMap variableType $ functionParameters x)

instance ToSqlId (SqlContext (Schema, Sequence)) where
  sqlId (SqlContext (s, x)) =
    SqlId $ SqlObj SQL_SEQUENCE (schemaName s <.> sequenceName x)

instance ToSqlId (SqlContext (Schema, Type)) where
  sqlId (SqlContext (s, x)) =
    SqlId $ SqlObj SQL_TYPE (schemaName s <.> typeName x)
