-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Database.YamSql.Internal.Schema
  ( Schema(..)
  , SqlContextObj(..)
  , SqlContextSqoArgtypes(..)
  , SqlContextSqo(..)
  , SqlContextSqoObj(..)
  , sqlIdNameOnly
  , module Database.YamSql.Internal.Check
  , module Database.YamSql.Internal.Domain
  , module Database.YamSql.Internal.Function
  , module Database.YamSql.Internal.Role
  , module Database.YamSql.Internal.Sequence
  , module Database.YamSql.Internal.Table
  , module Database.YamSql.Internal.Trigger
  , module Database.YamSql.Internal.Type
  ) where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Check
import Database.YamSql.Internal.Commons
import Database.YamSql.Internal.Domain
import Database.YamSql.Internal.Function
import Database.YamSql.Internal.Role
import Database.YamSql.Internal.Sequence
import Database.YamSql.Internal.Table
import Database.YamSql.Internal.Trigger
import Database.YamSql.Internal.Type

-- Schema --
data Schema = Schema
  { schemaName :: SqlName
  , schemaDescription :: Text
  , schemaDependencies :: Maybe [SqlName]
  , schemaFunctions :: Maybe [Function]
  , schemaFunctionTemplates :: Maybe [FunctionTpl]
  , schemaTables :: Maybe [Table]
  , schemaTableTemplates :: Maybe [TableTpl]
  , schemaColumnTemplates :: Maybe [TableColumnTpl]
  , schemaRoles :: Maybe [Role]
  , schemaSequences :: Maybe [Sequence]
  , schemaTriggers :: Maybe [Trigger]
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
  } deriving (Generic, Show, Data, Typeable)

instance FromJSON Schema where
  parseJSON = strictParseYaml

instance ToJSON Schema where
  toJSON = genericToJSON myOpt

instance ToSqlIdPart Schema where
    sqlIdPart = schemaName
    sqlIdPartType _ = "SCHEMA"

data SqlContextObj a where
   SqlContextObj  :: ToSqlIdPart a => {
    sqlObjectObject :: a
   } -> SqlContextObj a

instance ToSqlId (SqlContextObj a) where
    sqlId (SqlContextObj x) = SqlId $ SqlIdContentObj (sqlIdPartType x) (sqlIdPart x)

data SqlContextSqo a where  SqlContextSqo:: ToSqlIdPart a => {
    sqlSqoSchema:: Schema,
    sqlSqoObject :: a
   } -> SqlContextSqo a

instance ToSqlId (SqlContextSqo a) where
    sqlId (SqlContextSqo s x) = SqlId $ SqlIdContentSqo
        (sqlIdPartType x)
        (sqlIdPart s <.> sqlIdPart x)

data SqlContextSqoArgtypes a where SqlContextSqoArgtypes :: (ToSqlIdPart a, ToSqlIdPartArgs a) => {
    sqlSqoArgtypesSchema:: Schema,
    sqlSqoArgtypesObject :: a
   } -> SqlContextSqoArgtypes a

instance ToSqlId (SqlContextSqoArgtypes a) where
    sqlId (SqlContextSqoArgtypes s x) = SqlId $ SqlIdContentSqoArgtypes
        (sqlIdPartType x)
        (sqlIdPart s <.> sqlIdPart x)
        (sqlIdPartArgs x)

data SqlContextSqoObj a0 a1 where SqlContextSqoObj :: (ToSqlIdPart a0, ToSqlIdPart a1) =>  {
    sqlSqoObjectSchema :: Schema,
    sqlSqoObject1 :: a0,
    sqlSqoObject2 :: a1
} -> SqlContextSqoObj a0 a1

instance ToSqlId (SqlContextSqoObj a0 a1) where
    sqlId (SqlContextSqoObj s x1 x2) = SqlId $ SqlIdContentSqoObj
        (sqlIdPartType x1 ++ "-" ++ sqlIdPartType x2)
        (sqlIdPart s <.> sqlIdPart x1)
        (sqlIdPart x2)

sqlIdNameOnly :: SqlContextSqoArgtypes a -> SqlName
sqlIdNameOnly (SqlContextSqoArgtypes s x) = (sqlIdPart s <.> sqlIdPart x)

