-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.YamSql.Internal.Schema (Schema(..), SchemaInternal(..), module Database.YamSql.Internal.Check, module Database.YamSql.Internal.Domain, module Database.YamSql.Internal.Function, module Database.YamSql.Internal.Role, module Database.YamSql.Internal.Sequence, module Database.YamSql.Internal.Table, module Database.YamSql.Internal.Type) where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Check
import Database.YamSql.Internal.Commons
import Database.YamSql.Internal.Domain
import Database.YamSql.Internal.Function
import Database.YamSql.Internal.Role
import Database.YamSql.Internal.Sequence
import Database.YamSql.Internal.Table
import Database.YamSql.Internal.Type

-- Schema --

data Schema = Schema {
    schemaName              :: SqlName,
    schemaDescription       :: Text,
    schemaDependencies      :: Maybe [SqlName],

    schemaFunctions         :: Maybe [Function],
    schemaFunctionTemplates :: Maybe [FunctionTpl],
    schemaTables            :: Maybe [Table],
    schemaTableTemplates    :: Maybe [TableTpl],
    schemaColumnTemplates   :: Maybe [TableColumnTpl],
    schemaRoles             :: Maybe [Role],
    schemaSequences         :: Maybe [Sequence],

    schemaPrivUsage         :: Maybe [SqlName],
    schemaPrivSelectAll     :: Maybe [SqlName],
    schemaPrivInsertAll     :: Maybe [SqlName],
    schemaPrivUpdateAll     :: Maybe [SqlName],
    schemaPrivDeleteAll     :: Maybe [SqlName],
    schemaPrivSequenceAll   :: Maybe [SqlName],
    schemaPrivExecuteAll    :: Maybe [SqlName],
    schemaPrivAllAll        :: Maybe [SqlName],
    schemaDomains           :: Maybe [Domain],
    schemaTypes             :: Maybe [Type],
    schemaExecPostInstall   :: Maybe Text,
    xmoduleInternal         :: Maybe SchemaInternal
} deriving (Generic,Show, Data, Typeable)
instance FromJSON Schema where parseJSON = strictParseYaml
instance ToJSON Schema where toJSON = genericToJSON myOpt

data SchemaInternal = SchemaInternal {
  schemaLoadPath :: FilePath
} deriving (Data, Generic, Show, Typeable)
instance FromJSON SchemaInternal where parseJSON = strictParseYaml
instance ToJSON SchemaInternal where toJSON = genericToJSON myOpt

