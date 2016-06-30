-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.YamSql.Internal.Module (Module(..), ModuleInternal(..), module Database.YamSql.Internal.Check, module Database.YamSql.Internal.Domain, module Database.YamSql.Internal.Function, module Database.YamSql.Internal.Role, module Database.YamSql.Internal.Sequence, module Database.YamSql.Internal.Table, module Database.YamSql.Internal.Type) where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Check
import Database.YamSql.Internal.Commons
import Database.YamSql.Internal.Domain
import Database.YamSql.Internal.Function
import Database.YamSql.Internal.Role
import Database.YamSql.Internal.Sequence
import Database.YamSql.Internal.Table
import Database.YamSql.Internal.Type

-- Module --

data Module = Module {
    moduleName              :: SqlName,
    moduleDescription       :: Text,
    moduleDependencies      :: Maybe [SqlName],

    moduleFunctions         :: Maybe [Function],
    moduleFunctionTemplates :: Maybe [FunctionTpl],
    moduleTables            :: Maybe [Table],
    moduleTableTemplates    :: Maybe [TableTpl],
    moduleColumnTemplates   :: Maybe [TableColumnTpl],
    moduleRoles             :: Maybe [Role],
    moduleSequences         :: Maybe [Sequence],

    modulePrivUsage         :: Maybe [SqlName],
    modulePrivSelectAll     :: Maybe [SqlName],
    modulePrivInsertAll     :: Maybe [SqlName],
    modulePrivUpdateAll     :: Maybe [SqlName],
    modulePrivDeleteAll     :: Maybe [SqlName],
    modulePrivSequenceAll   :: Maybe [SqlName],
    modulePrivExecuteAll    :: Maybe [SqlName],
    modulePrivAllAll        :: Maybe [SqlName],
    moduleDomains           :: Maybe [Domain],
    moduleTypes             :: Maybe [Type],
    moduleExecPostInstall   :: Maybe Text,
    xmoduleInternal         :: Maybe ModuleInternal
} deriving (Generic,Show, Data, Typeable)
instance FromJSON Module where parseJSON = strictParseYaml
instance ToJSON Module where toJSON = genericToJSON myOpt

data ModuleInternal = ModuleInternal {
  moduleLoadPath :: FilePath
} deriving (Data, Generic, Show, Typeable)
instance FromJSON ModuleInternal where parseJSON = strictParseYaml
instance ToJSON ModuleInternal where toJSON = genericToJSON myOpt

