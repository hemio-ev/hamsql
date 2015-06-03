{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Parser.Module where

import Data.Typeable
import Data.Aeson.Types
import Data.Data
import GHC.Generics

import Parser.Basic
import Parser.Check
import Parser.Commons
import Parser.Domain
import Parser.Function
import Parser.Role
import Parser.Sequence
import Parser.Table
import Parser.Type
import Utils

-- Module --

data Module = Module {
    moduleName              :: SqlName,
    moduleDescription       :: String,
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
    moduleExecPostInstall   :: Maybe String,
    xmoduleInternal         :: Maybe ModuleInternal
} deriving (Generic,Show, Data, Typeable)
instance FromJSON Module where parseJSON = strictParseYaml
instance ToJSON Module where toJSON = genericToJSON myOpt
    
moduleInternal :: Module -> ModuleInternal
moduleInternal = fromJustReason "moduleInternal" . xmoduleInternal
    
data ModuleInternal = ModuleInternal {
  moduleLoadPath :: FilePath
} deriving (Data, Generic, Show, Typeable)
instance FromJSON ModuleInternal where parseJSON = strictParseYaml 
instance ToJSON ModuleInternal where toJSON = genericToJSON myOpt

