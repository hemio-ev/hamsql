-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Data.Yaml
import Data.Aeson.Types
import Data.Char
import Data.Maybe (fromJust)
import GHC.Generics
import Data.String.Utils
import Data.List

import qualified Data.ByteString.Char8 as B

import Utils

-- removes first part of camel case. e.g.:
-- columnDescriptionField |-> descriptionField
removeFirstPart :: String -> String
removeFirstPart xs = lowerStr rest
     where
        rest = dropWhile isLower xs
        lowerStr (x':xs') = toLower x':xs'
        lowerStr []       = "__"

-- makes camelCaseSpelling to camel_case_spelling
snakeify :: String -> String
snakeify [] = []
snakeify (x:xs) | isUpper x = '_' : toLower x : snakeify xs
snakeify (x:xs) | otherwise =               x : snakeify xs

myOpt :: Options
myOpt = defaultOptions { fieldLabelModifier     = snakeify . removeFirstPart
                       , constructorTagModifier = drop 1 . snakeify}

outJson :: Setup -> String
outJson s = show $ toJSON s

-- SqlCode (right now only SqlName)
                       
instance SqlCode SqlName
    where
        toSql n = toSql (expSqlName n)
        (//) (SqlName s) (SqlName t) = SqlName (s ++ t)
        
instance SqlCode [SqlName]
  where
    toSql [] = error "Not allowed: [SqlName]=[]"
    toSql xs = join "." (map getSql xs)
      where
        -- TODO: if quotes involved, do something
        getSql (SqlName s) = "\"" ++ s ++ "\""
    (//) a b = a ++ b

expSqlName :: SqlName -> [SqlName]
expSqlName n = map SqlName (split "." (getStr n))
  where
    getStr (SqlName n') = n'

contSqlName :: [SqlName] -> SqlName
contSqlName ns = SqlName $ (join ".") $ map getStr ns
  where
    getStr (SqlName n') = n'
    
class SqlCode a where
  toSql :: a -> String
  (//) :: a -> a -> a

-- SqlName
newtype SqlName = SqlName String deriving (Generic,Show,Eq)
instance FromJSON SqlName where parseJSON = genericParseJSON myOpt
instance ToJSON SqlName where toJSON = genericToJSON myOpt
  
-- Setup --

data Setup = Setup {
  setupModules    :: [String],
  setupModuleDirs :: [FilePath],
  setupPreCode    :: Maybe String,
  setupPostCode   :: Maybe String,
  xsetupInternal  :: Maybe SetupInternal
} deriving (Generic,Show)
instance FromJSON Setup where parseJSON = genericParseJSON myOpt
instance ToJSON Setup where toJSON = genericToJSON myOpt

data SetupInternal = SetupInternal {
  setupModuleData :: [Module]
} deriving (Generic,Show)
instance FromJSON SetupInternal where parseJSON = genericParseJSON myOpt
instance ToJSON SetupInternal where toJSON = genericToJSON myOpt

setupInternal :: Setup -> SetupInternal
setupInternal s = fromJust $ xsetupInternal s

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
    moduleDomains           :: Maybe [Domain],
    moduleTypes             :: Maybe [Type],
    xmoduleInternal         :: Maybe ModuleInternal
} deriving (Generic,Show)
instance FromJSON Module where parseJSON = genericParseJSON myOpt
instance ToJSON Module where toJSON = genericToJSON myOpt
    
moduleInternal :: Module -> ModuleInternal
moduleInternal = (fromJustReason "moduleInternal") . xmoduleInternal
    
data ModuleInternal = ModuleInternal {
  moduleLoadPath :: FilePath
} deriving (Generic,Show)
instance FromJSON ModuleInternal where parseJSON = genericParseJSON myOpt
instance ToJSON ModuleInternal where toJSON = genericToJSON myOpt

-- Table --

data Table = Table {
  tableName         :: SqlName,
  tableDescription  :: String,
  tableColumns      :: [Column],
  tablePrimaryKey   :: [SqlName],
  tableUnique       :: Maybe [[SqlName]],
  tableChecks       :: Maybe [Check],
  tableInherits     :: Maybe [SqlName],
  tablePrivSelect   :: Maybe [String],
  tablePrivInsert   :: Maybe [String],
  tablePrivUpdate   :: Maybe [String],
  tablePrivDelete   :: Maybe [String],
  tableTemplates    :: Maybe [SqlName],
  tableTemplateData :: Maybe [TableTpl],
  xtableInternal :: Maybe TableInternal
} deriving (Generic, Show)
instance FromJSON Table where parseJSON = genericParseJSON myOpt
instance ToJSON Table where toJSON = genericToJSON myOpt

tableInternal :: Table -> TableInternal
tableInternal = (fromJustReason "tableInternal") . xtableInternal

data TableInternal = TableInternal {
  tableParentModule :: Module,
  tableLoadPath     :: FilePath,
  tableOriginal     :: Table
}deriving (Generic, Show)
instance FromJSON TableInternal where parseJSON = genericParseJSON myOpt
instance ToJSON TableInternal where toJSON = genericToJSON myOpt

data Column = Column {
    columnName            :: SqlName,
    columnType            :: String,
    columnDescription     :: String,
    columnDefault         :: Maybe String,
    columnNull            :: Maybe Bool,
    columnReferences      :: Maybe SqlName,
    columnOnRefDelete     :: Maybe String,
    columnOnRefUpdate     :: Maybe String,
    columnUnique          :: Maybe Bool
} | ColumnTpl {
    columntplTemplate     :: SqlName,
    columntplTemplateData :: Maybe TableColumnTpl,
    columntplName         :: Maybe SqlName,
    columntplType         :: Maybe String,
    columntplDescription  :: Maybe String,
    columntplDefault      :: Maybe String,
    columntplNull         :: Maybe Bool,
    columntplReferences   :: Maybe SqlName,
    columntplOnRefDelete  :: Maybe String,
    columntplOnRefUpdate  :: Maybe String,
    columntplUnique       :: Maybe Bool
} deriving (Generic, Show)
instance FromJSON Column where parseJSON = genericParseJSON myOpt
instance ToJSON Column where toJSON = genericToJSON myOpt
      
data Check = Check {
    checkName        :: SqlName,
    checkDescription :: String,
    checkCheck       :: String
} deriving (Generic, Show)
instance FromJSON Check where parseJSON = genericParseJSON myOpt
instance ToJSON Check where toJSON = genericToJSON myOpt

data TableTpl = TableTpl {
    tabletplTemplate   :: SqlName,
    tabletplInherits   :: Maybe [SqlName],
    tabletplColumns    :: Maybe [Column],
    tabletplPrivSelect :: Maybe [String],
    tabletplPrivInsert :: Maybe [String], 
    tabletplPrivUpdate :: Maybe [String],
    tabletplPrivDelete :: Maybe [String]
} deriving (Generic, Show)
instance FromJSON TableTpl where parseJSON = genericParseJSON myOpt
instance ToJSON TableTpl where toJSON = genericToJSON myOpt

data TableColumnTpl = TableColumnTpl {
    tablecolumntplTemplate     :: SqlName,
    tablecolumntplName         :: SqlName,
    tablecolumntplType         :: String,
    tablecolumntplDescription  :: String,
    tablecolumntplDefault      :: Maybe String,
    tablecolumntplNull         :: Maybe Bool,
    tablecolumntplReferences   :: Maybe SqlName,
    tablecolumntplOnRefDelete  :: Maybe String,
    tablecolumntplOnRefUpdate  :: Maybe String,
    tablecolumntplUnique       :: Maybe Bool
} deriving (Generic, Show)
instance FromJSON TableColumnTpl where parseJSON = genericParseJSON myOpt
instance ToJSON TableColumnTpl where toJSON = genericToJSON myOpt

applyTableTpl :: TableTpl -> Table -> Table
applyTableTpl tpl t = t {
    tableColumns    = (maybeList $ tabletplColumns tpl) ++ (tableColumns t),
    tableInherits   = maybeJoin (tabletplInherits tpl) (tableInherits t),
    tablePrivSelect = maybeJoin (tabletplPrivSelect tpl) (tablePrivSelect t),
    tablePrivInsert = maybeJoin (tabletplPrivInsert tpl) (tablePrivInsert t),
    tablePrivUpdate = maybeJoin (tabletplPrivUpdate tpl) (tablePrivUpdate t),
    tablePrivDelete = maybeJoin (tabletplPrivDelete tpl) (tablePrivDelete t)
  }

applyColumnTpl :: TableColumnTpl -> Column -> Column
applyColumnTpl tmp c = Column {
    columnName        = maybeRight' (tablecolumntplName tmp) (columntplName c),
    columnType        = maybeRight' (tablecolumntplType tmp) (columntplType c),
    columnDescription = maybeRight' (tablecolumntplDescription tmp) (columntplDescription c),
    columnDefault     = maybeRight (tablecolumntplDefault tmp) (columntplDefault c),
    columnNull        = maybeRight (tablecolumntplNull tmp) (columntplNull c),
    columnReferences  = maybeRight (tablecolumntplReferences tmp) (columntplReferences c),
    columnOnRefDelete = maybeRight (tablecolumntplOnRefDelete tmp) (columntplOnRefDelete c),
    columnOnRefUpdate = maybeRight (tablecolumntplOnRefUpdate tmp) (columntplOnRefUpdate c),
    columnUnique      = maybeRight (tablecolumntplUnique tmp) (columntplUnique c)
  }
  where
    maybeRight' :: a -> Maybe a -> a
    maybeRight' x Nothing = x
    maybeRight' _ (Just y) = y

-- Function

data Function = Function {
    -- function name
    functionName            :: SqlName,
    -- description what the function is good for
    functionDescription     :: String,
    -- return type of the function, TABLE is special (see return_columns)
    functionReturn          :: String,
    -- parameters the function takes
    functionParameters      :: Maybe [Variable],
    -- list of templates, used for this function
    functionTemplates       :: Maybe [SqlName],
    -- loaded templates, not designed for use via Yaml
    -- TODO: move to xfunctionInternal
    functionTemplateData    :: Maybe [FunctionTpl],
    -- if return is TABLE, gives the columns that are returned (see parameter)
    functionReturnColumns   :: Maybe [Parameter],
    -- variables that are defined (ignored if language is given)
    functionVariables       :: Maybe [Variable],
    -- Role that has the privilege to execute the function
    functionPrivExecute     :: Maybe [SqlName],
    -- If true, the function is executed with the privileges of the owner!
    -- Owner has to be given, if this is true (not implemented yet!)
    functionSecurityDefiner :: Maybe Bool,
    -- owner of the function
    functionOwner           :: Maybe String,
    -- language in which the body is written
    -- if not defined, pgsql is assumed an variables must be defined via variables
    -- if pgsql is given explicitly, variables are your problem...
    functionLanguage        :: Maybe String,
    -- the code of the function (body)
    functionBody            :: String,
    xfunctionInternal       :: Maybe FunctionInternal
} deriving (Generic,Show)
instance FromJSON Function where parseJSON = genericParseJSON myOpt
instance ToJSON Function where toJSON = genericToJSON myOpt

functionInternal :: Function -> FunctionInternal
functionInternal = (fromJustReason "FunctionInternal") . xfunctionInternal

data FunctionInternal = FunctionInternal {
  functionParentModule  :: Module,
  functionLoadPath      :: FilePath,
  functionOriginal      :: Function,
  -- populated depending on the value of functionReturn
  functionReturnTable   :: Bool
} deriving (Generic,Show)
instance FromJSON FunctionInternal where parseJSON = genericParseJSON myOpt
instance ToJSON FunctionInternal where toJSON = genericToJSON myOpt

data Variable = Variable {
    variableName          :: SqlName,
    variableDescription   :: Maybe String,
    variableType          :: String,
    variableDefault       :: Maybe String
} deriving (Generic,Show)
instance FromJSON Variable where parseJSON = genericParseJSON myOpt
instance ToJSON Variable where toJSON = genericToJSON myOpt

data Parameter = Parameter {
    parameterName          :: SqlName,
    parameterDescription   :: Maybe String,
    parameterType          :: String
} deriving (Generic,Show)
instance FromJSON Parameter where parseJSON = genericParseJSON myOpt
instance ToJSON Parameter where toJSON = genericToJSON myOpt

data FunctionTpl = FunctionTpl {
    -- template name, used to refere the template via templates
    functiontplTemplate        :: SqlName,
    -- description what the template is good for
    functiontplDescription     :: String,
    -- language of the function has to be the same as for used templates
    -- TODO: implement checks to avoid explosions here ;)
    functiontplLanguage        :: Maybe String,
    -- variables are appended to the functions variables
    functiontplVariables       :: Maybe [Variable],
    -- defines priv_execute, can be overwritten by function definition
    functiontplPrivExecute     :: Maybe [SqlName],
    -- defines security_definer, can be overwritten by function definition
    functiontplSecurityDefiner :: Maybe Bool,
    -- defines owner, can be overwritten by function definition
    functiontplOwner           :: Maybe String,
    -- code added before the body of the function
    functiontplBodyPrelude     :: Maybe String,
    -- code added after the body of the function
    functiontplBodyPostlude    :: Maybe String
} deriving (Generic,Show)
instance FromJSON FunctionTpl where parseJSON = genericParseJSON myOpt
instance ToJSON FunctionTpl where toJSON = genericToJSON myOpt

applyFunctionTpl :: FunctionTpl -> Function -> Function
applyFunctionTpl t f = f {
    functionPrivExecute =
      maybeRight (functiontplPrivExecute t) (functionPrivExecute f),

    functionSecurityDefiner =
      maybeRight (functiontplSecurityDefiner t) (functionSecurityDefiner f),

    functionOwner =
      maybeRight (functiontplOwner t) (functionOwner f),

    functionVariables =
      maybeJoin (functionVariables f) (functiontplVariables t),
        
    functionBody =
      (maybeStringL $ functiontplBodyPrelude t) ++
      functionBody f ++
      (maybeStringR $ functiontplBodyPostlude t)
        
  }
  where
    maybeStringL (Just xs) = xs ++ "\n"
    maybeStringL Nothing = ""
    maybeStringR (Just xs) = "\n" ++ xs
    maybeStringR Nothing = ""

-- Domains --

data Domain = Domain {
    domainName :: SqlName,
    domainDescription :: String,
    domainType  :: String,
    domainDefault :: Maybe String,
    domainChecks :: Maybe [Check],
    xdomainInternal :: Maybe DomainInternal
} deriving (Generic, Show)
instance FromJSON Domain where parseJSON = genericParseJSON myOpt
instance ToJSON Domain where toJSON = genericToJSON myOpt

domainInternal :: Domain -> DomainInternal
domainInternal = (fromJustReason "DomainInternal") . xdomainInternal

data DomainInternal = DomainInternal {
  domainParentModule  :: Module,
  domainLoadPath      :: FilePath,
  domainOriginal      :: Domain
} deriving (Generic, Show)
instance FromJSON DomainInternal where parseJSON = genericParseJSON myOpt
instance ToJSON DomainInternal where toJSON = genericToJSON myOpt

-- Types --

data Type = Type {
    typeName :: SqlName,
    typeDescription :: String,
    typeElements :: [TypeElement],
    xtypeInternal :: Maybe TypeInternal
} deriving (Generic, Show)
instance FromJSON Type where parseJSON = genericParseJSON myOpt
instance ToJSON Type where toJSON = genericToJSON myOpt
      
typeInternal :: Type -> TypeInternal
typeInternal = (fromJustReason "TypeInternal") . xtypeInternal

data TypeInternal = TypeInternal {
  typeParentModule  :: Module,
  typeLoadPath      :: FilePath,
  typeOriginal      :: Type
} deriving (Generic, Show)
instance FromJSON TypeInternal where parseJSON = genericParseJSON myOpt
instance ToJSON TypeInternal where toJSON = genericToJSON myOpt

data TypeElement = TypeElement {
    typeelementName :: SqlName,
    typeelementType :: String
} deriving (Generic, Show)
instance FromJSON TypeElement where parseJSON = genericParseJSON myOpt
instance ToJSON TypeElement where toJSON = genericToJSON myOpt

-- Roles --

data Role = Role {
    roleName        :: SqlName,
    roleDescription :: String,
    roleLogin       :: Maybe Bool,
    rolePassword    :: Maybe String,
    roleMembers     :: Maybe [SqlName]
} deriving (Generic, Show)
instance FromJSON Role where parseJSON = genericParseJSON myOpt
instance ToJSON Role where toJSON = genericToJSON myOpt

-- Template handling and applyTemplate

data WithModule a = WithModule Module a
           
class WithName a where
 name :: a -> String

instance WithName (WithModule TableTpl) where
 name (WithModule m t) = toSql [Parser.moduleName m, tabletplTemplate t]

instance WithName (WithModule FunctionTpl) where
 name (WithModule m f) = toSql [Parser.moduleName m, functiontplTemplate f]

instance WithName (WithModule TableColumnTpl) where
 name (WithModule m f) = toSql [Parser.moduleName m, tablecolumntplTemplate f]

withoutModule (WithModule _ t) = t

selectTemplates ns ts = 
  -- TODO: error handling here should be done using exceptions
  [ withoutModule $ selectUniqueReason ("table or function tpl " ++ n) $
    filter (\t -> n == (name t)) ts 
    | n <- map toSql $ maybeList ns ]
 
selectTemplate x ts = head' $ map withoutModule $ filter (\y -> (name y) == toSql x) ts
  where
    head' zs = selectUniqueReason ("Column template " ++ toSql x) zs
    
-- get things from Setup

setupAllModules :: Setup -> [Module]
setupAllModules = setupModuleData . setupInternal

setupAllFunctionTemplates :: Setup -> [WithModule FunctionTpl]
setupAllFunctionTemplates s = concat $ [
  maybeMap (\x -> (WithModule m x)) (moduleFunctionTemplates m) | m <- (setupAllModules s) ]

setupAllTableTemplates    :: Setup -> [WithModule TableTpl]
setupAllTableTemplates s = concat $ [
  maybeMap (\x -> (WithModule m x)) (moduleTableTemplates m) | m <- (setupAllModules s) ]

setupAllColumnTemplates   :: Setup -> [WithModule TableColumnTpl]
setupAllColumnTemplates s = concat $ [
  maybeMap (\x -> (WithModule m x)) (moduleColumnTemplates m) | m <- (setupAllModules s) ]

applyTpl :: Setup -> Setup
applyTpl s = s {
    -- TODO: possible overwrite here!
    xsetupInternal = Just SetupInternal {
      setupModuleData =
        map applyModule (setupModuleData $ setupInternal s)
    }
  }
  
  where
    
    applyModule m = m {
        moduleTables =  Just $
          map applyColumnTemplates
          $ maybeMap applyTableTemplates (moduleTables m),
            
        moduleFunctions = Just $
          maybeMap applyFunctionTemplates (moduleFunctions m)
      }
      
    applyTableTemplates :: Table -> Table 
    applyTableTemplates t = foldr applyTableTpl t (tableTpls t)

    tableTpls :: Table -> [TableTpl]
    tableTpls t = selectTemplates (tableTemplates t) (setupAllTableTemplates s)
    
    applyFunctionTemplates :: Function -> Function 
    applyFunctionTemplates f = foldr applyFunctionTpl f (functionTpls f)

    functionTpls :: Function -> [FunctionTpl]
    functionTpls f = selectTemplates (functionTemplates f) (setupAllFunctionTemplates s)

    applyColumnTemplates :: Table -> Table
    applyColumnTemplates t = t { tableColumns = map f (tableColumns t) }
     where
       f x@(ColumnTpl{}) = applyColumnTpl (columnTpl x) x
       f x = x

    columnTpl :: Column -> TableColumnTpl
    columnTpl c@(ColumnTpl{}) = selectTemplate (columntplTemplate c) (setupAllColumnTemplates s)
    columnTpl _ = undefined
    