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

data WithModule a = WithModule Module a
                       
--name :: WithModule Show -> String
class Bla a where
 name :: a -> String

instance Bla (WithModule TableTpl) where
 name (WithModule m t) = toSql [Parser.moduleName m, tabletplTemplate t]

instance Bla (WithModule FunctionTpl) where
 name (WithModule m f) = toSql [Parser.moduleName m, functiontplTemplate f]

instance Bla (WithModule TableColumnTpl) where
 name (WithModule m f) = toSql [Parser.moduleName m, tablecolumntplTemplate f]
--withoutModule :: (WithModule a) -> a
withoutModule (WithModule _ t) = t

--selectTemplates :: [SqlName] -> [WithModule a] -> [a]
selectTemplates xs ts = map withoutModule $ filter (\x -> (name x) `elem` templateNames) ts
 where
  templateNames = map toSql $ maybeList xs
 
--selectTemplate :: a -> b -> TableColumnTpl
--selectTemplate :: t -> [WithModule a] -> a
selectTemplate x ts = h $ map withoutModule $ filter (\y -> (name y) == toSql x) ts
 where
  h [] = error $ "missing something: " ++ toSql x ++ show (map name ts)
  h zs = head zs

-- Setup

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

-- get things from Setup

setupAllModules :: Setup -> [Module]
setupAllModules = setupModuleData . setupInternal

setupAllTables :: Setup -> [Table]
setupAllTables s = concat $ map (maybeList . moduleTables) (setupAllModules s)

setupAllFunctions :: Setup -> [Function]
setupAllFunctions s = concat $ map (maybeList . moduleFunctions) (setupAllModules s)

setupAllFunctionTemplates :: Setup -> [WithModule FunctionTpl]
setupAllFunctionTemplates s = concat $ [
  maybeMap (\x -> (WithModule m x)) (moduleFunctionTemplates m) | m <- (setupAllModules s) ]

setupAllTableTemplates    :: Setup -> [WithModule TableTpl]
setupAllTableTemplates s = concat $ [
  maybeMap (\x -> (WithModule m x)) (moduleTableTemplates m) | m <- (setupAllModules s) ]

setupAllColumnTemplates   :: Setup -> [WithModule TableColumnTpl]
setupAllColumnTemplates s = concat $ [
  maybeMap (\x -> (WithModule m x)) (moduleColumnTemplates m) | m <- (setupAllModules s) ]

setupAllRoles             :: Setup -> [Role]
setupAllRoles s = setupAll s moduleRoles

setupAllDomains           :: Setup -> [Domain]
setupAllDomains s = setupAll s moduleDomains

setupAllTypes             :: Setup -> [Type]
setupAllTypes s = setupAll s moduleTypes

setupAll :: Setup -> (Module -> Maybe [a]) -> [a]
setupAll s f = concat $ map (maybeList . f) (setupAllModules s)

-- end things

outJson :: Setup -> String
outJson s = show $ toJSON s

getModuleByName :: SqlName -> Setup -> Module
getModuleByName name s = get 
    where
    g (SqlName n) = n
    -- oh, we need something to compare SqlNames!
    list = filter (\x -> (Parser.moduleName x) == name) modules
    
    modules = setupModuleData $ setupInternal s

    get
     | length list == 1 = head list
     | length list == 0 = error ("Module not found " ++ toSql name)
     | otherwise       = error ("Found more then one module " ++ toSql name)


-- new implementation
applyTpl :: Setup -> Setup
applyTpl s = s {
    -- TODO: possible overwrite here!
    xsetupInternal = Just SetupInternal {
      setupModuleData =
        map mo (setupModuleData $ setupInternal s)
    }
  }
  
  where
    
    mo m = m {
        moduleTables =  Just $
          map applyColumnTemplates
          $ maybeMap applyTableTemplates (moduleTables m)
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
       f x@(ColumnTpl{}) = applyColumnTpl (columnTemplate x) x
       f x = x

    columnTemplate :: Column -> TableColumnTpl
    columnTemplate c@(ColumnTpl{}) = selectTemplate (columntplTemplate c) (setupAllColumnTemplates s)
    columnTemplate _ = undefined
    
-- end new
     
-- name is outdated
implementAndApplyTemplates :: Setup -> Setup
implementAndApplyTemplates s =
  applySetup $ implementSetup s

  where
    
    implementSetup :: Setup -> Setup
    implementSetup s' = s' {
      xsetupInternal = Just (setupInternal s') {
        setupModuleData =
          map implementModule (setupModuleData $ setupInternal s')
      }
    }
     
    implementModule :: Module -> Module
    implementModule m = m {
      moduleTables = Just $
          maybeMap (implementTableTemplates s m) (moduleTables m),
      moduleFunctions = Just $
          maybeMap (implementFunctionTemplates s m) (moduleFunctions m)
    }
    
    applySetup :: Setup -> Setup
    applySetup s' = s' {
      xsetupInternal = Just (setupInternal s') {
        setupModuleData =
          map applyModule (setupModuleData $ setupInternal s')
      }
    }
    
    applyModule :: Module -> Module
    applyModule m = m {
      moduleTables = Just $
          maybeMap applyTableTemplates (moduleTables m),
      moduleFunctions = Just $
          maybeMap applyFunctionTemplates (moduleFunctions m)
    }

-- Module

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
    
implementFunctionTemplates :: Setup -> Module -> Function -> Function
implementFunctionTemplates s m f = foldr implementFunctionTpl f templateNames
    where
      
    templateNames = maybeList $ functionTemplates f

    implementFunctionTpl :: SqlName -> Function -> Function
    implementFunctionTpl name f' = f' {
        functionTemplateData = 
            Just $ getFunctionTpl name : (maybeList $ functionTemplateData f')
    }
   
    getFunctionTpl :: SqlName -> FunctionTpl
    getFunctionTpl name = selectUnique getTemplates
        where
        getTemplates =
            filter
            (\x -> functiontplTemplate x == tplName name)
            (maybeList$moduleFunctionTemplates$tplModule name)
        selectUnique (x:[]) = x
        selectUnique _ = error $ "Module: " ++ (toSql $ Parser.moduleName m) ++ ". Not exactly one function template " ++ toSql name ++ "."
    
    -- resolve the {tplModule}.tplName syntax
    tplName :: SqlName -> SqlName
    tplName name = last (expSqlName name)
    
    tplModule :: SqlName -> Module
    tplModule name
      -- if no module is given, take current module
      | length (expSqlName name) == 1 = m
      -- otherwise extract module name
      | otherwise =
        getModuleByName (contSqlName $ init $ expSqlName name) s


implementTableTemplates :: Setup -> Module -> Table -> Table
implementTableTemplates s m t = implementColumnTpl $ foldr implementTableTpl t templateNames
    where
      
    templateNames = maybeList $ tableTemplates t

    implementTableTpl :: SqlName -> Table -> Table
    implementTableTpl name t' = t' {
        tableTemplateData = 
            Just $ getTableTpl name : (maybeList $ tableTemplateData t')
    }
    
    implementColumnTpl :: Table -> Table
    implementColumnTpl t' = t' {
      tableColumns = map getColumnTpl (tableColumns t')
    }
   
    getTableTpl :: SqlName -> TableTpl
    getTableTpl name = selectUnique getTemplates
        where
        getTemplates =
            filter
            (\x -> tabletplTemplate x == tplName name)
            (maybeList$moduleTableTemplates$tplModule name)
        selectUnique (x:[]) = x
        selectUnique _ = error $ "Module: " ++ (toSql $ Parser.moduleName m) ++ ". Not exactly one table template '" ++ toSql name ++ "'."
    
    
    getColumnTpl :: Column -> Column
    getColumnTpl c@(ColumnTpl {}) =
      c {
        columntplTemplateData = Just $ getColumnTplByName $ columntplTemplate c
      }
    getColumnTpl c = c

      
    getColumnTplByName :: SqlName -> TableColumnTpl
    getColumnTplByName name = selectUnique getTemplates
      where
        getTemplates =
            filter
            (\x -> tablecolumntplTemplate x == tplName name)
            (maybeList $ moduleColumnTemplates $ tplModule name)
        selectUnique (x:[]) = x
        selectUnique _ = error $ "Module: " ++ (toSql$Parser.moduleName m) ++ ". Not exactly one column template '" ++ toSql name ++ "'."
    
    -- resolve the {tplModule}.tplName syntax
    tplName :: SqlName -> SqlName
    tplName name = last (expSqlName name)
    
    tplModule :: SqlName -> Module
    tplModule name
      -- if no module is given, take current module
      | length (expSqlName name) == 1 = m
      -- otherwise extract module name
      | otherwise =
        getModuleByName (contSqlName $ init $ expSqlName name) s

-- Table
newtype SqlName = SqlName String deriving (Generic,Show,Eq)
instance FromJSON SqlName where parseJSON = genericParseJSON myOpt
instance ToJSON SqlName where toJSON = genericToJSON myOpt
--instance Show SqlName where show (SqlName s) = s

-- SqlName
instance SqlCode SqlName
    where
        toSql n = toSql (expSqlName n)
        (//) (SqlName s) (SqlName t) = SqlName (s ++ t)
        
instance SqlCode [SqlName]
  where
    toSql [] = error "Not allowed: [SqlName]=[]"
    toSql xs = join "." (map getSql xs)
      where
        -- todo: if quotes involved, do something
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

applyTableTemplates :: Table -> Table
applyTableTemplates t =
        applyColumnTemplates applyTemplates
    where
    applyTemplates :: Table
    applyTemplates = foldr applyTableTpl t (maybeList (tableTemplateData t))
    
    applyColumnTemplates :: Table -> Table
    applyColumnTemplates t'' = t'' {
            tableColumns = map applyTableColumnTpl (tableColumns t'')
        }

    applyTableTpl :: TableTpl -> Table -> Table 
    applyTableTpl tpl t' = t' {
            tableColumns    = (maybeList $ tabletplColumns tpl) ++ (tableColumns t'),
            tableInherits   = maybeJoin (tabletplInherits tpl) (tableInherits t'),
            tablePrivSelect = maybeJoin (tabletplPrivSelect tpl) (tablePrivSelect t'),
            tablePrivInsert = maybeJoin (tabletplPrivInsert tpl) (tablePrivInsert t'),
            tablePrivUpdate = maybeJoin (tabletplPrivUpdate tpl) (tablePrivUpdate t'),
            tablePrivDelete = maybeJoin (tabletplPrivDelete tpl) (tablePrivDelete t')
        }

    applyTableColumnTpl :: Column -> Column
    applyTableColumnTpl c@(Column {}) = c
    applyTableColumnTpl c@(ColumnTpl {}) = Column {
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
            tmp :: TableColumnTpl
            tmp = fromJustReason
                    ("Table '" ++ toSql (tableName t) ++ "': Getting column tpl failed: " ++ (show c))
                    (columntplTemplateData c)

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
    -- later populated depending on the value of functionReturn TODO: move to xfunctionInternal
    functionReturnTable     :: Maybe Bool,
    -- parameters the function takes
    functionParameters      :: Maybe [Variable],
    -- list of templates, used for this function
    functionTemplates       :: Maybe [SqlName],
    -- loaded templates, not designed for use via Yaml TODO: move to xfunctionInternal
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
  functionOriginal      :: Function
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
applyFunctionTpl t f =
  f {
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

applyFunctionTemplates :: Function -> Function
applyFunctionTemplates f = foldr deriveFunctionFromTemplate f' (maybeList (functionTemplateData f))
    where
    f'
     | (functionReturn f == "TABLE") = f { functionReturnTable = Just True }
     | otherwise = f { functionReturnTable = Just False }

deriveFunctionFromTemplate :: FunctionTpl -> Function -> Function
deriveFunctionFromTemplate t f =
  f {
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
    
-- Domains
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

-- Types
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


-- Roles
data Role = Role {
    roleName        :: SqlName,
    roleDescription :: String,
    roleLogin       :: Maybe Bool,
    rolePassword    :: Maybe String,
    roleMembers     :: Maybe [SqlName]
} deriving (Generic, Show)
instance FromJSON Role where parseJSON = genericParseJSON myOpt
instance ToJSON Role where toJSON = genericToJSON myOpt

