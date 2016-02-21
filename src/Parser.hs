-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Parser where

import Control.Exception
import Data.Typeable
import Data.Yaml ()
import Data.Aeson.Types
import Data.Char
import Data.Maybe (fromJust,fromMaybe)
import Data.Data
import Data.Text (unpack)
import Data.List.Ordered (subset,minus,sort)
import GHC.Generics
import Data.List.Split (splitOn)
import Data.HashMap.Strict (member,insert,keys)
import System.IO
import qualified Data.ByteString.Char8 as B

import Utils
import Parser.Basic
import Parser.Function
import Parser.Module
import Parser.Table
  
-- Setup --

data Setup = Setup {
  setupModules    :: [String],
  setupModuleDirs :: [FilePath],
  setupRolePrefix :: Maybe SqlName,
  setupPreCode    :: Maybe String,
  setupPostCode   :: Maybe String,
  xsetupInternal  :: Maybe SetupInternal
} deriving (Generic,Show,Data,Typeable)
instance FromJSON Setup where parseJSON = strictParseYaml
instance ToJSON Setup where toJSON = genericToJSON myOpt

data SetupInternal = SetupInternal {
  setupModuleData :: [Module]
} deriving (Generic,Show, Data, Typeable)
instance FromJSON SetupInternal where parseJSON = strictParseYaml
instance ToJSON SetupInternal where toJSON = genericToJSON myOpt

setupInternal :: Setup -> SetupInternal
setupInternal s = fromJust $ xsetupInternal s

setupRolePrefix' setup = fromMaybe (SqlName "yamsql_") (setupRolePrefix setup)

-- Template handling and applyTemplate

data WithModule a = WithModule Module a deriving (Show)
           
class WithName a where
 name :: a -> String

instance WithName (WithModule TableTpl) where
 name (WithModule m t) = toSql $ (Parser.Module.moduleName m) <.> tabletplTemplate t

instance WithName (WithModule FunctionTpl) where
 name (WithModule m f) = toSql $ (Parser.Module.moduleName m) <.> functiontplTemplate f

instance WithName (WithModule TableColumnTpl) where
 name (WithModule m f) = toSql $ (Parser.Module.moduleName m) <.> tablecolumntplTemplate f

withoutModule (WithModule _ t) = t

--selectTemplates :: (WithName t) => Maybe [SqlName] -> [WithModule t] -> [t]
selectTemplates ns ts = 
  -- TODO: error handling here should be done using exceptions
  [ withoutModule $ selectUniqueReason ("table or function tpl " ++ n) $
    filter (\t -> n == name t) ts 
    | n <- map toSql $ maybeList ns ]
 
selectTemplate x ts = head' $ map withoutModule $ filter (\y -> name y == toSql x) ts
  where
    head' = selectUniqueReason ("Column template " ++ toSql x)
    
-- get things from Setup

setupAllModules :: Setup -> [Module]
setupAllModules = setupModuleData . setupInternal

setupAllFunctionTemplates :: Setup -> [WithModule FunctionTpl]
setupAllFunctionTemplates s = concat [
  maybeMap (WithModule m) (moduleFunctionTemplates m) | m <- setupAllModules s ]

setupAllTableTemplates    :: Setup -> [WithModule TableTpl]
setupAllTableTemplates s = concat [
  maybeMap (WithModule m) (moduleTableTemplates m) | m <- setupAllModules s ]

setupAllColumnTemplates   :: Setup -> [WithModule TableColumnTpl]
setupAllColumnTemplates s = concat [
  maybeMap (WithModule m) (moduleColumnTemplates m) | m <- setupAllModules s ]

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
    
