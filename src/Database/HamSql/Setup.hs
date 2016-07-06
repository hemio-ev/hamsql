-- This file is part of HamSql
--
-- Copyright 2014 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.HamSql.Setup where

import Data.Maybe (fromJust, fromMaybe)

import Database.HamSql.Internal.Utils
import Database.YamSql

-- Setup --

data Setup = Setup {
  setupSchemas    :: [SqlName],
  setupSchemaDirs :: [FilePath],
  setupRolePrefix :: Maybe SqlName,
  setupPreCode    :: Maybe Text,
  setupPostCode   :: Maybe Text,
  xsetupInternal  :: Maybe SetupInternal
} deriving (Generic,Show,Data,Typeable)
instance FromJSON Setup where parseJSON = strictParseYaml
instance ToJSON Setup where toJSON = genericToJSON myOpt

data SetupInternal = SetupInternal {
  setupSchemaData :: [Schema]
} deriving (Generic,Show, Data, Typeable)
instance FromJSON SetupInternal where parseJSON = strictParseYaml
instance ToJSON SetupInternal where toJSON = genericToJSON myOpt

setupInternal :: Setup -> SetupInternal
setupInternal s = fromJust $ xsetupInternal s

setupRolePrefix' setup = fromMaybe (SqlName "yamsql_") (setupRolePrefix setup)

-- Template handling and applyTemplate

data WithSchema a = WithSchema Schema a deriving (Show)

class WithName a where
 name :: a -> Text

instance WithName (WithSchema TableTpl) where
 name (WithSchema m t) = toSql $ schemaName m <.> tabletplTemplate t

instance WithName (WithSchema FunctionTpl) where
 name (WithSchema m f) = toSql $ schemaName m <.> functiontplTemplate f

instance WithName (WithSchema TableColumnTpl) where
 name (WithSchema m f) = toSql $ schemaName m <.> tablecolumntplTemplate f

withoutSchema (WithSchema _ t) = t

--selectTemplates :: (WithName t) => Maybe [SqlName] -> [WithSchema t] -> [t]
selectTemplates ns ts =
  -- TODO: error handling here should be done using exceptions
  [ withoutSchema $ selectUniqueReason ("table or function tpl " <> n) $
    filter (\t -> n == name t) ts
    | n <- map toSql $ maybeList ns ]

selectTemplate x ts = head' $ map withoutSchema $ filter (\y -> name y == toSql x) ts
  where
    head' = selectUniqueReason ("Column template " <> toSql x)

-- get things from Setup

setupAllSchemas :: Setup -> [Schema]
setupAllSchemas = setupSchemaData . setupInternal

setupAllFunctionTemplates :: Setup -> [WithSchema FunctionTpl]
setupAllFunctionTemplates s = concat [
  maybeMap (WithSchema m) (schemaFunctionTemplates m) | m <- setupAllSchemas s ]

setupAllTableTemplates    :: Setup -> [WithSchema TableTpl]
setupAllTableTemplates s = concat [
  maybeMap (WithSchema m) (schemaTableTemplates m) | m <- setupAllSchemas s ]

setupAllColumnTemplates   :: Setup -> [WithSchema TableColumnTpl]
setupAllColumnTemplates s = concat [
  maybeMap (WithSchema m) (schemaColumnTemplates m) | m <- setupAllSchemas s ]

applyTpl :: Setup -> Setup
applyTpl s = s {
    -- TODO: possible overwrite here!
    xsetupInternal = Just SetupInternal {
      setupSchemaData =
        map applySchema (setupSchemaData $ setupInternal s)
    }
  }

  where

    applySchema m = m {
        schemaTables =  Just $
          map applyColumnTemplates
          $ maybeMap applyTableTemplates (schemaTables m),

        schemaFunctions = Just $
          maybeMap applyFunctionTemplates (schemaFunctions m)
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
       f x@ColumnTpl{} = applyColumnTpl (columnTpl x) x
       f x = x

    columnTpl :: Column -> TableColumnTpl
    columnTpl c@ColumnTpl{} = selectTemplate (columntplTemplate c) (setupAllColumnTemplates s)
    columnTpl _ = undefined

