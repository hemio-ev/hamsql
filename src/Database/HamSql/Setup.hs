-- This file is part of HamSql
--
-- Copyright 2014 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Database.HamSql.Setup where

import Data.Maybe (fromJust, fromMaybe)
import Data.Typeable

import Database.HamSql.Internal.Utils
import Database.YamSql
import Database.HamSql.Internal.Stmt

data SetupContext = SetupContext
  { setupContextSetup :: Setup
  }

data SetupElement where SetupElement :: (ToSqlStmts a, Show b) => { setupElement :: a
    , setupElementSource :: Maybe b
    } -> SetupElement

instance ToSqlStmts SetupElement where
    toSqlStmts x SetupElement{setupElement=y} = toSqlStmts x y

class (Typeable a) => ToSqlStmts a where
    toSqlStmts :: SetupContext -> a -> [SqlStmt]



-- Setup --
data Setup = Setup
  { setupSchemas :: [SqlName]
  , setupSchemaDirs :: [FilePath]
  , setupRolePrefix :: Maybe SqlName
  , setupPreCode :: Maybe Text
  , setupPostCode :: Maybe Text
  , setupSchemaData :: Maybe [Schema]
  } deriving (Generic, Show, Data, Typeable)

instance FromJSON Setup where
  parseJSON = strictParseYaml

instance ToJSON Setup where
  toJSON = genericToJSON myOpt

setupRolePrefix' :: Setup -> SqlName
setupRolePrefix' setup = fromMaybe (SqlName "yamsql_") (setupRolePrefix setup)

-- Template handling and applyTemplate
data WithSchema a =
  WithSchema Schema
             a
  deriving (Show)

class WithName a  where
  name :: a -> Text

instance WithName (WithSchema TableTpl) where
  name (WithSchema m t) = toSqlCode $ schemaName m <.> tabletplTemplate t

instance WithName (WithSchema FunctionTpl) where
  name (WithSchema m f) = toSqlCode $ schemaName m <.> functiontplTemplate f

instance WithName (WithSchema TableColumnTpl) where
  name (WithSchema m f) = toSqlCode $ schemaName m <.> tablecolumntplTemplate f

withoutSchema :: WithSchema a -> a
withoutSchema (WithSchema _ t) = t

--selectTemplates :: (WithName t) => Maybe [SqlName] -> [WithSchema t] -> [t]
selectTemplates ns ts
                   -- TODO: error handling here should be done using exceptions
 =
  [ withoutSchema $
   selectUniqueReason ("table or function tpl " <> n) $
   filter (\t -> n == name t) ts
  | n <- map toSqlCode $ maybeList ns ]

selectTemplate x ts =
  head' $ map withoutSchema $ filter (\y -> name y == toSqlCode x) ts
  where
    head' = selectUniqueReason ("Column template " <> toSqlCode x)

-- get things from Setup
setupAllSchemas :: Setup -> [Schema]
setupAllSchemas = maybeList . setupSchemaData

setupAllFunctionTemplates :: Setup -> [WithSchema FunctionTpl]
setupAllFunctionTemplates s =
  concat
    [ maybeMap (WithSchema m) (schemaFunctionTemplates m)
    | m <- setupAllSchemas s ]

setupAllTableTemplates :: Setup -> [WithSchema TableTpl]
setupAllTableTemplates s =
  concat
    [ maybeMap (WithSchema m) (schemaTableTemplates m)
    | m <- setupAllSchemas s ]

setupAllColumnTemplates :: Setup -> [WithSchema TableColumnTpl]
setupAllColumnTemplates s =
  concat
    [ maybeMap (WithSchema m) (schemaColumnTemplates m)
    | m <- setupAllSchemas s ]

applyTpl :: Setup -> Setup
applyTpl s =
  s
  -- TODO: possible overwrite here!
  { setupSchemaData = Just $ maybeMap applySchema (setupSchemaData s)
  }
  where
    applySchema m =
      m
      { schemaTables =
        Just $
        map applyColumnTemplates $ maybeMap applyTableTemplates (schemaTables m)
      , schemaFunctions =
        Just $ maybeMap applyFunctionTemplates (schemaFunctions m)
      }
    applyTableTemplates :: Table -> Table
    applyTableTemplates t = foldr applyTableTpl t (tableTpls t)
    tableTpls :: Table -> [TableTpl]
    tableTpls t = selectTemplates (tableTemplates t) (setupAllTableTemplates s)
    applyFunctionTemplates :: Function -> Function
    applyFunctionTemplates f = foldr applyFunctionTpl f (functionTpls f)
    functionTpls :: Function -> [FunctionTpl]
    functionTpls f =
      selectTemplates (functionTemplates f) (setupAllFunctionTemplates s)
    applyColumnTemplates :: Table -> Table
    applyColumnTemplates t =
      t
      { tableColumns = map f (tableColumns t)
      }
      where
        f x@ColumnTpl {} = applyColumnTpl (columnTpl x) x
        f x = x
    columnTpl :: Column -> TableColumnTpl
    columnTpl c@ColumnTpl {} =
      selectTemplate (columntplTemplate c) (setupAllColumnTemplates s)
    columnTpl _ = undefined
