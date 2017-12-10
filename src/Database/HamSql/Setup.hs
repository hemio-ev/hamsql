-- This file is part of HamSql
--
-- Copyright 2014 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Database.HamSql.Setup where

import Data.Typeable
import Data.Yaml

import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Utils
import Database.YamSql
import Database.YamSql.Parser

data SetupContext = SetupContext
  { setupContextSetup :: Setup
  }

data SetupElement where
  SetupElement :: (ToSqlStmts a) => a -> SetupElement

instance ToSqlStmts SetupElement where
  toSqlStmts x (SetupElement y) = toSqlStmts x y

class Typeable a =>
      ToSqlStmts a
  where
  toSqlStmts :: SetupContext -> a -> [Maybe SqlStmt]

-- | Setup
data Setup = Setup
  { setupSchemas :: [SqlName]
  , setupSchemaDirs :: Maybe [FilePath]
  , setupRolePrefix :: Maybe Text
  , setupPreCode :: Maybe Text
  , setupPostCode :: Maybe Text
  , _setupSchemaData :: Maybe [Schema]
  , setupRoles :: Maybe [Role]
  } deriving (Generic, Show, Data)

makeLenses ''Setup

instance FromJSON Setup where
  parseJSON = parseYamSql

instance ToJSON Setup where
  toJSON = toYamSqlJson

setupRolePrefix' :: Setup -> Text
setupRolePrefix' setup = fromMaybe "yamsql_" (setupRolePrefix setup)

-- | Template handling and applyTemplate
data WithSchema a =
  WithSchema Schema
             a
  deriving (Show)

class WithName a where
  name :: a -> Text

instance WithName (WithSchema TableTpl) where
  name (WithSchema m t) = toSqlCode $ schemaName m <.> tabletplTemplate t

instance WithName (WithSchema FunctionTpl) where
  name (WithSchema m f) = toSqlCode $ schemaName m <.> functiontplTemplate f

withoutSchema :: WithSchema a -> a
withoutSchema (WithSchema _ t) = t

selectTemplates ::
     (ToSqlCode a, WithName (WithSchema t))
  => Maybe [a]
  -> [WithSchema t]
  -> [t]
selectTemplates ns ts
                   -- TODO: error handling here should be done using exceptions
 =
  [ withoutSchema $
  selectUniqueReason ("table or function tpl " <> n) $
  filter (\t -> n == name t) ts
  | n <- maybeMap toSqlCode ns
  ]

selectTemplate ::
     (ToSqlCode a1, WithName (WithSchema a)) => a1 -> [WithSchema a] -> a
selectTemplate x ts =
  head' $ map withoutSchema $ filter (\y -> name y == toSqlCode x) ts
  where
    head' = selectUniqueReason ("Column template " <> toSqlCode x)

-- get things from Setup
setupAllSchemas :: Setup -> [Schema]
setupAllSchemas = fromMaybe [] . _setupSchemaData

setupAllFunctionTemplates :: Setup -> [WithSchema FunctionTpl]
setupAllFunctionTemplates s =
  concat
    [ maybeMap (WithSchema m) (schemaFunctionTemplates m)
    | m <- setupAllSchemas s
    ]

setupAllTableTemplates :: Setup -> [WithSchema TableTpl]
setupAllTableTemplates s =
  concat
    [maybeMap (WithSchema m) (schemaTableTemplates m) | m <- setupAllSchemas s]

applyTpl :: Setup -> Setup
applyTpl s =
  s
  -- TODO: possible overwrite here!
  {_setupSchemaData = map applySchema <$> _setupSchemaData s}
  where
    applySchema m =
      m
      { _schemaTables = map applyTableTemplates <$> _schemaTables m
      , _schemaFunctions = map applyFunctionTemplates <$> _schemaFunctions m
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
