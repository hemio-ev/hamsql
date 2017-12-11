-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.YamSql.Internal.Obj.Function where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Commons

data Function = Function
                -- function name
  { functionName :: SqlName
    -- | description what the function is good for
  , functionDescription :: Text
    -- | return type of the function, TABLE is special (see return_columns)
  , _functionReturns :: ReturnType
    -- | parameters the function takes
  , _functionParameters :: Maybe [Variable]
    -- | list of templates, used for this function
  , functionTemplates :: Maybe [SqlName]
    -- | loaded templates, not designed for use via Yaml
    -- __TODO: move to xfunctionInternal__
  , functionTemplateData :: Maybe [FunctionTpl]
    -- | variables that are defined (ignored if language is given)
  , functionVariables :: Maybe [Variable]
    -- | Role that has the privilege to execute the function
  , functionPrivExecute :: Maybe [SqlName]
    -- | If true, the function is executed with the privileges of the owner!
    -- | Owner has to be given, if this is true (not implemented yet!)
  , functionSecurityDefiner :: Maybe Bool
    -- | owner of the function
  , functionOwner :: Maybe SqlName
    -- | language in which the body is written
    -- if not defined, pgsql is assumed an variables must be defined via variables
    -- if pgsql is given explicitly, variables are your problem...
  , functionLanguage :: Maybe Text
    -- | the code of the function (body)
  , functionBody :: Maybe Text
  } deriving (Generic, Show, Data)

instance FromJSON Function where
  parseJSON = parseYamSql

instance ToJSON Function where
  toJSON = toYamSqlJson

data Parameter = Parameter
  { parameterName :: SqlName
  , parameterDescription :: Maybe Text
  , _parameterType :: SqlType
  } deriving (Generic, Show, Data)

instance FromJSON Parameter where
  parseJSON = parseYamSql

instance ToJSON Parameter where
  toJSON = toYamSqlJson

data ReturnType
  = ReturnType SqlType
  | ReturnTypeTable { _returntypeTable :: [Parameter] }
  deriving (Generic, Show, Data)

instance FromJSON ReturnType where
  parseJSON = parseYamSql

instance ToJSON ReturnType where
  toJSON = toYamSqlJson

data SQL_FUNCTION =
  SQL_FUNCTION
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_FUNCTION where
  toSqlCode = const "FUNCTION"

data FunctionTpl = FunctionTpl
                   -- template name, used to refere the template via templates
  { functiontplTemplate :: SqlName
    -- description what the template is good for
  , functiontplDescription :: Text
    -- language of the function has to be the same as for used templates
    -- TODO: implement checks to avoid explosions here ;)
  , functiontplLanguage :: Maybe Text
    -- parameters are joined with function definition parameters
  , functiontplParameters :: Maybe [Variable]
    -- variables are appended to the functions variables
  , functiontplVariables :: Maybe [Variable]
    -- defines priv_execute, can be overwritten by function definition
  , functiontplPrivExecute :: Maybe [SqlName]
    -- defines security_definer, can be overwritten by function definition
  , functiontplSecurityDefiner :: Maybe Bool
    -- defines owner, can be overwritten by function definition
  , functiontplOwner :: Maybe SqlName
    -- code added before the body of the function
  , functiontplBodyPrelude :: Maybe Text
    -- code added after the body of the function
  , functiontplBodyPostlude :: Maybe Text
  } deriving (Generic, Show, Data)

instance FromJSON FunctionTpl where
  parseJSON = parseYamSql

instance ToJSON FunctionTpl where
  toJSON = toYamSqlJson

applyFunctionTpl :: FunctionTpl -> Function -> Function
applyFunctionTpl t f =
  f
  { functionPrivExecute = asum [functionPrivExecute f, functiontplPrivExecute t]
  , functionSecurityDefiner =
      asum [functionSecurityDefiner f, functiontplSecurityDefiner t]
  , functionOwner = asum [functionOwner f, functiontplOwner t]
  , _functionParameters = _functionParameters f <> functiontplParameters t
  , functionVariables = functionVariables f <> functiontplVariables t
  , functionBody =
      Just $
      maybeStringL (functiontplBodyPrelude t) <> fromMaybe "" (functionBody f) <>
      maybeStringR (functiontplBodyPostlude t)
  }
  where
    maybeStringL (Just xs) = xs <> "\n"
    maybeStringL Nothing = ""
    maybeStringR (Just xs) = "\n" <> xs
    maybeStringR Nothing = ""

makeLenses ''Function

makePrisms ''ReturnType

makeLenses ''ReturnType

makeLenses ''Parameter
