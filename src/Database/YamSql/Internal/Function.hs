-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.YamSql.Internal.Function where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Commons
import Utils

data Function = Function {
    -- function name
    functionName            :: SqlName,
    -- description what the function is good for
    functionDescription     :: Text,
    -- return type of the function, TABLE is special (see return_columns)
    functionReturns         :: SqlType,
    -- parameters the function takes
    functionParameters      :: Maybe [Variable],
    -- list of templates, used for this function
    functionTemplates       :: Maybe [SqlName],
    -- loaded templates, not designed for use via Yaml
    -- TODO: move to xfunctionInternal
    functionTemplateData    :: Maybe [FunctionTpl],
    -- if return is TABLE, gives the columns that are returned (see parameter)
    functionReturnsColumns  :: Maybe [Parameter],
    -- variables that are defined (ignored if language is given)
    functionVariables       :: Maybe [Variable],
    -- Role that has the privilege to execute the function
    functionPrivExecute     :: Maybe [SqlName],
    -- If true, the function is executed with the privileges of the owner!
    -- Owner has to be given, if this is true (not implemented yet!)
    functionSecurityDefiner :: Maybe Bool,
    -- owner of the function
    functionOwner           :: Maybe SqlName,
    -- language in which the body is written
    -- if not defined, pgsql is assumed an variables must be defined via variables
    -- if pgsql is given explicitly, variables are your problem...
    functionLanguage        :: Maybe Text,
    -- the code of the function (body)
    functionBody            :: Maybe Text
} deriving (Generic,Show, Data, Typeable)
instance FromJSON Function where parseJSON = strictParseYaml
instance ToJSON Function where toJSON = genericToJSON myOpt


data FunctionTpl = FunctionTpl {
    -- template name, used to refere the template via templates
    functiontplTemplate        :: SqlName,
    -- description what the template is good for
    functiontplDescription     :: Text,
    -- language of the function has to be the same as for used templates
    -- TODO: implement checks to avoid explosions here ;)
    functiontplLanguage        :: Maybe Text,
    -- parameters are joined with function definition parameters
    functiontplParameters      :: Maybe [Variable],
    -- variables are appended to the functions variables
    functiontplVariables       :: Maybe [Variable],
    -- defines priv_execute, can be overwritten by function definition
    functiontplPrivExecute     :: Maybe [SqlName],
    -- defines security_definer, can be overwritten by function definition
    functiontplSecurityDefiner :: Maybe Bool,
    -- defines owner, can be overwritten by function definition
    functiontplOwner           :: Maybe SqlName,
    -- code added before the body of the function
    functiontplBodyPrelude     :: Maybe Text,
    -- code added after the body of the function
    functiontplBodyPostlude    :: Maybe Text
} deriving (Generic,Show, Data, Typeable)
instance FromJSON FunctionTpl where parseJSON = strictParseYaml
instance ToJSON FunctionTpl where toJSON = genericToJSON myOpt

applyFunctionTpl :: FunctionTpl -> Function -> Function
applyFunctionTpl t f = f {
    functionPrivExecute =
      maybeRight (functiontplPrivExecute t) (functionPrivExecute f),

    functionSecurityDefiner =
      maybeRight (functiontplSecurityDefiner t) (functionSecurityDefiner f),

    functionOwner =
      maybeRight (functiontplOwner t) (functionOwner f),

    functionParameters =
      maybeJoin (functionParameters f) (functiontplParameters t),

    functionVariables =
      maybeJoin (functionVariables f) (functiontplVariables t),

    functionBody = Just $
      maybeStringL (functiontplBodyPrelude t) <>
      maybeText (functionBody f) <>
      maybeStringR (functiontplBodyPostlude t)

  }
  where
    maybeStringL (Just xs) = xs <> "\n"
    maybeStringL Nothing = ""
    maybeStringR (Just xs) = "\n" <> xs
    maybeStringR Nothing = ""

