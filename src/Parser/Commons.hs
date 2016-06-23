-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Parser.Commons where

import Data.Aeson.Types
import Data.Data
import Data.Typeable
import GHC.Generics

import Parser.Basic
import Utils

data Variable = Variable {
    variableName        :: SqlName,
    variableDescription :: Maybe Text,
    variableType        :: SqlType,
    variableDefault     :: Maybe Text
} deriving (Generic,Show, Data, Typeable)
instance FromJSON Variable where parseJSON = strictParseYaml
instance ToJSON Variable where toJSON = genericToJSON myOpt

data Parameter = Parameter {
    parameterName        :: SqlName,
    parameterDescription :: Maybe Text,
    parameterType        :: SqlType
} deriving (Generic,Show, Data, Typeable)
instance FromJSON Parameter where parseJSON = strictParseYaml
instance ToJSON Parameter where toJSON = genericToJSON myOpt
