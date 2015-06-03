{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Parser.Commons where

import Data.Aeson.Types
import Data.Typeable
import Data.Data
import GHC.Generics

import Parser.Basic

data Variable = Variable {
    variableName          :: SqlName,
    variableDescription   :: Maybe String,
    variableType          :: SqlType,
    variableDefault       :: Maybe String
} deriving (Generic,Show, Data, Typeable)
instance FromJSON Variable where parseJSON = strictParseYaml
instance ToJSON Variable where toJSON = genericToJSON myOpt

data Parameter = Parameter {
    parameterName          :: SqlName,
    parameterDescription   :: Maybe String,
    parameterType          :: SqlType
} deriving (Generic,Show, Data, Typeable)
instance FromJSON Parameter where parseJSON = strictParseYaml
instance ToJSON Parameter where toJSON = genericToJSON myOpt
