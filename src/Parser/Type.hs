{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Parser.Type where

import Data.Aeson.Types
import GHC.Generics
import Data.Data
import Data.Typeable

import Parser.Basic
import Utils

data Type = Type {
    typeName :: SqlName,
    typeDescription :: String,
    typeElements :: [TypeElement]
} deriving (Generic, Show, Data, Typeable)
instance FromJSON Type where parseJSON = strictParseYaml
instance ToJSON Type where toJSON = genericToJSON myOpt
      
data TypeElement = TypeElement {
    typeelementName :: SqlName,
    typeelementType :: SqlType
} deriving (Generic, Show, Data, Typeable)
instance FromJSON TypeElement where parseJSON = strictParseYaml
instance ToJSON TypeElement where toJSON = genericToJSON myOpt

