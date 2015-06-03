{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Parser.Check where

import Data.Typeable
import Data.Aeson.Types
import Data.Data
import GHC.Generics

import Parser.Basic
      
data Check = Check {
    checkName        :: SqlName,
    checkDescription :: String,
    checkCheck       :: String
} deriving (Generic, Show, Typeable, Data)
instance FromJSON Check where parseJSON = strictParseYaml
instance ToJSON Check where toJSON = genericToJSON myOpt

