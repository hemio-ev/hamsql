{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Parser.Sequence where

import Data.Aeson.Types
import GHC.Generics
import Data.Data
import Data.Typeable

import Parser.Basic
import Utils

data Sequence = Sequence {
    sequenceName :: SqlName,
    sequenceIncrement :: Maybe Int,
    sequenceMinValue :: Maybe Int,
    sequenceMaxValue :: Maybe Int,
    sequenceStartValue :: Maybe Int,
    -- sequenceCache :: Maybe Int,
    sequenceCycle :: Maybe Bool,
    -- PostgreSQL extension
    sequenceOwnedByColumn :: Maybe SqlName
} deriving (Generic, Show, Data, Typeable)
instance FromJSON Sequence where parseJSON = strictParseYaml
instance ToJSON Sequence where toJSON = genericToJSON myOpt
      

