{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Parser.Trigger where

import Data.Aeson.Types
import Data.Data
import Data.Typeable
import GHC.Generics

import Parser.Basic

data Role = Role {
    roleName        :: SqlName,
    roleDescription :: String,
    roleLogin       :: Maybe Bool,
    rolePassword    :: Maybe String,
    roleMemberIn    :: Maybe [SqlName]
} deriving (Generic, Show, Data, Typeable)
instance FromJSON Role where parseJSON = strictParseYaml
instance ToJSON Role where toJSON = genericToJSON myOpt

