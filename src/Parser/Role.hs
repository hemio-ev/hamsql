{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Parser.Role where

import Data.Aeson.Types
import GHC.Generics
import Data.Data
import Data.Typeable

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
