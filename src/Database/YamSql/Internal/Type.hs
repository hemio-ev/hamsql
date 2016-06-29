-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Database.YamSql.Internal.Type where

import Data.Data
import Data.Typeable
import GHC.Generics

import Database.YamSql.Internal.Basic
import Utils

data Type = Type {
    typeName        :: SqlName,
    typeDescription :: Text,
    typeElements    :: [TypeElement]
} deriving (Generic, Show, Data, Typeable)
instance FromJSON Type where parseJSON = strictParseYaml
instance ToJSON Type where toJSON = genericToJSON myOpt

data TypeElement = TypeElement {
    typeelementName :: SqlName,
    typeelementType :: SqlType
} deriving (Generic, Show, Data, Typeable)
instance FromJSON TypeElement where parseJSON = strictParseYaml
instance ToJSON TypeElement where toJSON = genericToJSON myOpt

