-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Parser.Check where

import Data.Aeson.Types
import Data.Data
import Data.Typeable
import GHC.Generics

import Parser.Basic
import Utils

data Check = Check {
    checkName        :: SqlName,
    checkDescription :: Text,
    checkCheck       :: Text
} deriving (Generic, Show, Typeable, Data)
instance FromJSON Check where parseJSON = strictParseYaml
instance ToJSON Check where toJSON = genericToJSON myOpt

