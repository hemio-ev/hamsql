-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.YamSql.Internal.Role where

import Database.YamSql.Internal.Basic
import Utils

data Role = Role {
    roleName        :: SqlName,
    roleDescription :: Text,
    roleLogin       :: Maybe Bool,
    rolePassword    :: Maybe Text,
    roleMemberIn    :: Maybe [SqlName]
} deriving (Generic, Show, Data, Typeable)
instance FromJSON Role where parseJSON = strictParseYaml
instance ToJSON Role where toJSON = genericToJSON myOpt

