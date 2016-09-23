-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.YamSql.Internal.Role where

import Database.YamSql.Internal.Basic

data Role = Role
  { roleName :: SqlName
  , roleDescription :: Text
  , roleLogin :: Maybe Bool
  , rolePassword :: Maybe Text
  , roleMemberIn :: Maybe [SqlName]
  } deriving (Generic, Show, Data)

instance FromJSON Role where
  parseJSON = parseYamSql

instance ToJSON Role where
  toJSON = toYamSqlJson

instance ToSqlIdPart Role where
  sqlIdPart = roleName
  sqlIdPartType = const "SEQUENCE"
