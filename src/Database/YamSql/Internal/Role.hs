-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Database.YamSql.Internal.Role where

import Database.YamSql.Internal.Basic

data Role = Role
  { roleName        :: SqlName
  , roleDescription :: Text
  , roleLogin       :: Maybe Bool
  , rolePassword    :: Maybe Text
  , roleMemberIn    :: Maybe [SqlName]
  } deriving (Generic, Show, Data)

instance FromJSON Role where
  parseJSON = parseYamSql

instance ToJSON Role where
  toJSON = toYamSqlJson

data SQL_ROLE =
  SQL_ROLE
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_ROLE where
  toSqlCode = const "ROLE"

instance ToSqlId (SqlContext Role) where
  sqlId (SqlContext x) = SqlId $ SqlObj SQL_ROLE (roleName x)
