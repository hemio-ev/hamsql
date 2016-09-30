-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Database.YamSql.Internal.Check where

import Database.YamSql.Internal.Basic

data Check = Check
  { checkName        :: SqlName
  , checkDescription :: Text
  , checkCheck       :: Text
  } deriving (Generic, Show, Data)

instance FromJSON Check where
  parseJSON = parseYamSql

instance ToJSON Check where
  toJSON = toYamSqlJson
