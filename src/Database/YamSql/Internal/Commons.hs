-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.YamSql.Internal.Commons where

import Database.YamSql.Internal.Basic

data Variable = Variable
  { variableName :: SqlName
  , variableDescription :: Maybe Text
  , variableType :: SqlType
  , variableDefault :: Maybe Text
  } deriving (Generic, Show, Data)

instance FromJSON Variable where
  parseJSON = parseYamSql

instance ToJSON Variable where
  toJSON = toYamSqlJson

data Parameter = Parameter
  { parameterName :: SqlName
  , parameterDescription :: Maybe Text
  , parameterType :: SqlType
  } deriving (Generic, Show, Data)

instance FromJSON Parameter where
  parseJSON = parseYamSql

instance ToJSON Parameter where
  toJSON = toYamSqlJson
