-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.YamSql.Internal.Type where

import Database.YamSql.Internal.Basic

data Type = Type
  { typeName :: SqlName
  , typeDescription :: Text
  , typeElements :: [TypeElement]
  } deriving (Generic, Show, Data)

instance FromJSON Type where
  parseJSON = parseYamSql

instance ToJSON Type where
  toJSON = toYamSqlJson

data TypeElement = TypeElement
  { typeelementName :: SqlName
  , typeelementType :: SqlType
  } deriving (Generic, Show, Data)

instance FromJSON TypeElement where
  parseJSON = parseYamSql

instance ToJSON TypeElement where
  toJSON = toYamSqlJson

instance ToSqlIdPart Type where
  sqlIdPart = typeName
  sqlIdPartType _ = "TYPE"
