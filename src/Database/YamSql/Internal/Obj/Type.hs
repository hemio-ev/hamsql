-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.YamSql.Internal.Obj.Type where

import Database.YamSql.Internal.Basic

data Type = Type
  { typeName :: SqlName
  , typeDescription :: Text
  , _typeElements :: [TypeElement]
  } deriving (Generic, Show, Data)

instance FromJSON Type where
  parseJSON = parseYamSql

instance ToJSON Type where
  toJSON = toYamSqlJson

data TypeElement = TypeElement
  { typeelementName :: SqlName
  , _typeelementType :: SqlType
  } deriving (Generic, Show, Data)

instance FromJSON TypeElement where
  parseJSON = parseYamSql

instance ToJSON TypeElement where
  toJSON = toYamSqlJson

data SQL_TYPE =
  SQL_TYPE
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_TYPE where
  toSqlCode = const "TYPE"

makeLenses ''Type

makeLenses ''TypeElement
