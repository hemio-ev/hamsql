-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.YamSql.Internal.Commons where

import Data.Aeson.Types (Value(Object))

import Database.YamSql.Internal.Basic

data Variable =
  Variable
    { variableName :: SqlName
    , variableDescription :: Maybe Text
    , _variableType :: SqlType
    , variableDefault :: Maybe Text
    , variableMode :: Maybe Text
    }
  deriving (Generic, Show, Data)

instance FromJSON Variable where
  parseJSON = parseYamSql

instance ToJSON Variable where
  toJSON = toYamSqlJson

data Abbr a b
  = ShortForm a
  | LongForm b
  deriving (Data, Generic, Show)

instance (FromJSON a, FromJSON b) => FromJSON (Abbr a b) where
  parseJSON x@(Object _) = LongForm <$> parseJSON x
  parseJSON x = ShortForm <$> parseJSON x

makeLenses ''Variable
