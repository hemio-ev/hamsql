-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.YamSql.Internal.Trigger where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Commons

data Trigger = Trigger
  { triggerName :: SqlName
  , triggerDescription :: Text
  , triggerTables :: [SqlName]
  , triggerMoment :: Text
  , triggerEvents :: [Text]
  , triggerForEach :: Text
  , triggerCondition :: Maybe Text
  , triggerLanguage :: Maybe Text
  , triggerVariables :: Maybe [Variable]
  , triggerBody :: Maybe Text
  } deriving (Generic, Show, Data)

instance FromJSON Trigger where
  parseJSON = strictParseYaml

instance ToJSON Trigger where
  toJSON = genericToJSON myOpt

instance ToSqlIdPart Trigger where
  sqlIdPart = triggerName
  sqlIdPartType _ = "TRIGGER"
