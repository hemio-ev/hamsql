-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.YamSql.Internal.Commons where

import Database.YamSql.Internal.Basic

data Variable = Variable {
    variableName        :: SqlName,
    variableDescription :: Maybe Text,
    variableType        :: SqlType,
    variableDefault     :: Maybe Text
} deriving (Generic,Show, Data, Typeable)
instance FromJSON Variable where parseJSON = strictParseYaml
instance ToJSON Variable where toJSON = genericToJSON myOpt

data Parameter = Parameter {
    parameterName        :: SqlName,
    parameterDescription :: Maybe Text,
    parameterType        :: SqlType
} deriving (Generic,Show, Data, Typeable)
instance FromJSON Parameter where parseJSON = strictParseYaml
instance ToJSON Parameter where toJSON = genericToJSON myOpt
