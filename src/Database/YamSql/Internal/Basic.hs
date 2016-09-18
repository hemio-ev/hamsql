-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.YamSql.Internal.Basic
  ( module Database.HamSql.Internal.Utils
  , module Database.YamSql.Internal.Basic
  , genericParseJSON
  , genericToJSON
  , ToJSON(..)
  , FromJSON(..)
  , Generic(..)
  , Data(..)
  
  ) where

import Control.Exception
import Data.Aeson.Types
       (GFromJSON(..), Options(..), defaultOptions, genericParseJSON,
        genericToJSON, Zero)

import Data.Char
import Data.Data
import Data.HashMap.Strict (keys)
import Data.List
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import System.IO

import Database.HamSql.Internal.Utils

-- | Idable
class Show a =>
      ToSqlId a  where
  sqlId :: a -> SqlId
  sqlIdCode :: a -> Text
  sqlIdCode = toSqlCode . sqlId

class (Typeable a, ToSqlCode a, Eq a, Show a) =>
      SqlIdContent a  where
  sqlIdContentType :: a -> SqlContextObjType

class Show a =>
      ToSqlIdPart a  where
  sqlIdPart :: a -> SqlName
  sqlIdPartType :: a -> SqlContextObjType

class ToSqlSqoId a  where
  sqlSqoId :: a -> SqlName
  sqlSqoIdCode :: a -> Text
  sqlSqoIdCode = toSqlCode . sqlSqoId

class ToSqlSqoObjId a  where
  sqlSqoObjId :: a -> SqlName
  sqlSqoObjIdCode :: a -> Text
  sqlSqoObjIdCode = toSqlCode . sqlSqoObjId

-- | Strings like "TABLE"
type SqlContextObjType = String

class ToSqlIdPartArgs a  where
  sqlIdPartArgs :: a -> [SqlType]

-- | SqlId
data SqlId where
        SqlId :: (SqlIdContent a) => { sqlIdContent :: a } -> SqlId

deriving instance Show SqlId

sqlIdType :: SqlId -> SqlContextObjType
sqlIdType (SqlId x) = sqlIdContentType x

instance Eq SqlId where
  SqlId x == SqlId y = Just x == cast y

instance Ord SqlId where
  SqlId x `compare` SqlId y =
    case sqlIdContentType x `compare` sqlIdContentType y of
      EQ -> toSqlCode x `compare` toSqlCode y
      x' -> x'

instance ToSqlId SqlId where
  sqlId = id

instance ToSqlCode SqlId where
  toSqlCode (SqlId x) = toSqlCode x

-- | ROLE, DATABASE, SCHEMA
data SqlIdContentObj =
  SqlIdContentObj SqlContextObjType
                  SqlName
  deriving (Eq, Show)

instance SqlIdContent SqlIdContentObj where
  sqlIdContentType (SqlIdContentObj x _) = x

instance ToSqlId SqlIdContentObj where
  sqlId = SqlId

instance ToSqlCode SqlIdContentObj where
  toSqlCode (SqlIdContentObj _ x) = toSqlCode x

-- | TABLE
data SqlIdContentSqo =
  SqlIdContentSqo SqlContextObjType
                  SqlName
  deriving (Eq, Show)

instance SqlIdContent SqlIdContentSqo where
  sqlIdContentType (SqlIdContentSqo x _) = x

instance ToSqlId SqlIdContentSqo where
  sqlId = SqlId

instance ToSqlCode SqlIdContentSqo where
  toSqlCode (SqlIdContentSqo _ x) = toSqlCode x

instance ToSqlSqoId SqlIdContentSqo where
  sqlSqoId (SqlIdContentSqo _ x) = x

-- | TABLE TRIGGER, TABLE CONTRAINT
data SqlIdContentSqoObj =
  SqlIdContentSqoObj SqlContextObjType
                     SqlName
                     SqlName
  deriving (Eq, Show)

instance SqlIdContent SqlIdContentSqoObj where
  sqlIdContentType (SqlIdContentSqoObj x _ _) = x

instance ToSqlId SqlIdContentSqoObj where
  sqlId = SqlId

instance ToSqlCode SqlIdContentSqoObj where
  toSqlCode (SqlIdContentSqoObj _ x y) = toSqlCode (x <.> y)

instance ToSqlSqoId SqlIdContentSqoObj where
  sqlSqoId (SqlIdContentSqoObj _ x _) = x

instance ToSqlSqoObjId SqlIdContentSqoObj where
  sqlSqoObjId (SqlIdContentSqoObj _ _ x) = x

-- | FUNCTION
data SqlIdContentSqoArgtypes =
  SqlIdContentSqoArgtypes SqlContextObjType
                          SqlName
                          [SqlType]
  deriving (Eq, Show)

instance SqlIdContent SqlIdContentSqoArgtypes where
  sqlIdContentType (SqlIdContentSqoArgtypes x _ _) = x

instance ToSqlId SqlIdContentSqoArgtypes where
  sqlId = SqlId

instance ToSqlSqoId SqlIdContentSqoArgtypes where
  sqlSqoId (SqlIdContentSqoArgtypes _ x _) = x

instance ToSqlCode SqlIdContentSqoArgtypes where
  toSqlCode (SqlIdContentSqoArgtypes _ x ys) =
    toSqlCode x <> "(" <> T.intercalate ", " (map toSqlCode ys) <> ")"

-- removes first part of camel case. e.g.:
-- columnDescriptionField |-> descriptionField
removeFirstPart :: String -> String
removeFirstPart xs = lowerStr rest
  where
    rest = dropWhile isLower xs
    lowerStr (x':xs') = toLower x' : xs'
    lowerStr [] = "__"

-- makes camelCaseSpelling to camel_case_spelling
snakeify :: String -> String
snakeify [] = []
snakeify (x:xs)
  | isUpper x = '_' : toLower x : snakeify xs
  | otherwise = x : snakeify xs

myOpt :: Options
myOpt =
  defaultOptions
  { fieldLabelModifier = snakeify . removeFirstPart
  , constructorTagModifier = drop 1 . snakeify
  }

outJson
  :: ToJSON a
  => a -> String
outJson s = show $ toJSON s

forceToJson
  :: ToJSON a
  => a -> IO ()
forceToJson s =
  withFile "/dev/null" WriteMode (\handl -> hPrint handl (toJSON s))

-- ToSqlCode (right now only SqlName)
unsafePlainName :: SqlName -> Text
unsafePlainName (SqlName n) = n

instance Eq SqlName where
  (==) x y = toSqlCode x == toSqlCode y

instance ToSqlCode SqlName where
  toSqlCode (SqlName n) =
    if '"' `isIn` n
      then n
      else toSqlCode' $ expSqlName $ SqlName n

instance SqlIdentifierConcat SqlName where
  (//) (SqlName s) (SqlName t) = SqlName (s <> t)

(<.>) :: SqlName -> SqlName -> SqlName
(<.>) (SqlName s) (SqlName t) = SqlName $ s <> "." <> t

expSqlName :: SqlName -> [SqlName]
expSqlName n = map SqlName (T.splitOn "." (getStr n))
  where
    getStr (SqlName n') = n'

instance ToSqlCode SqlType where
  toSqlCode (SqlType n)
            -- if quotes are contained
            -- assume that user cares for correct enquoting
   =
    if '"' `isIn` n ||
       -- if at least a pair of brakets is found
       -- assume that a type like varchar(20) is meant
       ('(' `isIn` n && ')' `isIn` n) ||
       -- if no dot is present, assume that buildin type
       -- like integer is meant
       not ('.' `isIn` n) ||
       -- if % is present, assume that something like
       -- table%ROWTYPE could be meant
       '%' `isIn` n
      then n
      else toSqlCode' $ expSqlName $ SqlName n

instance SqlIdentifierConcat SqlType where
  (//) (SqlType s) (SqlType t) = SqlType (s <> t)

contSqlName :: [SqlName] -> SqlName
contSqlName ns = SqlName $ T.intercalate "." $ map getStr ns
  where
    getStr (SqlName n') = n'

toSqlCode' :: [SqlName] -> Text
toSqlCode' xs = T.intercalate "." $ map quotedName xs
  where
    quotedName (SqlName s) = "\"" <> s <> "\""

class ToSqlCode a  where
  toSqlCode :: a -> Text

class SqlIdentifierConcat a  where
  (//) :: a -> a -> a

-- SqlName
newtype SqlName =
  SqlName Text
  deriving (Generic, Ord, Show, Data)

instance FromJSON SqlName where
  parseJSON = genericParseJSON myOpt

instance ToJSON SqlName where
  toJSON = genericToJSON myOpt

newtype SqlType =
  SqlType Text
  deriving (Generic, Show, Eq, Data)

instance FromJSON SqlType where
  parseJSON = genericParseJSON myOpt

instance ToJSON SqlType where
  toJSON = genericToJSON myOpt

strictParseYaml
  :: (Generic r, GFromJSON Zero (Rep r), Data r)
  => Value -> Parser r
strictParseYaml xs = do
  parsed <- genericParseJSON myOpt xs
  let diff = keysOfValue xs \\ keysOfData parsed
  return $
    if null diff
      then parsed
      else throw $ YamsqlException $ "Found unknown keys: " <> tshow diff
  where
    keysOfData u =
      "tag" : map (snakeify . removeFirstPart) (constrFields (toConstr u))
    keysOfValue :: Value -> [String]
    keysOfValue (Object ys) = map T.unpack $ keys ys
    keysOfValue _ = err "HAMSQL-UNEXPECTED 3"

-- EXCEPTIONS
data YamsqlException =
  YamsqlException Text
  deriving (Show)

instance Exception YamsqlException
