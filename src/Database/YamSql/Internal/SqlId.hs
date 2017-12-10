-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.YamSql.Internal.SqlId where

import Data.Semigroup (Semigroup)
import qualified Data.Text as T
import Data.Typeable

import Database.YamSql.Internal.Utils
import Database.YamSql.Parser

-- | Idable
class Show a =>
      ToSqlId a
  where
  sqlId :: a -> SqlId
  sqlIdCode :: a -> Text
  sqlIdCode = toSqlCode . sqlId

class (Typeable a, ToSqlCode a, Eq a, Show a) =>
      SqlIdContent a


-- | SqlId
data SqlId where
  SqlId :: (SqlObjType a, SqlIdContent b) => SqlObj a b -> SqlId

sqlIdShowType :: SqlId -> Text
sqlIdShowType (SqlId x) = tshow $ sqlObjType x

sqlIdTypeCode :: SqlId -> Text
sqlIdTypeCode (SqlId x) = toSqlCode $ sqlObjType x

deriving instance Show SqlId

instance Eq SqlId where
  SqlId x == SqlId y = Just x == cast y

instance Ord SqlId where
  (SqlId x) `compare` (SqlId y) =
    case toSqlCode (sqlObjType x) `compare` toSqlCode (sqlObjType y) of
      EQ -> toSqlCode x `compare` toSqlCode y
      x' -> x'

instance ToSqlId SqlId where
  sqlId = id

instance ToSqlCode SqlId where
  toSqlCode (SqlId x) = toSqlCode $ sqlObjId x

data SqlContext a =
  SqlContext a

-- FIXME
instance Show (SqlContext a) where
  show = const ""

instance (SqlObjType a, SqlIdContent b) => ToSqlId (SqlObj a b) where
  sqlId = SqlId

class (Typeable a, ToSqlCode a, Show a) =>
      SqlObjType a


data SqlObj a b where
  SqlObj :: (SqlObjType a, SqlIdContent b) => a -> b -> SqlObj a b

sqlObjType :: SqlObj a b -> a
sqlObjType (SqlObj x _) = x

sqlObjId :: SqlObj a b -> b
sqlObjId (SqlObj _ y) = y

deriving instance Show (SqlObj a b)

instance Eq (SqlObj a b) where
  SqlObj x1 y1 == SqlObj x2 y2 = typeOf x1 == typeOf x2 && y1 == y2

instance ToSqlCode (SqlObj a b) where
  toSqlCode (SqlObj _ x) = toSqlCode x

instance SqlIdContent SqlName

instance SqlIdContent (SqlName, SqlName)

instance ToSqlCode (SqlName, SqlName) where
  toSqlCode (x, y) = toSqlCode (x <.> y)

instance SqlIdContent (SqlName, [SqlType])

instance ToSqlCode (SqlName, [SqlType]) where
  toSqlCode (x, ys) =
    toSqlCode x <> "(" <> T.intercalate ", " (map toSqlCode ys) <> ")"

instance SqlIdContent (SqlName, SqlName, SqlName)

instance ToSqlCode (SqlName, SqlName, SqlName) where
  toSqlCode (x, _, y) = toSqlCode (x <.> y)

-- ToSqlCode (right now only SqlName)
unsafePlainName :: SqlName -> Text
unsafePlainName (SqlName n) = n

instance Eq SqlName where
  (==) x y = toSqlCode x == toSqlCode y

instance ToSqlCode SqlName where
  toSqlCode n'@(SqlName n) =
    if '"' `isIn` n
      then n
      else toSqlCode' $ expSqlName n'

instance SqlIdentifierConcat SqlName where
  (//) (SqlName s) (SqlName t) = SqlName (s <> T.replace "\"" "" t)

(<.>) :: SqlName -> SqlName -> SqlName
(<.>) (SqlName s) (SqlName t) = SqlName $ s <> "." <> t

expSqlName :: SqlName -> [SqlName]
expSqlName n = map SqlName (T.splitOn "." (getStr n))
  where
    getStr (SqlName n') = n'

instance ToSqlCode SqlType where
  toSqlCode (SqlType n)
    | hasSquotes n || hasParenthesesPair n || hasNoDot n || hasPercent n = n
    | otherwise = toSqlCode' $ expSqlName $ SqlName n
      -- if quotes are contained
      -- assume that user cares for correct enquoting  
    where
      hasSquotes = isIn '"'
      -- if at least a pair of brakets is found
      -- assume that a type like varchar(20) is meant
      hasParenthesesPair x = '(' `isIn` x && ')' `isIn` x
      -- if no dot is present, assume that buildin type
      -- like integer is meant
      hasNoDot x = not ('.' `isIn` x)
      -- if % is present, assume that something like
      -- table%ROWTYPE could be meant
      hasPercent = isIn '%'

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

class ToSqlCode a where
  toSqlCode :: a -> Text

class ToSqlName a where
  toSqlName :: a -> SqlName

class SqlIdentifierConcat a where
  (//) :: a -> a -> a

instance Semigroup SqlName where
  x@(SqlName x') <> y@(SqlName y')
    | x == mempty = y
    | y == mempty = x
    | otherwise = SqlName (x' <> "_" <> y')

instance Monoid SqlName where
  mempty = SqlName ""
  mappend = (<>)

-- | SqlName
newtype SqlName =
  SqlName Text
  deriving (Generic, Ord, Show, Data)

unsafeInternalName :: SqlName -> Text
unsafeInternalName (SqlName x) = x

instance FromJSON SqlName where
  parseJSON = genericParseJSON myOpt

instance ToJSON SqlName where
  toJSON = toYamSqlJson

newtype SqlType =
  SqlType Text
  deriving (Generic, Show, Eq, Data)

instance FromJSON SqlType where
  parseJSON = genericParseJSON myOpt

instance ToJSON SqlType where
  toJSON = toYamSqlJson
