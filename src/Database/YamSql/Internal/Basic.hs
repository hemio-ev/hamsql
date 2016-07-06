-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.YamSql.Internal.Basic (module Database.HamSql.Internal.Utils, module Database.YamSql.Internal.Basic, genericParseJSON, genericToJSON, ToJSON(..), FromJSON(..), Generic(..), Data(..), Typeable(..)) where

import           Control.Exception
import           Data.Aeson.Types    (Options (..), defaultOptions, genericParseJSON,
                                      genericToJSON)
import           Data.Char
import           Data.Data
import           Data.HashMap.Strict (keys)
import           Data.List
import           Data.Maybe          (fromJust, fromMaybe)
import qualified Data.Text           as T
import           Data.Yaml
import           GHC.Generics
import           System.IO

import Database.HamSql.Internal.Utils

-- removes first part of camel case. e.g.:
-- columnDescriptionField |-> descriptionField
removeFirstPart :: String -> String
removeFirstPart xs = lowerStr rest
     where
        rest = dropWhile isLower xs
        lowerStr (x':xs') = toLower x':xs'
        lowerStr []       = "__"

-- makes camelCaseSpelling to camel_case_spelling
snakeify :: String -> String
snakeify [] = []
snakeify (x:xs)
 | isUpper x = '_' : toLower x : snakeify xs
 | otherwise =               x : snakeify xs

myOpt :: Options
myOpt = defaultOptions {
 fieldLabelModifier     = snakeify . removeFirstPart
, constructorTagModifier = drop 1 . snakeify
}

outJson :: ToJSON a => a -> String
outJson s = show $ toJSON s

forceToJson :: ToJSON a => a -> IO ()
forceToJson s =
  withFile "/dev/null" WriteMode
 (\handle -> hPrint handle (toJSON s))

-- SqlCode (right now only SqlName)

unsafePlainName :: SqlName -> Text
unsafePlainName (SqlName n) = n

instance Eq SqlName where
  (==) x y = toSql x == toSql y

instance SqlCode SqlName
  where
    toSql (SqlName n) =
      if '"' `isIn` n then
        n
      else
        toSql' $ expSqlName $ SqlName n

    (//) (SqlName s) (SqlName t) = SqlName (s <> t)

(<.>) :: SqlName -> SqlName -> SqlName
(<.>) (SqlName s) (SqlName t) = SqlName $ s <> "." <> t

getSql (SqlName s) = "\"" <> s <> "\""

expSqlName :: SqlName -> [SqlName]
expSqlName n = map SqlName (T.splitOn "." (getStr n))
  where
    getStr (SqlName n') = n'

instance SqlCode SqlType
  where
    toSql (SqlType n) =
      if
        -- if quotes are contained
        -- assume that user cares for correct enquoting
        '"' `isIn` n ||
        -- if at least a pair of brakets is found
        -- assume that a type like varchar(20) is meant
        ('(' `isIn` n && ')' `isIn` n) ||
        -- if no dot is present, assume that buildin type
        -- like integer is meant
        not ('.' `isIn` n) ||
        -- if % is present, assume that something like
        -- table%ROWTYPE could be meant
        '%' `isIn` n
      then
        n
      else
        toSql' $ expSqlName $ SqlName n

    (//) (SqlType s) (SqlType t) = SqlType (s <> t)

contSqlName :: [SqlName] -> SqlName
contSqlName ns = SqlName $ T.intercalate "." $ map getStr ns
  where
    getStr (SqlName n') = n'

toSql' :: [SqlName] -> Text
toSql' xs = T.intercalate "." $ map quotedName xs
  where
    quotedName (SqlName s) = "\"" <> s <> "\""

class SqlCode a where
  toSql :: a -> Text
  (//) :: a -> a -> a


-- SqlName
newtype SqlName = SqlName Text deriving (Generic,Ord,Show, Typeable, Data)
instance FromJSON SqlName where parseJSON = genericParseJSON myOpt
instance ToJSON SqlName where toJSON = genericToJSON myOpt

newtype SqlType = SqlType Text deriving (Generic,Show,Eq, Typeable, Data)
instance FromJSON SqlType where parseJSON = genericParseJSON myOpt
instance ToJSON SqlType where toJSON = genericToJSON myOpt

strictParseYaml xs =
 do
  parsed <- genericParseJSON myOpt xs

  let diff = keysOfValue xs \\ keysOfData parsed
  return $
   if null diff then
    parsed
   else
    throw $ YamsqlException $ "Found unknown keys: " <> tshow diff
 where
  keysOfData u = "tag":map (snakeify.removeFirstPart) (constrFields (toConstr u))

  keysOfValue :: Value -> [String]
  keysOfValue (Object xs) = map T.unpack $ keys xs

-- EXCEPTIONS

data YamsqlException = YamsqlException Text
 deriving (Show, Typeable)

instance Exception YamsqlException


