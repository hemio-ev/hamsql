-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Database.YamSql.Parser
  ( module Database.YamSql.Parser
  , genericParseJSON
  , genericToJSON
  , ToJSON(..)
  , FromJSON(..)
  , Generic(..)
  , Data(..)
  ) where

import Control.Exception
import Data.Aeson.Types
       (GFromJSON, GToJSON, Options(..), defaultOptions, genericParseJSON,
        genericToJSON, Zero)

import Data.Char
import Data.Data
import Data.HashMap.Strict (keys)
import Data.List ((\\), minimumBy)
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import System.IO
import Text.EditDistance (defaultEditCosts, levenshteinDistance)

import Database.HamSql.Internal.Utils

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

parseYamSql
  :: (Generic r, GFromJSON Zero (Rep r), Data r)
  => Value -> Parser r
parseYamSql xs = do
  parsed <- genericParseJSON myOpt xs
  let diff = keysOfValue xs \\ keysOfData parsed
  return $
    if null diff
      then parsed
      else throw $
           YamsqlException $
           "Found unknown YamSql fields: " <>
           T.concat (map (explainMissing (keysOfData parsed)) diff)
  where
    keysOfData u =
      "tag" : map (snakeify . removeFirstPart) (constrFields (toConstr u))
    keysOfValue :: Value -> [String]
    keysOfValue (Object ys) = map T.unpack $ keys ys
    keysOfValue _ = err "HAMSQL-UNEXPECTED 3"
    explainMissing :: [String] -> String -> Text
    explainMissing ys x =
      "\n - " <> tshow x <> " (did you mean " <> tshow (closestString x ys) <>
      "?)"

closestString :: String -> [String] -> String
closestString x = minimumBy (\y y' -> compare (dist y) (dist y'))
  where
    dist = levenshteinDistance defaultEditCosts x

toYamSqlJson
  :: (Generic a, GToJSON Zero (Rep a))
  => a -> Value
toYamSqlJson = genericToJSON myOpt

data YamsqlException =
  YamsqlException Text
  deriving (Show)

instance Exception YamsqlException
