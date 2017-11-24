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
  ( GFromJSON
  , GToJSON
  , Options(..)
  , SumEncoding(UntaggedValue)
  , Zero
  , defaultOptions
  , genericParseJSON
  , genericToJSON
  )

import Data.Char
import Data.Data
import Data.HashMap.Strict (keys)
import Data.List ((\\), minimumBy)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import System.IO
import Text.EditDistance
  ( Costs(ConstantCost)
  , defaultEditCosts
  , levenshteinDistance
  , substitutionCosts
  )

import Database.YamSql.Internal.Utils

-- removes first part of camel case. e.g.:
-- columnDescriptionField |-> descriptionField
removeFirstPart :: String -> String
removeFirstPart xs = lowerStr rest
  where
    rest = dropWhile (\x -> isLower x || x == '_') xs
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
  , sumEncoding = UntaggedValue
  }

outJson :: ToJSON a => a -> String
outJson s = show $ toJSON s

forceToJson :: ToJSON a => a -> IO ()
forceToJson s =
  withFile "/dev/null" WriteMode (\handl -> hPrint handl (toJSON s))

parseYamSql :: (Generic r, GFromJSON Zero (Rep r), Data r) => Value -> Parser r
parseYamSql v = do
  let used = keysOfValue v
  parsed <- genericParseJSON myOpt v
  let known = keysOfData parsed
  let diff = used \\ known
  return $
    if null diff
      then parsed
      else throw $
           YamsqlException $
           "Found unknown YamSql fields: " <>
           T.concat (map (explainMissing known used) diff)
  where
    keysOfData u =
      "tag" : map (snakeify . removeFirstPart) (constrFields (toConstr u))
    keysOfValue :: Value -> [String]
    keysOfValue (Object ys) = map T.unpack $ keys ys
    keysOfValue _ = []
    explainMissing :: [String] -> [String] -> String -> Text
    explainMissing known used x =
      "\n - " <> tshow x <> " (did you mean " <> tshow (closestString x ls) <>
      "?)"
      where
        ls = filter (/= "tag") (known \\ used)

closestString :: String -> [String] -> String
closestString _ [] = "*no additional parameter at all*"
closestString x ys = minimumBy (\y y' -> compare (dist y) (dist y')) ys
  where
    dist =
      levenshteinDistance
        defaultEditCosts {substitutionCosts = ConstantCost 2}
        x

toYamSqlJson :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
toYamSqlJson = genericToJSON myOpt

data YamsqlException =
  YamsqlException Text
  deriving (Show)

instance Exception YamsqlException
