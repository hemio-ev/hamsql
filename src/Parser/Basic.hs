{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser.Basic where

import Control.Exception
import Data.Typeable
import Data.Yaml ()
import Data.Aeson.Types
import Data.Char
import Data.Maybe (fromJust,fromMaybe)
import Data.Data
import Data.Text (unpack)
import Data.List.Ordered (subset,minus,sort)
import GHC.Generics
import Data.List.Split (splitOn)
import Data.HashMap.Strict (member,insert,keys)
import System.IO
import qualified Data.ByteString.Char8 as B

import Utils

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
myOpt = defaultOptions { fieldLabelModifier     = snakeify . removeFirstPart
                       , constructorTagModifier = drop 1 . snakeify}

--outJson :: Setup -> String
outJson s = show $ toJSON s

forceToJson :: ToJSON a => a -> IO ()
forceToJson s = do
  withFile "/dev/null" WriteMode
   (\handle -> hPutStrLn handle $ show $ toJSON s)

-- SqlCode (right now only SqlName)

unsafePlainName (SqlName n) = n

instance Eq SqlName where
  (==) x y = toSql x == toSql y

instance SqlCode SqlName
  where
    toSql (SqlName n) = 
      if '"' `elem` n then
        n
      else
        toSql' $ expSqlName $ SqlName n

    (//) (SqlName s) (SqlName t) = SqlName (s ++ t)

(<.>) :: SqlName -> SqlName -> SqlName
(<.>) (SqlName s) (SqlName t) = SqlName $ s ++ "." ++ t

getSql (SqlName s) = "\"" ++ s ++ "\""

expSqlName :: SqlName -> [SqlName]
expSqlName n = map SqlName (splitOn "." (getStr n))
  where
    getStr (SqlName n') = n'

instance SqlCode SqlType
  where
    toSql (SqlType n) = 
      if
        -- if quotes are contained
        -- assume that user cares for correct enquoting
        '"' `elem` n ||
        -- if at least a pair of brakets is found
        -- assume that a type like varchar(20) is meant
        ('(' `elem` n && ')' `elem` n) ||
        -- if no dot is present, assume that buildin type
        -- like integer is meant
        not ('.' `elem` n) ||
        -- if % is present, assume that something like
        -- table%ROWTYPE could be meant
        '%' `elem` n
      then
        n
      else
        toSql' $ expSqlName $ SqlName n

    (//) (SqlType s) (SqlType t) = SqlType (s ++ t)

contSqlName :: [SqlName] -> SqlName
contSqlName ns = SqlName $ join "." $ map getStr ns
  where
    getStr (SqlName n') = n'

toSql' :: [SqlName] -> String
toSql' xs = join "." $ map quotedName xs
  where
    quotedName (SqlName s) = "\"" ++ s ++ "\""
    
class SqlCode a where
  toSql :: a -> String
  (//) :: a -> a -> a

 
-- SqlName
newtype SqlName = SqlName String deriving (Generic,Ord,Show, Typeable, Data)
instance FromJSON SqlName where parseJSON = genericParseJSON myOpt
instance ToJSON SqlName where toJSON = genericToJSON myOpt

newtype SqlType = SqlType String deriving (Generic,Show,Eq, Typeable, Data)
instance FromJSON SqlType where parseJSON = genericParseJSON myOpt
instance ToJSON SqlType where toJSON = genericToJSON myOpt


strictParseYaml xs =
 do
  parsed <- genericParseJSON myOpt xs
  let diff = minus (keysOfValue xs) (keysOfData parsed)
  return $
   if diff == [] then
    parsed
   else
    throw $ YamsqlException $ "Found unknown keys: " ++ show diff

 where
  keysOfData u = sort $ "tag":map (snakeify.removeFirstPart) (constrFields (toConstr u))
  
  keysOfValue :: Value -> [String]
  keysOfValue (Object xs) = sort $ map unpack $ keys xs

-- EXCEPTIONS

data YamsqlException = YamsqlException String
 deriving (Show, Typeable)

instance Exception YamsqlException


