-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.Load where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Char
import Data.Frontmatter
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml
import System.Directory
       (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath.Posix (combine, dropFileName, takeFileName)
import System.IO (stdin)

import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Utils
import Database.HamSql.Setup
import Database.YamSql
import Database.YamSql.Parser

loadSetup :: OptCommon -> FilePath -> IO Setup
loadSetup opts filePath = do
  setup <- readObjectFromFile opts filePath
  setup' <-
    loadSetupSchemas opts (dropFileName filePath) (initSetupInternal setup)
  return $ applyTpl setup'
  where
    initSetupInternal s' =
      s'
      { setupSchemas = removeDuplicates $ setupSchemas s'
      , setupSchemaData = Nothing
      }

-- Tries to loads all defined modules from defined module dirs
loadSetupSchemas :: OptCommon -> FilePath -> Setup -> IO Setup
loadSetupSchemas opts path s = do
  schemaData <- loadSchemas opts path s [] (setupSchemas s)
  return s {setupSchemaData = Just schemaData}

loadSchemas :: OptCommon
            -> FilePath
            -> Setup
            -> [Schema]
            -> [SqlName]
            -> IO [Schema]
loadSchemas _ _ _ allLoaded [] = return allLoaded
loadSchemas optCom path setup loadedSchemas missingSchemas = do
  schemas <-
    sequence
      [ loadSchema (T.unpack $ unsafePlainName schema)
      | schema <- missingSchemas
      ]
  let newDependencyNames =
        nub . concat $ map (fromMaybe [] . schemaDependencies) schemas
  let allLoadedSchemas = schemas ++ loadedSchemas
  let newMissingDepencenyNames =
        newDependencyNames \\ map schemaName allLoadedSchemas
  loadSchemas optCom path setup allLoadedSchemas newMissingDepencenyNames
  where
    loadSchema :: FilePath -> IO Schema
    loadSchema schema = do
      schemaPath <- findSchemaPath schema schemaDirs
      readSchema optCom schemaPath
    schemaDirs = map (combine path) (fromMaybe [""] $ setupSchemaDirs setup)

findSchemaPath :: FilePath -> [FilePath] -> IO FilePath
findSchemaPath schema search = findDir search
  where
    findDir [] =
      err $ "Schema '" <> tshow schema <> "' not found in " <> tshow search
    findDir (d:ds) = do
      let dir = combine d schema
      dirExists <- doesDirectoryExist (dir :: FilePath)
      if dirExists
        then return dir
        else findDir ds

catchErrors
  :: ToJSON a
  => FilePath -> a -> IO a
catchErrors filePath x = do
  y <- try (forceToJson x)
  return $
    case y of
      Left (YamsqlException exc) ->
        err $ "In file '" <> tshow filePath <> "': " <> exc
      Right _ -> x

isConfigDirFile :: FilePath -> Bool
isConfigDirFile xs = isAlphaNum (last fn) && head fn /= '.'
  where
    fn = takeFileName xs

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir path = do
  conts <- getDirectoryContents path
  let ordConts = sort conts
  fmap (map (combine path)) (filterM doesFileExist' ordConts)
  where
    doesFileExist' relName = doesFileExist (combine path relName)

selectFilesInDir :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
selectFilesInDir ending dir = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then return []
    else do
      files <- getFilesInDir dir
      return $ filter ending files

errorCheck :: Text -> Bool -> IO ()
errorCheck errMsg False = err errMsg
errorCheck _ True = return ()

readSchema :: OptCommon -> FilePath -> IO Schema
readSchema opts md = do
  doesDirectoryExist md >>=
    errorCheck ("module dir does not exist: " <> tshow md)
  schemaData <- readObjectFromFile opts schemaConfig
  domains <- confDirFiles "domains.d" >>= mapM (readObjectFromFile opts)
  types <- confDirFiles "types.d" >>= mapM (readObjectFromFile opts)
  sequences <- confDirFiles "sequences.d" >>= mapM (readObjectFromFile opts)
  tables <- confDirFiles "tables.d" >>= mapM (readObjectFromFile opts)
  functions <-
    let ins x s = x {functionBody = Just s}
    in confDirFiles "functions.d" >>= mapM (readFunctionFromFile ins opts)
  let schemaData' =
        schemaData
        { schemaDomains = schemaDomains schemaData <> Just domains
        , schemaTypes = schemaTypes schemaData <> Just types
        , schemaSequences = schemaSequences schemaData <> Just sequences
        , schemaTables = schemaTables schemaData <> Just tables
        , schemaFunctions = schemaFunctions schemaData <> Just functions
        }
  return schemaData'
  where
    schemaConfig = combine md "schema.yml"
    confDirFiles confDir = selectFilesInDir isConfigDirFile (combine md confDir)

readObjectFromFile
  :: (FromJSON a, ToJSON a)
  => OptCommon -> FilePath -> IO a
readObjectFromFile opts file = do
  b <- readYamSqlFile opts file
  readObject file b

readObject
  :: (FromJSON a, ToJSON a)
  => FilePath -> B.ByteString -> IO a
readObject file b =
  catchErrors file $
  case decodeEither' b of
    Left errMsg -> err $ "in yaml-file: " <> tshow file <> ": " <> tshow errMsg
    Right obj -> obj

readFunctionFromFile
  :: (FromJSON a, ToJSON a)
  => (a -> Text -> a) -> OptCommon -> FilePath -> IO a
readFunctionFromFile rpl opts file = do
  b <- readYamSqlFile opts file
  case parseFrontmatter b of
    Done body yaml -> do
      f <- readObject file yaml
      return $ rpl f (decodeUtf8 body)
    _ -> readObject file b

readYamSqlFile :: OptCommon -> FilePath -> IO B.ByteString
readYamSqlFile opts "-" = do
  debug opts "Reading file from STDIN" $ B.hGetContents stdin
readYamSqlFile opts file = do
  fileExists <- doesFileExist file
  unless fileExists $ err $ "Expected file existance: '" <> tshow file <> "'"
  debug opts ("Reading file " <> tshow file) $ B.readFile file
