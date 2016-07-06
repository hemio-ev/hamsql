-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Load where

import Control.Exception
import Control.Monad

import qualified Data.ByteString       as B
import           Data.Char
import           Data.Frontmatter
import           Data.List
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8)
import           Data.Yaml
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        getDirectoryContents)
import           System.FilePath.Posix (combine, dropFileName, takeExtension,
                                        takeFileName)

import Database.HamSql.Setup
import Database.YamSql
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Utils

loadSetup :: OptCommon -> FilePath -> IO Setup
loadSetup opts filePath = do
  setup <- readObjectFromFile opts filePath
  setup' <- loadSetupSchemas opts (dropFileName filePath) (initSetupInternal setup)
  return $ applyTpl setup'

initSetupInternal s' = s' {
  setupSchemas = removeDuplicates $ setupSchemas s',
  xsetupInternal = Just SetupInternal { setupSchemaData = [] }
}

-- Tries to loads all defined modules from defined module dirs
loadSetupSchemas :: OptCommon -> FilePath -> Setup -> IO Setup
loadSetupSchemas opts path s = do
  schemaData <- sequence [ loadSchema path (T.unpack $ unsafePlainName name) | name <- setupSchemas s ]
  return s {
          xsetupInternal = Just (setupInternal s) {
            setupSchemaData = schemaData
          }
      }

  where
    loadSchema :: FilePath -> FilePath -> IO Schema
    loadSchema path name = do
      schemaPath <- findSchemaPath name schemaDirs
      schemaData <- readSchema opts schemaPath
      return schemaData {
          xmoduleInternal = Just SchemaInternal {
            schemaLoadPath = schemaPath
          }
        }

    schemaDirs = map (combine path) (setupSchemaDirs s)

findSchemaPath :: FilePath -> [FilePath] -> IO FilePath
findSchemaPath schemaName search = findDir search
  where
    findDir [] =
      err $ "Schema '" <> tshow schemaName <> "' not found in " <> tshow search
    findDir (d:ds) = do
      let dir = combine d schemaName
      dirExists <- doesDirectoryExist (dir :: FilePath)
      if dirExists then
         return dir
      else
         findDir ds

catchErrors :: (FromJSON a, ToJSON a) => FilePath -> a -> IO a
catchErrors filePath x = do
 y <- try (forceToJson x)
 return $
  case y of
   Left (YamsqlException exc) -> err $
    "In file '" <> tshow filePath <> "': " <> exc
   Right _ -> x

isConfigDirFile :: FilePath -> Bool
isConfigDirFile xs = isAlphaNum (last fn) && head fn /= '.'
 where fn = takeFileName xs

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
  if not dirExists then
    return []
  else do
    files <- getFilesInDir dir
    return $ filter ending files

errorCheck :: Text -> Bool -> IO ()
errorCheck msg False = err msg
errorCheck _   True  = return ()

readSchema :: OptCommon -> FilePath -> IO Schema
readSchema opts md = do
    doesDirectoryExist md >>= errorCheck ("module dir does not exist: " <> tshow md)

    schemaData <- readObjectFromFile opts schemaConfig

    tables <- do
      files <- selectFilesInDir isConfigDirFile (combine md "tables.d")
      sequence [
        readObjectFromFile opts f :: IO Table
        | f <- files ]

    functions <- do
      files <- selectFilesInDir isConfigDirFile (combine md "functions.d")
      sequence [
        readFunctionFromFile opts f :: IO Function
        | f <- files ]

    let schemaData' = schemaData {
        schemaTables = maybeJoin (schemaTables schemaData) (Just tables),
        schemaFunctions = maybeJoin (schemaFunctions schemaData) (Just functions)
    }

    return schemaData'

    where
        schemaConfig = combine md "schema.yml"

readObjectFromFile :: (FromJSON a, ToJSON a) => OptCommon -> FilePath -> IO a
readObjectFromFile opts file = do
    b <- readYamSqlFile opts file
    readObject file b

readObject :: (FromJSON a, ToJSON a) => FilePath -> B.ByteString -> IO a
readObject file b =
    catchErrors file $
        case decodeEither' b of
            Left msg  -> err $ "in yaml-file: " <> tshow file <> ": " <> tshow msg
            Right obj -> obj

readFunctionFromFile :: OptCommon -> FilePath -> IO Function
readFunctionFromFile opts file = do
    b <- readYamSqlFile opts file

    case parseFrontmatter b of
        Done body yaml -> do
            f <- readObject file yaml
            return $ f { functionBody = Just (decodeUtf8 body) }
        _ -> readObject file b

readYamSqlFile :: OptCommon -> FilePath -> IO B.ByteString
readYamSqlFile opts file = do
    info opts $ "Reading file '" <> tshow file <> "'"
    fileExists <- doesFileExist file
    unless fileExists $
        err $ "Expected file existance: '" <> tshow file <> "'"

    B.readFile file

