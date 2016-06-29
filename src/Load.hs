-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Load where

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

import Database.YamSql
import Option
import Parser
import Utils

loadSetup :: OptCommon -> FilePath -> IO Setup
loadSetup opts filePath = do
  setup <- readObjectFromFile opts filePath
  setup' <- loadSetupModules opts (dropFileName filePath) (initSetupInternal setup)
  return $ applyTpl setup'

initSetupInternal s' = s' {
  setupModules = removeDuplicates $ setupModules s',
  xsetupInternal = Just SetupInternal { setupModuleData = [] }
}

-- Tries to loads all defined modules from defined module dirs
loadSetupModules :: OptCommon -> FilePath -> Setup -> IO Setup
loadSetupModules opts path s = do
  moduleData <- sequence [ loadModule path (getName name) | name <- setupModules s ]
  return s {
          xsetupInternal = Just (setupInternal s) {
            setupModuleData = moduleData
          }
      }

  where
    loadModule :: FilePath -> FilePath -> IO Module
    loadModule path name = do
      modulePath <- findModulePath name moduleDirs
      moduleData <- readModule opts modulePath
      return moduleData {
          xmoduleInternal = Just ModuleInternal {
            moduleLoadPath = modulePath
          }
        }

    moduleDirs = map (combine path) (setupModuleDirs s)
    -- TODO: make global
    getName (SqlName n) = T.unpack n

findModulePath :: FilePath -> [FilePath] -> IO FilePath
findModulePath moduleName search = findDir search
  where
    findDir [] =
      err $ "Module '" <> tshow moduleName <> "' not found in " <> tshow search
    findDir (d:ds) = do
      let dir = combine d moduleName
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

yamlEnding :: FilePath -> Bool
yamlEnding xs = takeExtension xs == ".yaml" || takeExtension xs == "yml"

pgsqlEnding :: FilePath -> Bool
pgsqlEnding xs = isAlphaNum (last fn) && head fn /= '.'
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

readModule :: OptCommon -> FilePath -> IO Module
readModule opts md = do
    doesDirectoryExist md >>= errorCheck ("module dir does not exist: " <> tshow md)

    moduleData <- readObjectFromFile opts moduleConfig

    tables <- do
      files <- selectFilesInDir yamlEnding (combine md "tables.d")
      sequence [
        readObjectFromFile opts f :: IO Table
        | f <- files ]

    functions <- do
      files <- selectFilesInDir pgsqlEnding (combine md "functions.d")
      sequence [
        readFunctionFromFile opts f :: IO Function
        | f <- files ]

    let moduleData' = moduleData {
        moduleTables = maybeJoin (moduleTables moduleData) (Just tables),
        moduleFunctions = maybeJoin (moduleFunctions moduleData) (Just functions)
    }

    return moduleData'

    where
        moduleConfig = combine md "module.yaml"

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

