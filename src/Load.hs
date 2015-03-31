-- This file is part of HamSql
--
-- Copyright 2014 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

module Load where

import Control.Exception
import Data.Yaml
import qualified Data.ByteString.Char8 as B
import System.FilePath.Posix (combine, dropFileName)
import Control.Monad (filterM, liftM)
import Text.Regex.Posix
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import Data.Yaml
import Control.Monad
import Data.List

import Parser
import Option
import Utils

loadSetup :: OptCommon -> FilePath -> IO Setup
loadSetup opts filePath = do
  setup <- readObjectFromFile opts filePath
  setup' <- loadSetupModules opts (dropFileName filePath) (initSetupInternal setup)
  return $ applyTpl setup'

initSetupInternal s' = s' {
  xsetupInternal = Just SetupInternal { setupModuleData = [] }
}

-- Tries to loads all defined modules from defined module dirs
loadSetupModules :: OptCommon -> FilePath -> Setup -> IO Setup
loadSetupModules opts path s = do
  moduleData <- sequence [ loadModule path name | name <- setupModules s ]
  return s {
          xsetupInternal = Just (setupInternal s) {
            setupModuleData = moduleData
          }
      }

  where
    loadModule :: FilePath -> String -> IO Module
    loadModule path name = do
      modulePath <- findModulePath name moduleDirs
      moduleData <- readModule opts modulePath
      return moduleData {
          xmoduleInternal = Just ModuleInternal {
            moduleLoadPath = modulePath
          }
        }

    moduleDirs = map (combine path) (setupModuleDirs s)

findModulePath :: String -> [FilePath] -> IO FilePath
findModulePath moduleName search = findDir search
  where
    findDir [] =
      err $ "Module '" ++ moduleName ++ "' not found in " ++ show search
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
    "In file '" ++ filePath ++ "': " ++ exc
   Right _ -> x

yamlEnding :: String -> Bool
yamlEnding xs = xs =~ "\\.yaml$"

pgsqlEnding :: String -> Bool
pgsqlEnding xs = xs =~ "\\.sql$"

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir path = do
    conts <- getDirectoryContents path
    let ordConts = sort conts
    liftM (map (combine path)) (filterM doesFileExist' ordConts)
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

errorCheck :: String -> Bool -> IO ()
errorCheck msg False = err msg
errorCheck _   True  = return ()

readModule :: OptCommon -> FilePath -> IO Module
readModule opts md = do
    doesDirectoryExist md >>= errorCheck ("module dir does not exist: " ++ md)

    moduleData <- readObjectFromFile opts moduleConfig

    tables <- do
      files <- selectFilesInDir yamlEnding (combine md "tables.d")
      sequence [
        do
          t <- readObjectFromFile opts f
          return $ tablePopulateInternal moduleData f t
        | f <- files ]

    functions <- do
      files <- selectFilesInDir pgsqlEnding (combine md "functions.d")
      sequence [
        do
          t <- readObjectFromFile opts f
          return $ functionPopulateInternal moduleData f t
        | f <- files ]

    let moduleData' = moduleData {
      moduleTables = maybeLeftJoin (moduleTables moduleData) tables,
      moduleFunctions = maybeLeftJoin (moduleFunctions moduleData) functions,
      moduleTypes = Just $ maybeMap (typePopulateInternal moduleData md) (moduleTypes moduleData),
      moduleDomains = Just $ maybeMap (domainPopulateInternal moduleData md) (moduleDomains moduleData)
    }

    return moduleData'

    where
        moduleConfig = combine md "module.yaml"

insertTable :: Module -> Table -> Module
insertTable m t = m {
    moduleTables = maybeJoin (moduleTables m) (Just [t])
  }

tablePopulateInternal :: Module -> FilePath -> Table -> Table
tablePopulateInternal m path t = t {
    xtableInternal = Just TableInternal {
      tableParentModule = m,
      tableLoadPath = path,
      tableOriginal = t
    }
  }

functionPopulateInternal :: Module -> FilePath -> Function -> Function
functionPopulateInternal m path f = f {
    xfunctionInternal = Just FunctionInternal {
      functionParentModule = m,
      functionLoadPath = path,
      functionOriginal = f,
      functionReturnsTable = toSql (functionReturns f) == "TABLE"
    }
  }

typePopulateInternal :: Module -> FilePath -> Type -> Type
typePopulateInternal m path t = t {
    xtypeInternal = Just TypeInternal {
      typeParentModule = m,
      typeLoadPath = path,
      typeOriginal = t
    }
  }

domainPopulateInternal :: Module -> FilePath -> Domain -> Domain
domainPopulateInternal m path d = d {
    xdomainInternal = Just DomainInternal {
      domainParentModule = m,
      domainLoadPath = path,
      domainOriginal = d
    }
  }

readObjectFromFile :: (FromJSON a, ToJSON a) => OptCommon -> FilePath -> IO a
readObjectFromFile opts file = do
  info opts $ "Reading and parsing yaml-file '" ++ file ++ "'"

  fileExists <- doesFileExist file
  unless fileExists $
    err $ "Expected file existance: '" ++ file ++ "'"

  c <- B.readFile file
  catchErrors file $
   case decodeEither' c of
    Left msg  -> err $ "in yaml-file: " ++ file ++ ": " ++ (show msg)
    Right obj -> obj
  
