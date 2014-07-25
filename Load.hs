-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

module Load where

import Data.Yaml
import qualified Data.ByteString.Char8 as B
import System.FilePath.Posix (combine)
import Control.Monad (filterM,liftM)
import Text.Regex.Posix
import System.Directory (doesFileExist,doesDirectoryExist,getDirectoryContents)

import Parser
import Options
import Utils
import SQL

loadSetup :: FilePath -> IO (Setup)
loadSetup filePath = do
  setup <- loadYamlFile filePath
  setup' <- loadSetupModules (initSetupInternal setup)
  return $ implementAndApplyTemplates setup'

initSetupInternal s' = s' {
  xsetupInternal = Just SetupInternal { setupModuleData = [] }
} 

-- Tries to loads all defined modules from defined module dirs
loadSetupModules :: Setup -> IO Setup
loadSetupModules s = do
  moduleData <- sequence [ loadModule name | name <- setupModules s ]
  return s {
          xsetupInternal = Just (setupInternal s) {
            setupModuleData = moduleData
          }
      }
  
  where
    loadModule :: FilePath -> IO Module
    loadModule name = do
      modulePath <- findModulePath name (setupModuleDirs s)
      moduleData <- readModule modulePath
      return moduleData {
          xmoduleInternal = Just ModuleInternal {
            moduleLoadPath = modulePath
          }
        }
    
findModulePath :: String -> [FilePath] -> IO FilePath
findModulePath moduleName search = findDir search
  where
    findDir [] =
      error $ "Module '" ++ moduleName ++ "'not found in " ++ (show search)
    findDir (d:ds) = do
      let dir = combine d moduleName
      dirExists <- doesDirectoryExist (dir :: FilePath)
      case dirExists of
	True -> do
	  fileExists <- doesFileExist (combine dir "module.yaml")
	  case fileExists of
	    True -> return dir
	    False -> error $ "file 'module.yaml' missing in '" ++ dir ++ "'"
	False -> do
	  xxx <- findDir ds
	  return xxx
    
loadYamlFile:: (FromJSON a0) => FilePath -> IO (a0)
loadYamlFile filePath = do
  fileContent <- B.readFile filePath
  case decodeEither fileContent of
    Left msg -> error $
      "Error while decoding '" ++ filePath ++ "'. " ++ msg
    Right decoded -> return decoded

yamlEnding :: String -> Bool
yamlEnding xs = xs =~ "\\.yaml$"

pgsqlEnding :: String -> Bool
pgsqlEnding xs = xs =~ "\\.pgsql$"

getFilesInDir :: FilePath -> IO [FilePath]
getFilesInDir path = do conts <- getDirectoryContents path
                        liftM (map ((path++"/")++)) (filterM doesFileExist' conts)
 where doesFileExist' relName = doesFileExist (path++"/"++relName)

selectFilesInDir :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
selectFilesInDir ending dir = do
  dirExists <- doesDirectoryExist dir
  if not dirExists then
    return []
  else do
    files <- getFilesInDir dir  
    return $ filter ending files

errorCheck :: String -> Bool -> IO ()
errorCheck msg False = error msg
errorCheck _   True  = return ()
    
readModule :: FilePath -> IO Module
readModule md = do
    doesDirectoryExist md >>= (errorCheck $ "module dir does not exist: "++md)
    
    doesFileExist (moduleConfig) >>=
      (errorCheck $ "module file does not exist: " ++ moduleConfig)
    moduleFile <- B.readFile (moduleConfig)
    moduleData <- case decodeEither moduleFile of
            Left msg -> err $ "in file " ++ moduleConfig ++ ": " ++ msg
            Right m  -> return m
            
    tables <- do
      files <- selectFilesInDir yamlEnding (combine md "tables.d")
      tables' <- sequence [
        do
          t <- readObjectFromFile f
          return $ tablePopulateInternal moduleData f t
        | f <- files ]
      return tables'
    
    functions <- do
      files <- selectFilesInDir pgsqlEnding (combine md "functions.d")
      functions' <- sequence [
        do
          t <- readObjectFromFile f
          return $ functionPopulateInternal moduleData f t
        | f <- files ]
      return functions'
    
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
      functionOriginal = f
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

readObjectFromFile :: FromJSON a => FilePath -> IO a
readObjectFromFile file = do
  c <- B.readFile file
  case decodeEither c of
    Left msg  -> err $ "in file: " ++ file ++ ": " ++ msg
    Right obj -> return obj
     

