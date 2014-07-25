-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE DeriveGeneric #-}

module Options where

import System.Console.GetOpt
import Data.Yaml
import Data.Aeson.Types
import GHC.Generics
import Network.URL
--import URL
import Data.Maybe

import Parser
import Utils

data Opt = Opt
 { optVerbose     :: Bool
 , optShowVersion :: Bool
 , optExecuteSql  :: Bool
 , optPrintSql    :: Bool
 , optPrintPhp    :: Bool
 , optPrintDoc    :: Bool
 , optPrintJson   :: Bool
 , optSetupFile   :: FilePath
 , optServerConnectionUrl :: URL
 } deriving (Generic,Show)
--instance FromJSON Opt where parseJSON = genericParseJSON myOpt
--instance FromJSON URL where parseJSON = importURL

defaultOpts :: Opt
defaultOpts = Opt
 { optVerbose     = False
 , optShowVersion = False
 , optExecuteSql  = False
 , optPrintSql    = False
 , optPrintPhp	  = False
 , optPrintJson   = False
 , optPrintDoc    = False
 , optSetupFile   = "setup.yaml"
 , optServerConnectionUrl = defaultServerConnectionUrl
 }
 
defaultServerConnectionUrl = 
  URL
    (Absolute $ Host (RawProt "postgres")
    "postgres@localhost" Nothing) ""
    [("fallback_application_name", defaultServerConnectionAppName)]
    
defaultServerConnectionAppName = "HamSql"
    
options :: [OptDescr (Opt -> Opt)]
options =
  [ Option ['v']     ["verbose"]
    (NoArg (\opts -> opts { optVerbose = True }))
    "chatty output on stderr"
  , Option ['V']     ["version"]
    (NoArg (\opts -> opts { optShowVersion = True }))
    "shows version of this program"
  , Option ['e']     ["execute-sql"]
    (NoArg (\opts -> opts { optExecuteSql = True }))
    "executes sql code on sql server"
  , Option ['p']     ["print-sql"]
    (NoArg (\opts -> opts { optPrintSql = True }))
    "dumps the generated sql code to stdout"
  , Option ['d']     ["print-doc"]
    (NoArg (\opts -> opts { optPrintDoc = True }))
    "sumehow generates documentation, this could have many options, but yet there are non."
  , Option ['j']     ["print-json"]
    (NoArg (\opts -> opts { optPrintJson = True }))
    "prints full setup as json"
  , Option ['P']     ["print-php"]
    (NoArg (\opts -> opts { optPrintPhp = True }))
    "first debug output for php output"
  , Option ['c']     ["connection-url"]
    (OptArg (\u opts -> opts { optServerConnectionUrl = 
      case u of
           Nothing -> defaultServerConnectionUrl 
           (Just s) -> fromJustReason ("invalid url: " ++ s) (importURL s) 
      }) "postgresql://<user>@<host>/<db>?<options>")
    "connection options for postgresql server"    
  , Option ['s']     ["setup_file"]
    (ReqArg (\ds opts -> opts
             { optSetupFile = ds })
     "setup.yaml")
    "setup file to use, this defines everything..."
  ]


processOpts :: [String] -> IO (Opt, [String])
processOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOpts o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))
      
usageHeader = "Usage: yamsql [OPTION...]"
