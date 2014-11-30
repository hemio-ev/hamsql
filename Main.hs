-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

module Main where

import System.Environment
import System.Console.GetOpt
import Data.Text
import Network.URL

import Parser
import Options
import SQL
import PHP
import Utils
import Load
import Documentation
import PostgresCon

main :: IO ()
main = do
  -- read options from command line
  optargs <- getArgs
  (opts,args) <- processOpts optargs

  main' opts args

main' :: Opt -> [String] -> IO ()
main' opts args
  | optPrintGraph opts = do
    setup <- loadSetup (optSetupFile opts)
    u <- getGraphDoc setup
    putStrLn $ unpack u
    
  | optPrintDoc opts = do
    setup <- loadSetup (optSetupFile opts)
    u <- toSetupDoc setup
    putStrLn $ getDoc $ unpack u
    
  | optExecuteSql opts = do
    setup <- loadSetup (optSetupFile opts)
    
    -- CREATE DATABASE part disabled, see bug <https://github.com/hdbc/hdbc/issues/25>
    --pgsqlExec
    --  ((optServerConnectionUrl opts) { url_path = "" })
    --  (sqlCreateDatabase $ url_path $ optServerConnectionUrl opts)
    
    let statements = getSetupStatements opts setup
    pgsqlExec (optServerConnectionUrl opts) statements

  | optPrintPhp opts = do
    setup <- loadSetup (optSetupFile opts)
    let statements = getSetupPhpClasses opts setup
    putStrLn statements

  | optPrintJson opts = do
    setup <- loadSetup (optSetupFile opts)
    putStrLn $ outJson setup

  | optPrintSql opts = do
    setup <- loadSetup (optSetupFile opts)
    let statements = getSetupStatements opts setup
    putStrLn $ sqlPrinter $ sqlAddTransact statements
    putStrLn "-- end of SQL code"
 
 | optShowVersion opts = 
    print "YamSql v0.0001"

  | otherwise = putStrLn $ usageInfo usageHeader options

