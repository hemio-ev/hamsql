-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

module Main where

import System.Environment
import System.Console.GetOpt
import Data.List
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
    setup <- loadSetup opts (optSetupFile opts)
    u <- getGraphDoc setup
    putStrLn $ unpack u
    
  | optPrintDoc opts = do
    setup <- loadSetup opts (optSetupFile opts)
    u <- toSetupDoc setup
    putStrLn $ getDoc $ unpack u
    
  | optExecuteSql opts = do
    setup <- loadSetup opts (optSetupFile opts)
    statements <- pgsqlGetFullStatements opts setup
    
    pgsqlExec (optServerConnectionUrl opts) (sort statements)

  | optPrintPhp opts = do
    setup <- loadSetup opts (optSetupFile opts)
    let statements = getSetupPhpClasses opts setup
    putStrLn statements

  | optPrintJson opts = do
    setup <- loadSetup opts (optSetupFile opts)
    putStrLn $ outJson setup

  | optPrintSql opts = do
    setup <- loadSetup opts (optSetupFile opts)
    statements <- pgsqlGetFullStatements opts setup
    putStrLn $ sqlPrinter $ sqlAddTransact (sort statements)
    putStrLn "-- end of SQL code"
 
 | optShowVersion opts = 
    print "YamSql v0.0001"

  | otherwise = putStrLn $ usageInfo usageHeader options

