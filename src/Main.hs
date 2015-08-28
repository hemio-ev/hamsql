-- This file is part of HamSql
--
-- Copyright 2014-2015 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

import Options.Applicative (execParser)
import Network.URL
import Data.List
import Data.Text (unpack)
import Control.Monad (when)

import Documentation
import Load
import Option
import PostgresCon
import Sql
import Sql.Statement.Create
import Utils

main :: IO ()
main = execParser parserInfoHamsql >>= run

run :: Command -> IO ()

-- Install
run (Install opt optDb optInstall) = do
  let dbname = url_path $ getConUrl optDb

  if not (optEmulate optDb || optPrint optDb) then
    pgsqlExecWithoutTransact
      ((getConUrl optDb) { url_path = "postgres" })
      (sqlCreateDatabase (optDeleteExistingDatabase optInstall) dbname)
  else 
    when (optDeleteExistingDatabase optInstall) $
      warn' $ "WARNING [hamsql]: In --emulate and --print mode the" ++
        " DROP/CREATE DATABASE statements are skipped. You have to ensure that a empty " ++
        " database exists for those commands to make sense."
    
  setup <- loadSetup opt (optSetup opt)

  statements <- pgsqlGetFullStatements opt optDb setup
  
  useSqlStmts optDb (sort statements)
  
-- Upgrade
run (Upgrade opt optDb optUpgrade) = do
    setup <- loadSetup opt (optSetup opt)
  
    conn <- pgsqlConnectUrl (getConUrl optDb)
    
    deleteStmts <- pgsqlDeleteAllStmt conn   
    createStmts <- pgsqlGetFullStatements opt optDb setup
    
    fragile <- pgsqlUpdateFragile conn createStmts
    
    let stmts = (sort deleteStmts) ++ (sort $ fragile ++ (Data.List.filter afterDelete createStmts))
    
    useSqlStmts optDb stmts

-- Doc
run (Doc opt optDoc) =
  case optFormat optDoc of
    "html" -> do
      setup <- loadSetup opt (optSetup opt)
      html <- toSetupDoc optDoc setup
      putStrLn $ getDoc $ unpack html
      
    "dot" -> do
      setup <- loadSetup opt (optSetup opt)
      dot <- getGraphDoc optDoc setup
      putStrLn $ unpack dot

useSqlStmts :: OptCommonDb -> [SqlStatement] -> IO ()
useSqlStmts optDb stmts
  | optPrint optDb = putStrLn $ sqlPrinter $ sqlAddTransact stmts
  | optEmulate optDb = pgsqlExec (getConUrl optDb) stmts
  | otherwise = pgsqlExec (getConUrl optDb) stmts
