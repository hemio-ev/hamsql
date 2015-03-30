import Options.Applicative (execParser)
import Network.URL
import Data.List
import Data.Text (unpack)
import Control.Monad (when)

import Utils
import SQL
import Option
import PostgresCon
import Load
import Documentation

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
  
  useSqlStmts optDb statements
  
-- Upgrade
run (Upgrade opt optDb optUpgrade) = do
    setup <- loadSetup opt (optSetup opt)
  
    conn <- pgsqlConnectUrl (getConUrl optDb)
    
    deleteStmts <- pgsqlDeleteAllStmt conn   
    createStmts <- pgsqlGetFullStatements opt optDb setup
    
    let stmts = (sort deleteStmts) ++ (sort (Data.List.filter afterDelete createStmts))
    
    useSqlStmts optDb stmts

-- Doc
run (Doc opt optDoc) = do
  setup <- loadSetup opt (optSetup opt)
  html <- toSetupDoc setup
  dot <- getGraphDoc setup
    
  case optFormat optDoc of
    "html" -> putStrLn $ getDoc $ unpack html
    "dot" -> putStrLn $ unpack dot

useSqlStmts :: OptCommonDb -> [SqlStatement] -> IO ()
useSqlStmts optDb stmts
  | optPrint optDb = putStrLn $ sqlPrinter $ sqlAddTransact stmts
  | optEmulate optDb = pgsqlExec (getConUrl optDb) stmts
  | otherwise = pgsqlExec (getConUrl optDb) stmts
