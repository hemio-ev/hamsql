-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

module PostgresCon where

import Control.Exception
import Database.HDBC
import Database.HDBC.PostgreSQL
import Network.URL

import SQL
import Parser
import Utils
import Options

pgsqlGetFullStatements :: Opt -> Setup -> IO [SqlStatement]
pgsqlGetFullStatements opts setup  = do
  -- CREATE DATABASE part disabled, see bug <https://github.com/hdbc/hdbc/issues/25>
  --pgsqlExec
  --  ((optServerConnectionUrl opts) { url_path = "" })
  --  (sqlCreateDatabase $ url_path $ optServerConnectionUrl opts)
  
  role_delete_stmts <- pgsqlDeleteRoleStmt (optServerConnectionUrl opts) rolePrefix
  let main_stmts = getSetupStatements opts setup

  return $ main_stmts ++ role_delete_stmts

  where
    rolePrefix :: String
    rolePrefix = getPrefix (setupRolePrefix' setup)
    getPrefix :: SqlName -> String
    getPrefix (SqlName xs) = xs
 
pgsqlConnectUrl :: URL -> IO Connection
pgsqlConnectUrl url = do
  connResult <- try $ connectPostgreSQL $ exportURL url

  return $ case connResult of
    Left e@(SqlError{}) -> err $ "sql connection failed: " ++ seErrorMsg e
    Right conn -> conn

pgsqlDeleteRoleStmt :: URL -> String -> IO [SqlStatement]
pgsqlDeleteRoleStmt url prefix = do
  conn <- pgsqlConnectUrl url

  result <- quickQuery conn
    ("SELECT rolname FROM pg_roles WHERE rolname LIKE '" ++ prefix ++ "%'") []
  let users = map (fromSql.head) result :: [String]
  
  return $ map statement users
  
  where
    statement user = SqlStmtRoleDelete $ "DROP ROLE \"" ++ user ++ "\""

pgsqlExec :: URL -> [SqlStatement] -> IO ()
pgsqlExec connUrl xs = do
    conn <- pgsqlConnectUrl connUrl
       
    _ <- sequence [ 
      do
        let code = Parser.toSql stmt
        execResult <- try (runRaw conn code)

        case execResult of
                  Left e@(SqlError{}) -> 
                    err $
                      "sql error in following statement:\n" ++
                      code ++ "\n" ++
                      "sql error: " ++ seErrorMsg e ++ "(Error Code: " ++ seState e ++ ")"
                  Right _ -> return ()

        return ()
      | stmt <- xs ]

    commit conn

