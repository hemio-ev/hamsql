-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.


{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module PostgresCon where

import Control.Exception
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal
import Network.URL
import qualified Data.ByteString.Char8 as B
import Data.String

import SQL
import Parser
import Utils
import Options

toQry :: String -> Query
toQry = fromString

pgsqlGetFullStatements :: Opt -> Setup -> IO [SqlStatement]
pgsqlGetFullStatements opts setup  = do
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
  connResult <- try $ connectPostgreSQL $ (B.pack (exportURL url))

  return $ case connResult of
    Left e@(SqlError{}) -> err $ "sql connection failed: " ++ B.unpack (sqlErrorMsg e)
    Right conn -> conn

pgsqlDeleteRoleStmt :: URL -> String -> IO [SqlStatement]
pgsqlDeleteRoleStmt url prefix = do
  conn <- pgsqlConnectUrl url

  result :: [Only String] <- query_ conn $ toQry
    ("SELECT rolname FROM pg_roles WHERE rolname LIKE '" ++ prefix ++ "%'")
  let users = result
  
  return $ map statement users
  
  where
    -- statement :: (TextBl.ByteString) -> SqlStatement
    statement (Only user) = SqlStmtRoleDelete $ "DROP ROLE \"" ++ user ++ "\""

pgsqlHandleErr code e@(SqlError{}) =
  err $
    "sql error in following statement:\n" ++
    code ++ "\n" ++
    "sql error: " ++ B.unpack (sqlErrorMsg e) ++ " (Error Code: " ++ B.unpack (sqlState e) ++ ")"
pgsqlHandleQry code e@(QueryError{}) = do
  putStrLn $
    "Information for SQL query:\n" ++
    code ++ "\n" ++
    "Message: '" ++ qeMessage e ++ "'"
  return 2

isNotEmptyStmt SqlStmtEmpty = False
isNotEmptyStmt _ = True

pgsqlExecWithoutTransact :: URL -> [SqlStatement] -> IO ()
pgsqlExecWithoutTransact connUrl xs = do
    conn <- connectPostgreSQL (B.pack (exportURL connUrl))
       
    _ <- sequence [ 
      do
        let code = (Parser.toSql stmt)
        execResult <- catch (catch (execute_ conn (toQry code)) (pgsqlHandleErr code)) (pgsqlHandleQry code)

        return ()
      | stmt <- filter isNotEmptyStmt xs ]

    return ()

pgsqlExec :: URL -> [SqlStatement] -> IO ()
pgsqlExec connUrl xs = do
    conn <- connectPostgreSQL (B.pack (exportURL connUrl))
    begin conn
       
    _ <- sequence [ 
      do
        let code = (Parser.toSql stmt)
        execResult <- catch (catch (execute_ conn (toQry code)) (pgsqlHandleErr code)) (pgsqlHandleQry code)

        return ()
      | stmt <- filter isNotEmptyStmt xs ]

    commit conn

