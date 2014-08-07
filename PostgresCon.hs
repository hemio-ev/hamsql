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
  
pgsqlExec :: URL -> [SqlStatement] -> IO ()
pgsqlExec connUrl xs = do
    connResult <- try $ connectPostgreSQL $ exportURL connUrl

    let conn = case connResult of
                 Left e@(SqlError{}) ->
                   err $ "sql connection failed: " ++ seErrorMsg e
                 Right conn' -> conn'
       
    _ <- sequence [ 
      do
        let code = Parser.toSql stmt
        execResult <- try (runRaw conn code)

        case execResult of
                  Left e@(SqlError{}) -> do
                    err $
                      "sql error in following statement:\n" ++
                      code ++ "\n" ++
                      "sql error: " ++ seErrorMsg e ++ "(Error Code: " ++ seState e ++ ")"
                  Right _ -> return ()

        return ()
      | stmt <- xs ]

    commit conn
