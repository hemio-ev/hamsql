module Main where

import Database.HamSql.Cli

main :: IO ()
main = do
    exec ["--help"] 
  where exec xs = parseThisArgv xs >>= run
