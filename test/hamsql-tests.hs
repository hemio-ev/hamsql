module Main where

import System.Directory
import Test.Tasty
import Test.Tasty.HUnit

import Database.HamSql.Cli

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Integration Tests"
    [testCase "1" (exec ["--help"]), integrationTests]

integrationTests :: TestTree
integrationTests =
  testCase "x" $
    --exec ["--help"]
   do
    exec
      [ "install"
      , "-s"
      , "test/setups/invalid.yml"
      , "-c"
      , "postgresql://postgres@/test1"
      ]
    getCurrentDirectory >>= putStrLn

exec xs = parseThisArgv xs >>= run
