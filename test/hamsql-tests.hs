module Main where

import Control.Exception.Safe
import Control.Monad.Trans.Reader (runReaderT)
import Database.PostgreSQL.Simple
import System.Exit

--import System.Directory
import Test.Tasty
import Test.Tasty.HUnit

import Database.HamSql.Cli
import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.InquireDeployed
import Database.YamSql.Internal.SqlId (SqlName(..))

main :: IO ()
main = defaultMain tests

conn :: IO Connection
conn =
  pgsqlConnectUrl $
  getConUrlApp "hamsql-test" "postgresql://postgres@/carnivora"

tests :: TestTree
tests = testGroup "Integration Tests" [integrationTests, integrationTests2, abc]

abc :: TestTree
abc =
  testGroup
    "grp3"
    [ testCase "setups/domain.yml" $
      exec'
        [ "install"
        , "--permit-data-deletion"
        , "-ds"
        , "test/setups/domain.yml"
        , "-c"
        , "postgresql://postgres@/test1"
        ]
    , xx
    ]

xx :: TestTree
xx =
  testCase "tables" $ do
    tables <- conn >>= runReaderT (deployedTables $ SqlName "web")
    print $ show tables

integrationTests2 :: TestTree
integrationTests2 = testCase "x" $ exec' ["--help"]

integrationTests :: TestTree
integrationTests =
  testCase "y" $ do
    r <-
      exec
        (ExitFailure 1)
        [ "install"
        , "-s"
        , "test/setups/invalid.yml"
        , "-c"
        , "postgresql://postgres@/test1"
        ]
    r @? "Should fail"

exec
  :: (Eq e, Exception e)
  => e -> [String] -> IO Bool
exec y xs =
  handle (\x -> return $ x == y) (parseThisArgv xs >>= run >> return True)

exec' :: [String] -> Assertion
exec' xs = do
  r <- exec ExitSuccess xs
  r @? "Exec should not fail"
