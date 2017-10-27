module Main where

import Control.Exception.Safe
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as T
import Data.Yaml.Pretty
import Database.PostgreSQL.Simple (Connection)
import System.Exit
import Text.Pretty.Simple

--import System.Directory
import Test.Tasty
import Test.Tasty.HUnit

import Database.HamSql.Cli
import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.InquireDeployed
import Database.HamSql.Internal.Load (loadSetup)
import Database.HamSql.Internal.PostgresCon
import Database.HamSql.Setup
import Database.YamSql
import Database.YamSql.Internal.SqlId (SqlName(..))

conn :: IO Connection
conn =
  pgsqlConnectUrl $ getConUrlApp "hamsql-test" "postgresql://postgres@/test1"

deploySetup s =
  exec'
    [ "install"
    , "--permit-data-deletion"
    , "-ds"
    , "test/setups/" ++ s
    , "-c"
    , "postgresql://postgres@/test1"
    ]

--B.putStrLn $ encodePretty defConfig (newSetup schemas)
xx :: TestTree
xx =
  testCase "tables" $ do
    deploySetup "self-test.yml"
    setupLocal <- (loadSetup "test/setups/self-test.yml")
    stmtsLocal <- pgsqlGetFullStatements setupLocal
    schemasDb <- conn >>= runReaderT deployedSchemas
    stmtsDb <- pgsqlGetFullStatements (newSetup schemasDb)
    assertNoShowDiff (Just schemasDb) (setupSchemaData setupLocal)
    assertNoDiff stmtsDb stmtsLocal

assertNoShowDiff
  :: (Show a0, Show a1)
  => a0 -> a1 -> Assertion
assertNoShowDiff x y =
  assertNoDiff (T.lines $ pShowNoColor x) (T.lines $ pShowNoColor y)

assertNoDiff
  :: (Show a, Eq a)
  => [a] -> [a] -> Assertion
assertNoDiff xs ys =
  case firstListDiff xs ys of
    Nothing -> return ()
    Just (x, y) ->
      assertFailure $
      T.unpack
        ("version 1: " <> pShowNoColor x <> "\nversion 2: " <> pShowNoColor y)

firstListDiff
  :: Eq a
  => [a] -> [a] -> Maybe (Maybe a, Maybe a)
firstListDiff [] [] = Nothing
firstListDiff [] (y:_) = Just (Nothing, Just y)
firstListDiff (x:_) [] = Just (Just x, Nothing)
firstListDiff (x:xs) (y:ys)
  | x == y = firstListDiff xs ys
  | otherwise = Just (Just x, Just y)

newSetup :: [Schema] -> Setup
newSetup s =
  Setup
  { setupSchemas = []
  , setupSchemaDirs = Nothing
  , setupRolePrefix = Just "hamsql-test_"
  , setupPreCode = Nothing
  , setupPostCode = Nothing
  , setupSchemaData = Just s
  }

---------------------
---------------------
---------------------
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Integration Tests" [integrationTests, integrationTests2, abc]

abc :: TestTree
abc =
  testGroup
    "grp3"
    [ testCase "setups/domain.yml" $
      exec'
        [ "install"
        , "--delete-residual-roles"
        , "--permit-data-deletion"
        , "-ds"
        , "test/setups/domain.yml"
        , "-c"
        , "postgresql://postgres@/test1"
        ]
    , xx
    ]

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
