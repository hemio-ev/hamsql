module Main where

import Control.Exception.Safe
import Control.Monad.Trans.Reader (runReaderT)

import qualified Data.ByteString as B
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as T

import Data.Maybe (catMaybes, fromMaybe)
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

main :: IO ()
main =
  defaultMain $
  testGroup
    "Tests"
    [ testGroup
        "CLI"
        [integrationTests, testCase "show help" $ exec' ["--help"]]
    , testGroup
        "Integration Tests"
        [ testCase "domain.yml" $ deploySetup "test/setups/domain.yml"
        , testGroup "self-test.yml" [selfTestStmt, selfTestStruct]
        ]
    ]

integrationTests :: TestTree
integrationTests =
  testCase "invalid schema" $ do
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
    --B.putStrLn $ encodePretty defConfig (setupLocal)
    --pPrint schemasDb

--B.putStrLn $ encodePretty defConfig (newSetup schemas)
selfTestStmt :: TestTree
selfTestStmt =
  testCaseSteps "stmt" $ \step -> do
    (schemasDb, setupLocal) <- deploy step "test/setups/self-test.yml"
    --B.putStrLn $ encodePretty (setConfDropNull True defConfig) (setupLocal)
    step "check statement diff"
    assertNoDiff
      (pgsqlGetFullStatements (newSetup schemasDb))
      (pgsqlGetFullStatements setupLocal)

selfTestStruct :: TestTree
selfTestStruct =
  testCaseSteps "struct" $ \step -> do
    (schemasDb, setupLocal) <- deploy step "test/setups/self-test.yml"
    step "check schema diff"
    assertNoShowDiff schemasDb (fromMaybe [] $ setupSchemaData setupLocal)

deploy :: (String -> IO ()) -> String -> IO ([Schema], Setup)
deploy step file = do
  step "deploy ..."
  deploySetup file
  step "retrive deployed from database ..."
  schemasDb <- conn >>= runReaderT deployedSchemas
  step "load setup ..."
  setupLocal <- loadSetup file
  return (schemasDb, setupLocal)

conn :: IO Connection
conn =
  pgsqlConnectUrl $ getConUrlApp "hamsql-test" "postgresql://postgres@/test1"

deploySetup :: String -> Assertion
deploySetup s =
  exec'
    [ "install"
    , "--delete-residual-roles"
    , "--permit-data-deletion"
    , "-ds"
    , s
    , "-c"
    , "postgresql://postgres@/test1"
    ]

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

exec
  :: (Eq e, Exception e)
  => e -> [String] -> IO Bool
exec y xs =
  handle (\x -> return $ x == y) (parseThisArgv xs >>= run >> return True)

exec' :: [String] -> Assertion
exec' xs = do
  r <- exec ExitSuccess xs
  r @? "Exec should not fail"

assertNoShowDiff
  :: (Show a0, Show a1)
  => a0 -> a1 -> Assertion
assertNoShowDiff x y =
  assertNoDiff (T.lines $ pShowNoColor x) (T.lines $ pShowNoColor y)

assertNoDiff
  :: (Show a, Eq a)
  => [a] -> [a] -> Assertion
assertNoDiff xs ys =
  case firstDiff xs ys of
    Nothing -> return ()
    Just i ->
      assertFailure $
      T.unpack $
      "version 1:\n" <> showContext xs i <> "\n\nversion 2:\n" <>
      showContext ys i
  where
    getContext zs i =
      let d =
            case get zs i of
              Nothing -> 2
              Just n
                | T.length (pShowNoColor n) > 150 -> 0
                | otherwise -> 2
      in catMaybes [get zs j | j <- [i - d .. i + d]]
    showContext zs i = T.intercalate "\n" $ map pShowNoColor $ getContext zs i

firstDiff
  :: Eq a
  => [a] -> [a] -> Maybe Int
firstDiff xs ys = findDiff 0
  where
    findDiff i
      | i < max (length xs) (length ys) =
        if get xs i /= get ys i
          then Just i
          else findDiff (i + 1)
      | otherwise = Nothing

get :: [a] -> Int -> Maybe a
get xs i
  | i < length xs && i >= 0 = Just (xs !! i)
  | otherwise = Nothing
