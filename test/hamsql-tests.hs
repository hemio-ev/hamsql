module Main where

import Control.Exception.Safe
import Control.Monad.Trans.Reader (runReaderT)
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as T
import Database.PostgreSQL.Simple (Connection, close)
import System.Exit
import Text.Pretty.Simple

import Test.Tasty
import Test.Tasty.HUnit

import Database.HamSql.Cli
import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.InquireDeployed
import Database.HamSql.Internal.Load (loadSetup)
import Database.HamSql.Internal.PostgresCon
import Database.HamSql.Setup
import Database.HamSql.Write
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
        [ testCase "domain.yml" $ installSetup "test/setups/domain.yml"
        , testGroup
            "self-test.yml"
            [ selfTestStmt "test/setups/self-test.yml"
            , selfTestStruct
            , selfTestUpgrade "test/setups/self-test.yml"
            --, selfTestUpgrade "test/setups/self-test-empty.yml"
            --, selfTestUpgradeDelete "test/setups/self-test-empty.yml"
            , selfTestUpgrade "test/setups/self-test.yml"
            ]
        , testGroup
            "self test stmt only"
            [ selfTestStmt "test/setups/self-test-empty.yml"
            --,selfTestStmt "test/setups/self-test-stmt.yml"
            , selfTestStmt "test/setups/domain.yml"
            , selfTestUpgrade "test/setups/domain.yml"
            , selfTestUpgrade "test/setups/domain-upgrade.yml"
            ]
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
    --pPrint schemasDb

selfTestStmt :: String -> TestTree
selfTestStmt file =
  testCaseSteps ("stmt " ++ file) $ \step -> do
    (schemasDb, setupLocal) <- deploy step installSetup file
    mapM_ (doWrite "/tmp/testout" . schemaToDirTree) schemasDb
    step "check statement diff"
    assertNoDiff
      (sort $ stmtsInstall (newSetup schemasDb))
      (sort $ stmtsInstall setupLocal)

selfTestStruct :: TestTree
selfTestStruct =
  testCaseSteps "struct" $ \step -> do
    (schemasDb, setupLocal) <-
      deploy step installSetup "test/setups/self-test.yml"
    step "check schema diff"
    assertNoShowDiff schemasDb (fromMaybe [] $ setupSchemaData setupLocal)

selfTestUpgrade :: String -> TestTree
selfTestUpgrade file =
  testCaseSteps ("upgrade self-test " ++ file) $ \step
    {-
    step "load setup ..."
    schemasDb <- conn >>= runReaderT deployedSchemas
    setupLocal <- loadSetup file
    let stmtsDb = sort $ stmtsInstall (newSetup schemasDb)
    let stmtsSrc = sort $ stmtsInstall setupLocal
    step $ "stmts src: " ++ show (length stmtsSrc) ++ ", stmts db: " ++ show (length stmtsDb)
    step "Missing statements, to be executed"
    step $ T.unpack $ pShow (stmtsSrc \\ stmtsDb)
    step "Residual statements, to be dropped or converted to delete stmt"
    step $ T.unpack $ pShow $ stmtsUpdateDrop (stmtsDb \\ stmtsSrc)
    -}
    --------------------------------------
   -> do
    (schemasDb, setupLocal) <- deploy step upgradeSetup file
    step "check schema diff"
    assertNoShowDiff schemasDb (fromMaybe [] $ setupSchemaData setupLocal)

selfTestUpgradeDelete :: String -> TestTree
selfTestUpgradeDelete file =
  testCaseSteps ("upgrade self-test delete " ++ file) $ \step -> do
    (schemasDb, setupLocal) <- deploy step upgradeSetupDelete file
    step "check schema diff"
    assertNoShowDiff schemasDb (fromMaybe [] $ setupSchemaData setupLocal)

deploy ::
     (String -> IO ())
  -> (String -> Assertion)
  -> String
  -> IO ([Schema], Setup)
deploy step f file = do
  step "deploy ..."
  f file
  step "retrive deployed from database ..."
  con <- conn
  schemasDb <- runReaderT deployedSchemas con
  close con
  step "load setup ..."
  setupLocal <- loadSetup file
  return (schemasDb, setupLocal)

conn :: IO Connection
conn =
  pgsqlConnectUrl $ getConUrlApp "hamsql-test" "postgresql://postgres@/test1"

installSetup :: String -> Assertion
installSetup s =
  exec'
    [ "install"
    , "--verbose"
    , "--delete-residual-roles"
    , "--permit-data-deletion"
    , "-ds"
    , s
    , "-c"
    , "postgresql://postgres@/test1"
    , "--sql-log"
    , "/tmp/log.sql"
    ]

upgradeSetup :: String -> Assertion
upgradeSetup s =
  exec' ["upgrade", "--verbose", "-s", s, "-c", "postgresql://postgres@/test1"]

upgradeSetupDelete :: String -> Assertion
upgradeSetupDelete s =
  exec'
    [ "upgrade"
    , "--verbose"
    , "--permit-data-deletion"
    , "-s"
    , s
    , "-c"
    , "postgresql://postgres@/test1"
    , "--sql-log"
    , "/tmp/del.sql"
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

exec :: (Eq e, Exception e) => e -> [String] -> IO Bool
exec y xs =
  handle (\x -> return $ x == y) (parseThisArgv xs >>= run >> return True)

exec' :: [String] -> Assertion
exec' xs = do
  r <- exec ExitSuccess xs
  r @? "Exec should not fail"

assertNoShowDiff :: (Show a0, Show a1) => a0 -> a1 -> Assertion
assertNoShowDiff x y =
  assertNoDiff (T.lines $ pShowNoColor x) (T.lines $ pShowNoColor y)

assertNoDiff :: (Show a, Eq a) => [a] -> [a] -> Assertion
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

firstDiff :: Eq a => [a] -> [a] -> Maybe Int
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
