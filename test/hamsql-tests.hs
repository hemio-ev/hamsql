module Main where

import Control.Exception.Safe
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.ByteString as B
import Data.Yaml.Pretty
import Database.PostgreSQL.Simple (Connection)
import System.Exit

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

xx :: TestTree
xx =
  testCase "tables" $ do
    deploySetup "self-test.yml"
    schemas <- conn >>= runReaderT deployedSchemas
    --B.putStrLn $ encodePretty defConfig (newSetup schemas)
    stmtsYamSql <-
      pgsqlGetFullStatements =<< (loadSetup "test/setups/self-test.yml")
    stmtsDb <- pgsqlGetFullStatements (newSetup schemas)
    print (show stmtsYamSql)
    print (show stmtsDb)
    firstListDiff stmtsDb stmtsYamSql @?= Nothing

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
