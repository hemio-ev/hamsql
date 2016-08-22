-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Sql where

import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Database.PostgreSQL.Simple.FromField

import Database.HamSql.Internal.Utils
import Database.YamSql.Internal.Basic


instance FromField SqlType where
 fromField x y = do
    -- use existing parser for strings
    typeName <- fromField x y
    return $ SqlType (typeName::Text)

instance FromField SqlName where
 fromField x y = do
    -- use existing parser for strings
    typeName <- fromField x y
    return $ SqlName (typeName::Text)

afterDelete :: SqlStatement -> Bool
afterDelete (SqlStmt t _ _) = f t
  where
      f :: SqlStatementType -> Bool

      f SqlCreateSchema = True
      f SqlRoleMembership = True
      f SqlCreateFunction = True
      f SqlCreatePrimaryKeyConstr = True
      f SqlCreateUniqueConstr = True
      f SqlCreateForeignKeyConstr = True
      f SqlCreateCheckConstr = True
      f SqlAddDefault = True
      f SqlPriv = True
      f SqlComment = True
      f _ = False
afterDelete _ = False

-- SQL statements

data SqlStatement =
  SqlStmt SqlStatementType SqlName Text |
  SqlStmtFunction SqlStatementType SqlName [SqlType] Text |
  SqlStmtEmpty
  deriving (Show)

data SqlStatementType =
  SqlDropDatabase |
  SqlCreateDatabase |
  SqlPreInstall |
  SqlDropRole |
  SqlCreateRole |
  SqlRoleMembership |
  SqlCreateSchema |
  SqlCreateDomain |
  SqlCreateType |
  -- DROP CONSTRAINTS
  SqlDropTableConstraint |
  SqlDropDomainConstraint |
  -- DROP FUNCTION
  SqlDropColumn |
  SqlDropTable |
  SqlDropFunction |
  -- SEQUENCE
  SqlCreateSequence |
  -- TABLE
  SqlCreateTable |
  SqlAddColumn |
  SqlAlterTable |
  SqlDropColumnDefault |
  SqlAlterColumn |
  -- TRIGGER
  SqlCreateTrigger |
  -- ALTER SEQUENCE
  SqlAlterSequence |
  -- FUNCTION
  SqlDropDomain |
  SqlDropType |
  SqlCreateFunction |
  SqlInherit |
  SqlCreatePrimaryKeyConstr |
  SqlCreateUniqueConstr |
  SqlCreateForeignKeyConstr |
  SqlCreateCheckConstr |
  SqlAddDefault |
  SqlPriv |
  SqlComment |
  SqlUnclassified |
  SqlPostInstall
    deriving (Eq, Ord, Show)

instance Eq SqlStatement where
  (==) (SqlStmt t1 n1 _) (SqlStmt t2 n2 _) = t1 == t2 && n1 == n2
  (==) (SqlStmtFunction t1 n1 s1 _) (SqlStmtFunction t2 n2 s2 _) =
    t1 == t2 && n1 == n2 && s1 == s2
  (==) SqlStmtEmpty SqlStmtEmpty = True
  (==) _ _ = False

instance Ord SqlStatement where
  (<=) (SqlStmt t1 _ _) (SqlStmt t2 _ _) = t1 <= t2
  (<=) (SqlStmtFunction t1 _ _ _) (SqlStmt t2 _ _) = t1 <= t2
  (<=) (SqlStmt t1 _ _) (SqlStmtFunction t2 _ _ _) = t1 <= t2
  (<=) (SqlStmtFunction t1 _ _ _) (SqlStmtFunction t2 _ _ _) = t1 <= t2
  (<=) SqlStmtEmpty SqlStmtEmpty = True
  (<=) SqlStmtEmpty _ = False
  (<=) _ SqlStmtEmpty = True

typeEq :: SqlStatementType -> SqlStatement -> Bool
typeEq t1 (SqlStmt t2 _ _) = t1 == t2
typeEq t1 (SqlStmtFunction t2 _ _ _) = t1 == t2
typeEq _ SqlStmtEmpty = False

stmtsFilterExecutable :: [SqlStatement] -> [SqlStatement]
stmtsFilterExecutable = filter executable
  where
    executable SqlStmtEmpty = False
    executable _ = True

replacesTypeOf :: SqlStatementType -> SqlStatement -> SqlStatement
replacesTypeOf t (SqlStmt _ x y) = SqlStmt t x y
replacesTypeOf t (SqlStmtFunction _ x y z) = SqlStmtFunction t x y z

instance SqlCode SqlStatement where
  toSql SqlStmtEmpty = ""
  toSql (SqlStmt SqlPreInstall _ xs) = xs <> "\n"
  toSql (SqlStmt SqlPostInstall _ xs) = xs <> "\n"
  toSql (SqlStmt _ _ xs) = termin xs
  toSql (SqlStmtFunction _ _ _ xs) = termin xs
  (//) _ _ = undefined

termin "" = undefined
termin ys = ys <> statementTermin

statementTermin :: Text
statementTermin = ";\n"

sqlFromStmt :: SqlStatement -> Text
sqlFromStmt (SqlStmt _ _ str) = str
sqlFromStmt (SqlStmtFunction _ _ _ str) = str
sqlFromStmt SqlStmtEmpty = undefined

sqlPrinter :: [SqlStatement] -> Text
sqlPrinter xs = T.concat $ map toSql xs

toSqlString :: Text -> Text
toSqlString xs = "'" <> T.replace "'" "''" xs <> "'"

