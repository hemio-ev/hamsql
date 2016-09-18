-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE GADTs #-}

--{-# LANGUAGE FlexibleInstances #-}
module Database.HamSql.Internal.Stmt where

import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromField

import Database.HamSql.Internal.Utils
import Database.YamSql

data SqlStmtId = SqlStmtId
  { stmtType :: SqlStmtType
  , stmtSqlId :: SqlId
  } deriving (Eq, Ord)

instance Show SqlStmtId where
  show (SqlStmtId x y) = "(SqlStmtId " ++ show x ++ " " ++ show y ++ ")"

data SqlStmt
  = SqlStmt SqlStmtId
            Text
  | SqlStmtEmpty
  deriving (Show)

stmtId :: SqlStmt -> Maybe SqlStmtId
stmtId (SqlStmt x _) = Just x
stmtId SqlStmtEmpty = Nothing

stmtBody :: SqlStmt -> Maybe Text
stmtBody (SqlStmt _ x) = Just x
stmtBody SqlStmtEmpty = Nothing

stmtIdType :: SqlStmt -> Maybe SqlStmtType
stmtIdType (SqlStmt x _) = Just (stmtType x)
stmtIdType SqlStmtEmpty = Nothing

stmtDesc :: SqlStmt -> Text
stmtDesc stmt =
  case stmtId stmt of
    Nothing -> "EMPTY-STATEMENT"
    Just x -> T.pack (sqlIdType $ stmtSqlId x) <-> toSqlCode (stmtSqlId x)

instance Eq SqlStmt where
  x == y = stmtId x == stmtId y

instance Ord SqlStmt where
  x `compare` y = stmtId x `compare` stmtId y

instance ToSqlId SqlStmt where
  sqlId x =
    case stmtId x of
      Nothing -> SqlId $ SqlIdContentObj "?" (SqlName "")
      Just y -> stmtSqlId y

newSqlStmtId
  :: (ToSqlId a)
  => SqlStmtType -> a -> SqlStmtId
newSqlStmtId x y = SqlStmtId x (sqlId y)

newSqlStmt
  :: (ToSqlId a)
  => SqlStmtType -> a -> Text -> SqlStmt
newSqlStmt t o b = SqlStmt (newSqlStmtId t o) b

sqlPrinter :: [SqlStmt] -> Text
sqlPrinter xs = T.concat $ map toSqlCode xs

instance FromField SqlType where
  fromField x y = SqlType <$> fromField x y

instance FromField SqlName where
  fromField x y = SqlName <$> fromField x y

-- | More like always perform unfiltered after delete
allowInUpgrade :: SqlStmt -> Bool
allowInUpgrade SqlStmtEmpty = False
allowInUpgrade x =
  case stmtId x of
    Nothing -> False
    Just y ->
      case stmtType y of
        SqlPreInstall -> False
        SqlPostInstall -> False
        _ -> True

stmtRequiresPermitDeletion :: SqlStmt -> Bool
stmtRequiresPermitDeletion x =
  case stmtIdType x of
    Nothing -> False
    Just SqlDropDatabase -> True
    Just SqlDropTable -> True
    Just SqlDropTableColumn -> True
    Just _ -> False

data SqlStmtType
  = SqlDropDatabase
  | SqlCreateDatabase
  | SqlPreInstall
  | SqlDropRole
  | SqlCreateRole
  | SqlRoleMembership
  | SqlCreateSchema
  | SqlCreateDomain
  | SqlCreateType
    -- DROP CONSTRAINTS
  | SqlDropTableConstr
  | SqlDropDomainConstr
  | SqlDropSequence
    -- DROP FUNCTION
  | SqlDropTableColumn
  | SqlDropTable
  | SqlDropFunction
    -- SEQUENCE
  | SqlCreateSequence
    -- TABLE
  | SqlCreateTable
  | SqlAddColumn
  | SqlAlterTable
  | SqlDropColumnDefault
  | SqlAlterColumn
    -- ALTER SEQUENCE
  | SqlAlterSequence
    -- FUNCTION
  | SqlDropDomain
  | SqlDropType
  | SqlCreateFunction
  | SqlInherit
  | SqlAddTableConstr
  | SqlCreatePrimaryKeyConstr
  | SqlCreateUniqueConstr
  | SqlCreateForeignKeyConstr
  | SqlCreateCheckConstr
  | SqlAddDefault
    -- TRIGGER
  | SqlCreateTrigger
  | SqlPriv
  | SqlComment
  | SqlUnclassified
  | SqlPostInstall
  deriving (Eq, Ord, Show)

instance ToSqlCode SqlStmt where
  toSqlCode x =
    case stmtBody x of
      Nothing -> ""
      Just y -> y <> ";\n"

toSqlCodeString :: Text -> Text
toSqlCodeString xs = "'" <> T.replace "'" "''" xs <> "'"
