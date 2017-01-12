-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE GADTs #-}

module Database.HamSql.Internal.Stmt where

import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromField

import Database.HamSql.Internal.Utils
import Database.YamSql

data SqlStmtId = SqlStmtId
  { stmtType  :: SqlStmtType
  , stmtSqlId :: SqlId
  } deriving (Eq, Ord)

instance Show SqlStmtId where
  show (SqlStmtId x y) = "(SqlStmtId " ++ show x ++ " " ++ show y ++ ")"

data SqlStmt =
  SqlStmt SqlStmtId
          Text
  deriving (Show)

stmtId :: SqlStmt -> SqlStmtId
stmtId (SqlStmt x _) = x

stmtBody :: SqlStmt -> Text
stmtBody (SqlStmt _ x) = x

stmtIdType :: SqlStmt -> SqlStmtType
stmtIdType (SqlStmt x _) = stmtType x

stmtDesc :: SqlStmt -> Text
stmtDesc stmt = sqlIdShowType (sqlId stmt) <-> sqlIdCode stmt

instance Eq SqlStmt where
  x == y = stmtId x == stmtId y

instance Ord SqlStmt where
  x `compare` y = stmtId x `compare` stmtId y

instance ToSqlId SqlStmt where
  sqlId = stmtSqlId . stmtId

newSqlStmtId
  :: (ToSqlId a)
  => SqlStmtType -> a -> SqlStmtId
newSqlStmtId x y = SqlStmtId x (sqlId y)

newSqlStmt
  :: (ToSqlId a)
  => SqlStmtType -> a -> Text -> Maybe SqlStmt
newSqlStmt t o b = Just $ SqlStmt (newSqlStmtId t o) b

sqlPrinter :: [SqlStmt] -> Text
sqlPrinter xs = T.concat $ map toSqlCode xs

instance FromField SqlType where
  fromField x y = SqlType <$> fromField x y

instance FromField SqlName where
  fromField x y = SqlName <$> fromField x y

-- | More like always perform unfiltered after delete
allowInUpgrade :: SqlStmt -> Bool
allowInUpgrade x =
  case stmtIdType x of
    SqlPreInstall -> False
    SqlPostInstall -> False
    _ -> True

stmtRequiresPermitDeletion :: SqlStmt -> Bool
stmtRequiresPermitDeletion x =
  case stmtIdType x of
    SqlDropDatabase -> True
    SqlDropTable -> True
    SqlDropTableColumn -> True
    _ -> False

data SqlStmtType
  = SqlDropDatabase
  | SqlCreateDatabase
  | SqlPre
  | SqlPreInstall
  | SqlRevokePrivilege
  | SqlRevokeMembership
  | SqlDropRole
  | SqlCreateRole
  | SqlAlterRole
  | SqlGrantMembership
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
  -- DEFAULT erlier then NULL
  | SqlColumnSetDefault
  | SqlColumnSetNull
  | SqlColumnSetType
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
  | SqlDomainSetDefault
    -- TRIGGER
  | SqlCreateTrigger
  | SqlPriv
  | SqlComment
  | SqlUnclassified
  | SqlPostInstallAndUpgrade
  | SqlPostInstall
  deriving (Eq, Ord, Show)

instance ToSqlCode SqlStmt where
  toSqlCode x = stmtBody x <> ";\n"

toSqlCodeString :: Text -> Text
toSqlCodeString xs = "'" <> T.replace "'" "''" xs <> "'"
