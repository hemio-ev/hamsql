-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.Stmt.Commons where

import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Utils
import Database.HamSql.Setup
import Database.YamSql

stmtCommentOn
  :: (ToSqlId a)
  => a -> Text -> Maybe SqlStmt
stmtCommentOn obj comment =
  newSqlStmt SqlComment obj $
  "COMMENT ON " <> rewriteType (sqlIdType (sqlId obj)) <> " " <> sqlIdCode obj <>
  " IS " <>
  toSqlCodeString comment
  where
    rewriteType "TABLE-COLUMN" = "COLUMN"
    rewriteType xs = xs

prefixedRole :: Setup -> SqlName -> Text
prefixedRole setup role = toSqlCode ((SqlName $ setupRolePrefix' setup) // role)
