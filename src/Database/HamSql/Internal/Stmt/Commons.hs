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
  :: ToSqlId a
  => Text -> a -> Text -> SqlStmt
stmtCommentOn on obj com =
  newSqlStmt SqlComment obj $
  "COMMENT ON " <> on <> " " <> sqlIdCode obj <> " IS " <> toSqlCodeString com

prefixedRole :: Setup -> SqlName -> Text
prefixedRole setup role = toSqlCode (setupRolePrefix' setup // role)
