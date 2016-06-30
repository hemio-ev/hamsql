-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Commons where

import Database.HamSql.Setup
import Database.YamSql
import Database.HamSql.Internal.Sql
--import Database.HamSql

stmtCommentOn :: Text -> SqlName -> Text -> SqlStatement
stmtCommentOn on obj com = SqlStmt SqlComment (SqlName $ toSql obj) $
  "COMMENT ON " <> on <> " " <> toSql obj <> " IS " <> toSqlString com

prefixedRole :: Setup -> SqlName -> Text
prefixedRole setup role = toSql (setupRolePrefix' setup // role)

