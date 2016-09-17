-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Drop where

import qualified Data.Text as T

import Database.HamSql.Internal.Stmt
import Database.YamSql

--import Database.HamSql
-- ROLE
stmtsDropRole :: SqlIdContentObj -> [SqlStmt]
stmtsDropRole role =
  [newSqlStmt SqlDropRole role $ "DROP ROLE " <> toSqlCode role]

-- TABLE
stmtsDropTable :: SqlIdContentSqo -> [SqlStmt]
stmtsDropTable t = [newSqlStmt SqlDropTable t $ "DROP TABLE " <> toSqlCode t]

-- TYPE
stmtsDropType :: SqlIdContentSqo -> [SqlStmt]
stmtsDropType t = [newSqlStmt SqlDropType t $ "DROP TYPE " <> toSqlCode t]
