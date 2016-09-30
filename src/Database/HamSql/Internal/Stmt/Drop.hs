-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.Stmt.Drop where

import Database.HamSql.Internal.Stmt.Basic

-- TABLE
stmtsDropTable :: SqlObj SQL_TABLE SqlName -> [Maybe SqlStmt]
stmtsDropTable t = [newSqlStmt SqlDropTable t $ "DROP TABLE " <> toSqlCode t]

-- TYPE
stmtsDropType :: SqlObj SQL_TYPE SqlName -> [Maybe SqlStmt]
stmtsDropType t = [newSqlStmt SqlDropType t $ "DROP TYPE " <> toSqlCode t]
