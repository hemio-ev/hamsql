-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.InquireDeployed where

import Data.Text (stripPrefix)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (PGArray (..), fromPGArray)

import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.Utils
import Database.HamSql.Setup
import Database.YamSql

sqlManageSchemaJoin :: Text -> Text
sqlManageSchemaJoin schemaid =
  " JOIN pg_namespace AS n " <\> "  ON" <-> schemaid <-> "= n.oid AND " <\>
  "  NOT n.nspname LIKE 'pg_%' AND " <\>
  "  n.nspname NOT IN ('information_schema') "

deployedTableConstrIds :: Connection
                       -> IO [SqlObj SQL_TABLE_CONSTRAINT (SqlName, SqlName, SqlName)]
deployedTableConstrIds conn = map toSqlCodeId <$> query_ conn qry
  where
    toSqlCodeId (schema, table, constraint) =
      SqlObj SQL_TABLE_CONSTRAINT (schema, table, constraint)
    qry =
      toQry $
      "SELECT n.nspname, t.relname, c.conname" <\> "FROM pg_constraint AS c" <\>
      "JOIN pg_class AS t" <\>
      " ON c.conrelid = t.oid" <->
      sqlManageSchemaJoin "c.connamespace"

deployedDomainConstrIds :: Connection
                        -> IO [SqlObj SQL_DOMAIN_CONSTRAINT (SqlName, SqlName)]
deployedDomainConstrIds conn = map toSqlCodeId <$> query_ conn qry
  where
    toSqlCodeId (schema, table, constraint) =
      SqlObj SQL_DOMAIN_CONSTRAINT (schema <.> table, constraint)
    qry =
      toQry $
      "SELECT n.nspname, d.typname, c.conname" <\> "FROM pg_constraint AS c " <\>
      "JOIN pg_type AS d " <\>
      " ON c.contypid = d.oid" <->
      sqlManageSchemaJoin "c.connamespace"

-- | List SEQUENCE
deployedSequenceIds :: Connection -> IO [SqlObj SQL_SEQUENCE SqlName]
deployedSequenceIds conn = map toSqlCodeId <$> query_ conn qry
  where
    toSqlCodeId (s, t) = SqlObj SQL_SEQUENCE (s <.> t)
    qry =
      toQry $
      "SELECT sequence_schema, sequence_name" <\>
      "FROM information_schema.sequences"

-- | List TABLE
deployedTableIds :: Connection -> IO [SqlObj SQL_TABLE SqlName]
deployedTableIds conn = do
  dat <-
    query_ conn $
    toQry $
    "SELECT table_schema, table_name" <\> "FROM information_schema.tables" <\>
    "WHERE table_type = 'BASE TABLE'" <\>
    " AND table_schema NOT IN ('information_schema', 'pg_catalog')"
  return $ map toSqlCodeId dat
  where
    toSqlCodeId (s, t) = SqlObj SQL_TABLE (s <.> t)

-- | List TABLE COLUMN
deployedTableColumnIds :: Connection
                       -> IO [SqlObj SQL_COLUMN (SqlName, SqlName)]
deployedTableColumnIds conn = map toSqlCodeId <$> query_ conn qry
  where
    toSqlCodeId (s, t, u) = SqlObj SQL_COLUMN (s <.> t, u)
    qry =
      toQry $
      "SELECT table_schema, table_name, column_name" <\>
      " FROM information_schema.columns" <\>
      --" WHERE table_type = 'BASE TABLE'" ++
      " WHERE table_schema NOT IN ('information_schema', 'pg_catalog')"

-- | List TYPE
deployedTypeIds :: Connection -> IO [SqlObj SQL_TYPE SqlName]
deployedTypeIds conn = map toSqlCodeId <$> query_ conn qry
  where
    toSqlCodeId (s, t) = SqlObj SQL_TYPE (s <.> t)
    qry =
      toQry $
      "SELECT user_defined_type_schema, user_defined_type_name" <\>
      " FROM information_schema.user_defined_types" <\>
      " WHERE user_defined_type_schema NOT IN ('information_schema', 'pg_catalog')"

-- | List ROLE
deployedRoleIds :: Setup -> Connection -> IO [SqlObj SQL_ROLE SqlName]
deployedRoleIds setup conn = do
  roles <-
    query conn "SELECT rolname FROM pg_roles WHERE rolname LIKE ?" $
    Only $ prefix <> "%"
  return $ map toSqlCodeId roles
  where
    prefix = setupRolePrefix' setup
    unprefixed =
      fromJustReason "Retrived role without prefix from database" .
      stripPrefix prefix
    toSqlCodeId (Only role) = SqlObj SQL_ROLE (SqlName $ unprefixed role)

deployedDomainIds :: Connection -> IO [SqlObj SQL_DOMAIN SqlName]
deployedDomainIds conn = map toSqlCodeId <$> query_ conn qry
  where
    toSqlCodeId (schema, domain) = SqlObj SQL_DOMAIN $ schema <.> domain
    qry =
      toQry $
      "SELECT domain_schema, domain_name" <\> " FROM information_schema.domains" <\>
      " WHERE domain_schema NOT IN ('information_schema', 'pg_catalog')"

deployedFunctionIds :: Connection
                    -> IO [SqlObj SQL_FUNCTION (SqlName, [SqlType])]
deployedFunctionIds conn = map toSqlCodeId <$> query_ conn qry
  where
    toSqlCodeId (schema, function, args) =
      SqlObj SQL_FUNCTION (schema <.> function, fromPGArray args)
    qry =
      toQry $
      "SELECT n.nspname, p.proname, " <>
      -- This part of the query includes a workaround for
      -- <https://github.com/lpsmith/postgresql-simple/issues/166>
      "ARRAY(SELECT UNNEST(p.proargtypes::regtype[]::varchar[]))" <\>
      "FROM pg_proc AS p" <->
      sqlManageSchemaJoin "p.pronamespace" <\>
      "WHERE p.probin IS NULL"
