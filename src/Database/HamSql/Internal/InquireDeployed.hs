-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.InquireDeployed where

import Data.Text                        (stripPrefix)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (PGArray (..), fromPGArray)

import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.Utils
import Database.YamSql

sqlManageSchemaJoin :: Text -> Text
sqlManageSchemaJoin schemaid =
  " JOIN pg_namespace AS n " <\> "  ON" <-> schemaid <-> "= n.oid AND " <\>
  "  NOT n.nspname LIKE 'pg_%' AND " <\>
  "  n.nspname NOT IN ('information_schema') "

deployedTableConstrIds :: Connection -> IO [SqlIdContentSqoObj]
deployedTableConstrIds conn = do
  tableConstrs <-
    query_ conn $
    toQry $
    "SELECT n.nspname, t.relname, c.conname" <\> "FROM pg_constraint AS c" <\>
    "JOIN pg_class AS t" <\>
    " ON c.conrelid = t.oid" <->
    sqlManageSchemaJoin "c.connamespace"
  return $ map toSqlCodeId tableConstrs
  where
    toSqlCodeId (schema, table, constraint) =
      SqlIdContentSqoObj "TABLE-CONSTRAINT" (schema <.> table) constraint

-- DROP DOMAIN CONSTRAINT
deployedDomainConstrIds :: Connection -> IO [SqlIdContentSqoObj]
deployedDomainConstrIds conn = do
  domainConstrs <-
    query_ conn $
    toQry $
    "SELECT n.nspname, d.typname, c.conname" <\> "FROM pg_constraint AS c " <\>
    "JOIN pg_type AS d " <\>
    " ON c.contypid = d.oid" <->
    sqlManageSchemaJoin "c.connamespace"
  return $ map toSqlCodeId domainConstrs
  where
    toSqlCodeId (schema, table, constraint) =
      SqlIdContentSqoObj "DOMAIN-CONSTRAINT" (schema <.> table) constraint

-- List SEQUENCE
deployedSequenceIds :: Connection -> IO [SqlIdContentSqo]
deployedSequenceIds conn = do
  dat <-
    query_
      conn
      "SELECT sequence_schema, sequence_name FROM information_schema.sequences"
  return $ map toSqlCodeId dat
  where
    toSqlCodeId (s, t) = SqlIdContentSqo "SEQUENCE" $ s <.> t

-- List TABLE
deployedTableIds :: Connection -> IO [SqlIdContentSqo]
deployedTableIds conn = do
  dat <-
    query_ conn $
    toQry $
    "SELECT table_schema, table_name" <\> "FROM information_schema.tables" <\>
    "WHERE table_type = 'BASE TABLE'" <\>
    " AND table_schema NOT IN ('information_schema', 'pg_catalog')"
  return $ map toSqlCodeId dat
  where
    toSqlCodeId (s, t) = SqlIdContentSqo "TABLE" $ s <.> t

-- List TABLE COLUMN
deployedTableColumnIds :: Connection -> IO [SqlIdContentSqoObj]
deployedTableColumnIds conn = do
  dat <-
    query_ conn $
    toQry $
    "SELECT table_schema, table_name, column_name" <\>
    " FROM information_schema.columns" <\>
    --" WHERE table_type = 'BASE TABLE'" ++
    " WHERE table_schema NOT IN ('information_schema', 'pg_catalog')"
  return $ map toSqlCodeId dat
  where
    toSqlCodeId (s, t, u) = SqlIdContentSqoObj "TABLE-COLUMN" (s <.> t) u

deployedTypeIds :: Connection -> IO [SqlIdContentSqo]
deployedTypeIds conn = do
  types <-
    query_ conn $
    toQry $
    "SELECT user_defined_type_schema, user_defined_type_name" <\>
    " FROM information_schema.user_defined_types" <\>
    " WHERE user_defined_type_schema NOT IN ('information_schema', 'pg_catalog')"
  return $ map toSqlCodeId types
  where
    toSqlCodeId (schema, t) = SqlIdContentSqo "TYPE" $ schema <.> t

-- ids for all roles on the server prefixed with `prefix`
-- TODO: Fix this
deployedRoleIds :: Connection -> Text -> IO [SqlIdContentObj]
deployedRoleIds conn prefix = do
  roles <-
    query conn "SELECT rolname FROM pg_roles WHERE rolname LIKE ?" $
    Only $ prefix <> "%"
  return $ map toSqlCodeId roles
  where
    unprefixed =
      fromJustReason "Retrived role without prefix from database" .
      stripPrefix prefix
    toSqlCodeId (Only role) = SqlIdContentObj "ROLE" (SqlName $ unprefixed role)

-- TODO: REMOVE PREFIX HERE
deployedDomainIds :: Connection -> IO [SqlIdContentSqo]
deployedDomainIds conn = do
  domains <-
    query_ conn $
    toQry $
    "SELECT domain_schema, domain_name" <\> " FROM information_schema.domains" <\>
    " WHERE domain_schema NOT IN ('information_schema', 'pg_catalog')"
  return $ map toSqlCodeId domains
  where
    toSqlCodeId (schema, domain) = SqlIdContentSqo "DOMAIN" $ schema <.> domain

deployedFunctionIds :: Connection -> IO [SqlIdContentSqoArgtypes]
deployedFunctionIds conn = do
  result <-
    query_ conn $
    toQry $
    "SELECT n.nspname, p.proname, " <>
    -- This part of the query includes a workaround for
    -- <https://github.com/lpsmith/postgresql-simple/issues/166>
    "ARRAY(SELECT UNNEST(p.proargtypes::regtype[]::varchar[]))" <\>
    "FROM pg_proc AS p" <->
    sqlManageSchemaJoin "p.pronamespace" <\>
    "WHERE p.probin IS NULL"
  return $ map toSqlCodeId result
  where
    toSqlCodeId (schema, function, args) =
      SqlIdContentSqoArgtypes
        "FUNCTION"
        (schema <.> function)
        (fromPGArray args)
