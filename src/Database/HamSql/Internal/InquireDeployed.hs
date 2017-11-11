-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.InquireDeployed where

import Data.Text (intercalate, stripPrefix, stripSuffix)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types (PGArray(..), fromPGArray)

import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.Utils
import Database.HamSql.Setup
import Database.YamSql

recoverIndexName :: Text -> [Text] -> Text -> Text -> Maybe IndexName
recoverIndexName tbl keys n s =
  case stripPrefix (tbl <> "_") n >>= stripSuffix ("_" <> s) of
    Nothing -> Just IndexNamePrefixed {indexnamePrefixed = SqlName n}
    Just unprefixed
      | unprefixed == intercalate "_" keys -> Nothing
      | otherwise -> Just $ IndexNameUnprefixed (SqlName unprefixed)

deployedSchemas :: SqlT [Schema]
deployedSchemas = do
  schemas <- psqlQry_ qry
  mapM toSchema schemas
  where
    toSchema (schema, description) = do
      tables <- deployedTables schema
      sequences <- deployedSequences schema
      functions <- deployedFunctions schema
      domains <- deployedDomains schema
      return
        Schema
        { schemaName = schema
        , schemaDescription = description
        , schemaDependencies = Nothing
        , schemaFunctions = presetEmpty functions
        , schemaFunctionTemplates = Nothing
        , schemaTables = presetEmpty tables
        , schemaTableTemplates = Nothing
        , schemaRoles = Nothing
        , schemaSequences = presetEmpty sequences
        , schemaPrivUsage = Nothing
        , schemaPrivSelectAll = Nothing
        , schemaPrivInsertAll = Nothing
        , schemaPrivUpdateAll = Nothing
        , schemaPrivDeleteAll = Nothing
        , schemaPrivSequenceAll = Nothing
        , schemaPrivExecuteAll = Nothing
        , schemaPrivAllAll = Nothing
        , schemaDomains = presetEmpty domains
        , schemaTypes = Nothing
        , schemaExecPostInstall = Nothing
        , schemaExecPostInstallAndUpgrade = Nothing
        }
    qry =
      [sql|
      SELECT
        nspname,
        COALESCE(pg_catalog.obj_description(oid, 'pg_namespace'), '')
      FROM pg_catalog.pg_namespace
      WHERE nspname <> 'information_schema' AND nspname NOT LIKE 'pg\_%'
      -- TODO: do public right
      AND nspname <> 'public'
    |]

deployedTables :: SqlName -> SqlT [Table]
deployedTables schema = do
  tbls <- psqlQry qry (Only $ toSqlCode schema)
  mapM toTable tbls
  where
    toTable (table, description) = do
      columns <- deployedColumns (schema, table)
      pk <- deployedPrimaryKey (schema, table)
      fks <- deployedForeignKeys (schema, table)
      uniques <- deployedUniqueConstraints (schema, table)
      return
        Table
        { tableName = table
        , tableDescription = fromMaybe "" description
        , tableColumns = columns
        , tablePrimaryKey = pk
        , tableUnique = presetEmpty uniques
        , tableForeignKeys = presetEmpty fks
        , tableChecks = Nothing
        , tableInherits = Nothing
        , tablePrivSelect = Nothing
        , tablePrivInsert = Nothing
        , tablePrivUpdate = Nothing
        , tablePrivDelete = Nothing
        , tableTemplates = Nothing
        }
    qry =
      [sql|
        SELECT
          relname,
          pg_catalog.obj_description(oid, 'pg_class') AS desc
        FROM pg_catalog.pg_class
        WHERE
          relkind = 'r'
          AND relnamespace = ?::regnamespace::oid
      |]

deployedColumns :: (SqlName, SqlName) -> SqlT [Column]
deployedColumns tbl = map toColumn <$> psqlQry qry (Only $ toSqlCode tbl)
  where
    toColumn (sname, dataType, columnDefault', isNullable, description) =
      Column
      { columnName = sname
      , columnType = dataType
      , columnDescription = fromMaybe "" description
      , columnDefault = columnDefault'
      , columnNull = preset False isNullable
      , columnReferences = Nothing
      , columnOnRefDelete = Nothing
      , columnOnRefUpdate = Nothing
      , columnUnique = Nothing
      , columnChecks = Nothing
      }
    qry =
      [sql|
        SELECT
          attname,
          atttypid::regtype::text,
          def.adsrc,
          NOT attnotnull,
          pg_catalog.col_description(attrelid, attnum)
        FROM pg_catalog.pg_attribute
        LEFT JOIN pg_catalog.pg_attrdef AS def
          ON attrelid = adrelid AND adnum = attnum
        WHERE
          NOT attisdropped
          AND attnum > 0
          AND attrelid = ?::regclass::oid
      |]

--deployedKeys :: 
deployedPrimaryKey :: (SqlName, SqlName) -> SqlT [SqlName]
deployedPrimaryKey tbl = do
  res <- psqlQry keyQuery (toSqlCode tbl, True, True)
  return $
    case res of
      [] -> []
      (x:_) -> toPrimaryKey x
  where
    toPrimaryKey :: (SqlName, PGArray SqlName) -> [SqlName]
  -- TODO: do not ignore name
    toPrimaryKey (_, keys) = fromPGArray keys

deployedUniqueConstraints ::
     (SqlName, SqlName) -> SqlT [Abbr [SqlName] UniqueConstraint]
deployedUniqueConstraints tbl@(_, table) = do
  res <- psqlQry keyQuery (toSqlCode tbl, True, False)
  return $ map toUniqueConstraint res
  where
    toUniqueConstraint (keyName, keys') =
      let keys = fromPGArray keys'
          idx = recoverIndexName (unsafeInternalName table) keys keyName "key"
      in case idx of
           Nothing -> ShortForm $ map SqlName keys
           index ->
             LongForm
               UniqueConstraint
               { uniqueconstraintName = index
               , uniqueconstraintColumns = map SqlName keys
               }

deployedForeignKeys :: (SqlName, SqlName) -> SqlT [ForeignKey]
deployedForeignKeys tbl@(_, table) = do
  res <- psqlQry qry (Only $ toSqlCode tbl)
  return $ map toForeignKey res
  where
    toForeignKey (keyName, cols', fTbl, fCols') =
      let cols = fromPGArray cols'
          fCols = fromPGArray fCols'
      in ForeignKey
         { foreignkeyName =
             recoverIndexName (unsafeInternalName table) cols keyName "fkey"
         , foreignkeyColumns = map SqlName cols
         , foreignkeyRefTable = SqlName fTbl
         , foreignkeyRefColumns =
             if map SqlName cols == fCols
               then Nothing
               else Just fCols
         , foreignkeyOnDelete = Nothing
         , foreignkeyOnUpdate = Nothing
         }
    qry =
      [sql|
        SELECT
          conname,
          (SELECT array_agg(attname) FROM unnest(conkey) AS id
           JOIN pg_attribute ON attnum=id AND attrelid=conrelid),
          confrelid::regclass::text,
          (SELECT array_agg(attname) FROM unnest(confkey) AS id
           JOIN pg_attribute ON attnum=id AND attrelid=confrelid)
        FROM pg_catalog.pg_constraint
        WHERE contype='f' AND conrelid::regclass = ?::regclass
      |]

-- (tbl, unique, primary)
keyQuery :: Query
keyQuery =
  [sql|
          SELECT
            irel.relname AS index_name,
            array_agg (a.attname ORDER BY c.ordinality) AS columns
          FROM pg_index AS i
          JOIN pg_class AS trel ON trel.oid = i.indrelid
          JOIN pg_namespace AS tnsp ON trel.relnamespace = tnsp.oid
          JOIN pg_class AS irel ON irel.oid = i.indexrelid
          CROSS JOIN LATERAL unnest (i.indkey) WITH ORDINALITY AS c (colnum, ordinality)
          JOIN pg_attribute AS a
            ON trel.oid = a.attrelid AND a.attnum = c.colnum
          WHERE
             trel.oid::regclass = ?::regclass
             AND i.indisunique = ?
             AND i.indisprimary = ?
          GROUP BY tnsp.nspname, trel.relname, irel.relname;      
    |]

toVariable :: SqlType -> Maybe SqlName -> Maybe Text -> Variable
toVariable varType varName varDefault =
  Variable
  { variableName = fromMaybe undefined varName
  , variableDescription = Nothing
  , variableType = varType
  , variableDefault = varDefault
  }

deployedFunctions :: SqlName -> SqlT [Function]
deployedFunctions schema = do
  funs <- psqlQry qry (Only $ toSqlCode schema)
  return $ map toFunction funs
  where
    toFunction (proname, description, prorettype, proargnames, proargtypes, proargdefaults, owner, language, prosecdef, source) =
      Function
      { functionName = proname
      , functionDescription = fromMaybe "" description
      , functionReturns = prorettype
      , functionParameters =
          let n = length $ fromPGArray proargtypes
              def = fromMaybe (replicate n Nothing)
          in presetEmpty $
             zipWith3
               toVariable
               (fromPGArray proargtypes)
               (def $ fromPGArray <$> proargnames)
               (def $ fromPGArray <$> proargdefaults)
      , functionTemplates = Nothing
      , functionTemplateData = Nothing
      , functionReturnsColumns = Nothing
      , functionVariables = Nothing
      , functionPrivExecute = Nothing
      , functionSecurityDefiner = preset False prosecdef
      , functionOwner = owner
      , functionLanguage = Just language
      , functionBody = source
      }
    qry =
      [sql|
        SELECT
          proname,
          pg_catalog.obj_description(p.oid, 'pg_proc')::text AS description,
          prorettype::regtype::text,
          proargnames,
          ARRAY(SELECT UNNEST(proargtypes::regtype[]::text[])),
          ARRAY(SELECT pg_get_function_arg_default(p.oid, n)
            FROM generate_series(1, pronargs) t(n)),
          CASE WHEN proowner<>current_user::regrole
           THEN proowner::regrole::text END,
          lanname,
          prosecdef,
          prosrc
        FROM pg_catalog.pg_proc AS p
        JOIN pg_catalog.pg_language AS l
          ON p.prolang = l.oid
        WHERE pronamespace::regnamespace = ?::regnamespace
      |]

deployedDomains :: SqlName -> SqlT [Domain]
deployedDomains schema = do
  doms <- psqlQry qry (Only $ toSqlCode schema)
  mapM toDomain doms
  where
    toDomain (domname, domdesc, domtype, domdefault) = do
      constraints <- deployedDomainConstraints (schema, domname)
      return $
        Domain
        { domainName = domname
        , domainDescription = fromMaybe "" domdesc
        , domainType = domtype
        , domainDefault = domdefault
        , domainChecks = presetEmpty constraints
        }
    qry =
      [sql|
        SELECT
          typname,
          pg_catalog.obj_description(oid, 'pg_type')::text AS desc,
          pg_catalog.format_type(typbasetype, typtypmod) AS domtype,
          typdefault
        FROM pg_type
        WHERE
          typtype = 'd'
          AND typnamespace = ?::regnamespace::oid
      |]

deployedDomainConstraints :: (SqlName, SqlName) -> SqlT [Check]
deployedDomainConstraints dom = do
  cons <- psqlQry qry (Only $ toSqlCode dom)
  return $ map toCheck cons
  where
    toCheck (coname, codesc, cocheck) =
      Check
      { checkName = coname
      , checkDescription = fromMaybe "" codesc
      , checkCheck = cocheck
      }
    qry =
      [sql|
    SELECT
      conname,
      pg_catalog.obj_description(c.oid, 'pg_constraint')::text AS condesc,
      consrc
    FROM pg_catalog.pg_constraint c
    JOIN pg_type t ON t.oid = contypid
    WHERE
      t.oid = ?::regtype::oid
  |]

deployedSequences :: SqlName -> SqlT [Sequence]
deployedSequences schema = do
  seqs <- psqlQry qry1 (Only $ toSqlCode schema)
  map toSequence <$> head <$> sequence <$> mapM doQry2 seqs
  where
    doQry2 (n, desc) = psqlQry (qry2 (n :: Text)) (Only $ (desc :: Maybe Text))
    toSequence (seqname, seqstartvalue, seqincrementby, seqmaxvalue, seqminvalue, seqcachevalue, seqiscycled, seqdesc) =
      Sequence
      { sequenceName = seqname
      , sequenceDescription = fromMaybe "" seqdesc
      , sequenceIncrement = preset 1 seqincrementby
      , sequenceMinValue = preset 1 seqminvalue
      , sequenceMaxValue = preset 9223372036854775807 seqmaxvalue
      , sequenceStartValue = preset 1 seqstartvalue
      , sequenceCache = preset 1 seqcachevalue
      , sequenceCycle = preset False seqiscycled
      , sequenceOwnedByColumn = Nothing
      }
    qry1 =
      [sql|
        SELECT
          oid::regclass::text,
          pg_catalog.obj_description(oid, 'pg_class')::text AS seqdesc
        FROM pg_class
        WHERE
          relkind = 'S'
          AND relnamespace = ?::regnamespace::oid
      |]
    qry2 n =
      toQry $
      "SELECT sequence_name, start_value, increment_by, max_value," <\>
      "min_value, cache_value, is_cycled::bool, ?::text AS desc FROM " <>
      n

sqlManageSchemaJoin :: Text -> Text
sqlManageSchemaJoin schemaid =
  " JOIN pg_namespace AS n " <\> "  ON" <-> schemaid <-> "= n.oid AND " <\>
  "  NOT n.nspname LIKE 'pg_%' AND " <\>
  "  n.nspname NOT IN ('information_schema') "

deployedTableConstrIds ::
     Connection -> IO [SqlObj SQL_TABLE_CONSTRAINT (SqlName, SqlName, SqlName)]
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

deployedDomainConstrIds ::
     Connection -> IO [SqlObj SQL_DOMAIN_CONSTRAINT (SqlName, SqlName)]
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

-- | List SCHEMA
deployedSchemaIds :: Connection -> IO [SqlObj SQL_SCHEMA SqlName]
deployedSchemaIds conn = map toSqlCodeId <$> query_ conn qry
  where
    toSqlCodeId (Only s) = SqlObj SQL_SCHEMA s
    qry =
      toQry $
      "SELECT s.nspname FROM pg_namespace AS s" <\> sqlManageSchemaJoin "s.oid"

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
deployedTableColumnIds ::
     Connection -> IO [SqlObj SQL_COLUMN (SqlName, SqlName)]
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
deployedRoleIds setup conn =
  map toSqlCodeId <$> query conn qry (Only $ prefix <> "%")
  where
    qry = "SELECT rolname FROM pg_roles WHERE rolname LIKE ?"
    prefix = setupRolePrefix' setup
    unprefixed =
      fromJustReason "Retrived role without prefix from database" .
      stripPrefix prefix
    toSqlCodeId (Only role) = SqlObj SQL_ROLE (SqlName $ unprefixed role)

deployedRoleMemberIds ::
     Setup -> Connection -> IO [SqlObj SQL_ROLE_MEMBERSHIP (SqlName, SqlName)]
deployedRoleMemberIds setup conn =
  map toSqlCodeId <$> query conn qry (prefix <> "%", prefix <> "%")
  where
    prefix = setupRolePrefix' setup
    unprefixed =
      fromJustReason "Retrived role without prefix from database" .
      stripPrefix prefix
    toSqlCodeId (role, member) =
      SqlObj
        SQL_ROLE_MEMBERSHIP
        (SqlName $ unprefixed role, SqlName $ unprefixed member)
    qry =
      toQry $
      "SELECT a.rolname, b.rolname FROM pg_catalog.pg_auth_members AS m" <\>
      " INNER JOIN pg_catalog.pg_roles AS a ON a.oid=m.roleid" <\>
      " INNER JOIN pg_catalog.pg_roles AS b ON b.oid=m.member" <\>
      "WHERE a.rolname LIKE ? AND b.rolname LIKE ?"

-- | List DOMAIN
deployedDomainIds :: Connection -> IO [SqlObj SQL_DOMAIN SqlName]
deployedDomainIds conn = map toSqlCodeId <$> query_ conn qry
  where
    toSqlCodeId (schema, domain) = SqlObj SQL_DOMAIN $ schema <.> domain
    qry =
      toQry $
      "SELECT domain_schema, domain_name" <\> " FROM information_schema.domains" <\>
      " WHERE domain_schema NOT IN ('information_schema', 'pg_catalog')"

deployedFunctionIds ::
     Connection -> IO [SqlObj SQL_FUNCTION (SqlName, [SqlType])]
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
