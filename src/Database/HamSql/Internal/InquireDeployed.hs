-- This file is part of HamSql
--
-- Copyright 2014-2017 by it's authors.
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

-- ** Schemas

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
      types <- deployedTypes schema
      return
        Schema
        { schemaName = schema
        , schemaDescription = description
        , schemaDependencies = Nothing
        , _schemaFunctions = presetEmpty functions
        , schemaFunctionTemplates = Nothing
        , _schemaTables = presetEmpty tables
        , schemaTableTemplates = Nothing
        , schemaRoles = Nothing
        , _schemaSequences = presetEmpty sequences
        , schemaPrivUsage = Nothing
        , schemaPrivSelectAll = Nothing
        , schemaPrivInsertAll = Nothing
        , schemaPrivUpdateAll = Nothing
        , schemaPrivDeleteAll = Nothing
        , schemaPrivSequenceAll = Nothing
        , schemaPrivExecuteAll = Nothing
        , schemaPrivAllAll = Nothing
        , _schemaDomains = presetEmpty domains
        , _schemaTypes = presetEmpty types
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
      ORDER BY nspname
      -- TODO: do public right
    |]

-- ** Tables

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
      checks <- deployedTableChecks (schema, table)
      trs <- deployedTriggers (schema, table)
      return
        Table
        { tableName = table
        , tableDescription = fromMaybe "" description
        , _tableColumns = columns
        , tablePrimaryKey = pk
        , tableUnique = presetEmpty uniques
        , tableForeignKeys = presetEmpty fks
        , tableChecks = presetEmpty checks
        , tableInherits = Nothing
        , tablePrivSelect = Nothing
        , tablePrivInsert = Nothing
        , tablePrivUpdate = Nothing
        , tablePrivDelete = Nothing
        , tableTemplates = Nothing
        , tableTriggers = presetEmpty trs
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
      , _columnType = dataType
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
          pg_catalog.format_type(atttypid, atttypmod),
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
        ORDER BY attname
      |]

deployedTriggers :: (SqlName, SqlName) -> SqlT [Trigger]
deployedTriggers tbl = do
  trs <- psqlQry qry (Only $ toSqlCode tbl)
  return $ map toTrigger trs
  where
    toTrigger (trname, trdesc, trevents, trcond, trorient, trtiming, trcall, cols) =
      Trigger
      { triggerName = trname
      , triggerDescription = fromMaybe "" trdesc
      , triggerMoment = trtiming
      , triggerEvents =
          map (fixUpd (fromPGArray <$> cols)) $ fromPGArray trevents
      , triggerForEach = trorient
      , triggerCondition = trcond
      , triggerExecute =
          fromMaybe trcall $ stripPrefix "EXECUTE PROCEDURE " trcall
      }
    fixUpd :: Maybe [Text] -> Text -> Text
    fixUpd (Just ys) x
      | x == "UPDATE" = x <> " OF " <> intercalate ", " ys
      | otherwise = x
    fixUpd _ x = x
    qry =
      [sql|
      SELECT
        inf.trigger_name,
        pg_catalog.obj_description(tr.oid, 'pg_trigger') AS desc,
        (SELECT
          array_agg(i.event_manipulation::text)
        FROM information_schema.triggers i
        WHERE i.trigger_schema = inf.trigger_schema AND i.trigger_name = inf.trigger_name )
        AS events,
        inf.action_condition,
        inf.action_orientation,
        inf.action_timing,
        inf.action_statement,
        (
          SELECT array_agg(col.attname)
          FROM pg_catalog.pg_attribute col
          WHERE col.attrelid = tr.tgrelid AND col.attnum = ANY(tr.tgattr))
        AS cols
      FROM pg_catalog.pg_trigger tr
      JOIN pg_catalog.pg_class cl ON tr.tgrelid = cl.oid
      JOIN pg_catalog.pg_namespace ns ON ns.oid = cl.relnamespace
      JOIN information_schema.triggers inf
        ON inf.trigger_schema = ns.nspname AND inf.trigger_name = tr.tgname
      WHERE tr.tgrelid = ?::regclass::oid
      GROUP BY 1, 2, 4, 5, 6, 7, 8, inf.trigger_schema;
    |]

deployedTableChecks :: (SqlName, SqlName) -> SqlT [Check]
deployedTableChecks tbl = do
  cons <- psqlQry qry (Only $ toSqlCode tbl)
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
          pg_catalog.obj_description(oid, 'pg_constraint')::text AS condesc,
          consrc
        FROM pg_catalog.pg_constraint
        WHERE
          contype = 'c'
          AND conrelid IS NOT NULL
          AND conrelid = ?::regclass::oid
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

recoverIndexName :: Text -> [Text] -> Text -> Text -> Maybe IndexName
recoverIndexName tbl keys n s =
  case stripPrefix (tbl <> "_") n >>= stripSuffix ("_" <> s) of
    Nothing -> Just IndexNamePrefixed {indexnamePrefixed = SqlName n}
    Just unprefixed
      | unprefixed == intercalate "_" keys -> Nothing
      | otherwise -> Just $ IndexNameUnprefixed (SqlName unprefixed)

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

-- ** Functions

deployedFunctions :: SqlName -> SqlT [Function]
deployedFunctions schema = do
  funs <- psqlQry qry (Only $ toSqlCode schema)
  return $ map toFunction funs
  where
    toFunction (proname, description, prorettype, proargnames, proargtypes, proargdefaults, owner, language, prosecdef, source) =
      Function
      { functionName = proname
      , functionDescription = fromMaybe "" description
      , _functionReturns = prorettype
      , _functionParameters =
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
      , _functionReturnsColumns = Nothing
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
        ORDER BY proname
      |]

toVariable :: SqlType -> Maybe SqlName -> Maybe Text -> Variable
toVariable varType varName varDefault =
  Variable
  { variableName = fromMaybe undefined varName
  , variableDescription = Nothing
  , _variableType = varType
  , variableDefault = varDefault
  }

-- ** Domains

deployedDomains :: SqlName -> SqlT [Domain]
deployedDomains schema = do
  doms <- psqlQry qry (Only $ toSqlCode schema)
  mapM toDomain doms
  where
    toDomain (domname, domdesc, domtype, domdefault) = do
      constraints <- deployedDomainConstraints (schema, domname)
      return
        Domain
        { domainName = domname
        , domainDescription = fromMaybe "" domdesc
        , _domainType = domtype
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
        ORDER BY typname
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

-- ** Sequences

deployedSequences :: SqlName -> SqlT [Sequence]
deployedSequences schema = do
  seqs <- psqlQry qry1 (Only $ toSqlCode schema)
  (map toSequence . head) . sequence <$> mapM doQry2 seqs
  where
    doQry2 (n, desc, ownedby) =
      psqlQry (qry2 (n :: Text)) (desc :: Maybe Text, ownedby :: Maybe Text)
    toSequence (seqname, seqstartvalue, seqincrementby, seqmaxvalue, seqminvalue, seqcachevalue, seqiscycled, seqdesc, ownedby) =
      Sequence
      { sequenceName = seqname
      , sequenceDescription = fromMaybe "" seqdesc
      , sequenceIncrement = preset 1 seqincrementby
      , sequenceMinValue = preset 1 seqminvalue
      , sequenceMaxValue = preset 9223372036854775807 seqmaxvalue
      , sequenceStartValue = preset 1 seqstartvalue
      , sequenceCache = preset 1 seqcachevalue
      , sequenceCycle = preset False seqiscycled
      , sequenceOwnedByColumn = ownedby
      }
    qry1 =
      [sql|
        SELECT
          oid::regclass::text,
          pg_catalog.obj_description(oid, 'pg_class')::text AS seqdesc,
          (
            SELECT d.refobjid::regclass::text || '."' || a.attname || '"'
            FROM pg_depend d
            JOIN pg_catalog.pg_attribute a
              ON a.attrelid = d.refobjid AND a.attnum = d.refobjsubid
            WHERE
              d.classid = 'pg_class'::regclass
              AND d.objid = pg_class.oid
          )
          AS ownedby
        FROM pg_class
        WHERE
          relkind = 'S'
          AND relnamespace = ?::regnamespace::oid
      |]
    qry2 n =
      toQry $
      "SELECT sequence_name, start_value, increment_by, max_value," <\>
      "min_value, cache_value, is_cycled::bool, ?::text AS desc, ?::text AS ownedby FROM " <>
      n

-- ** Types

deployedTypes :: SqlName -> SqlT [Type]
deployedTypes schema = do
  types <- psqlQry qry (Only $ toSqlCode schema)
  mapM toType types
  where
    toType (typname, typdesc) = do
      elements <- map toElement <$> deployedColumns (schema, typname)
      return
        Type
        { typeName = typname
        , typeDescription = fromMaybe "" typdesc
        , _typeElements = elements
        }
    toElement x =
      TypeElement
      {typeelementName = columnName x, _typeelementType = _columnType x}
    qry =
      [sql|
        SELECT
          typname,
          pg_catalog.obj_description(oid, 'pg_type')::text AS desc
          -- typdefault
        FROM pg_type
        WHERE
          typtype = 'c'
          AND typisdefined
          AND (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c
              WHERE c.oid = typrelid)
          AND typnamespace = ?::regnamespace::oid
      |]
