-- This file is part of HamSql
--
-- Copyright 2014-2017 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.InquireDeployed where

import Data.List (zipWith4)
import Data.Text (intercalate, singleton, stripPrefix, stripSuffix)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types (PGArray(..), fromPGArray)

import Database.HamSql.Internal.DbUtils
import Database.HamSql.Internal.Utils
import Database.HamSql.Setup
import Database.YamSql

-- * Database
inquireSetup ::
     Maybe Text -- ^ Role prefix
  -> SqlT Setup
inquireSetup rolePrefix = do
  schemas <- deployedSchemas
  roles <- inquireRoles rolePrefix
  return
    Setup
    { setupSchemas = []
    , setupSchemaDirs = Nothing
    , setupRolePrefix = rolePrefix
    , setupPreCode = Nothing
    , setupPostCode = Nothing
    , _setupSchemaData = Just schemas
    , setupRoles = presetEmpty roles
    }

-- ** Roles
inquireRoles :: Maybe Text -> SqlT [Role]
inquireRoles prfx = do
  roles <- psqlQry qry (Only $ prfx' <> "%")
  mapM toRole roles
  where
    prfx' = fromMaybe "" prfx
    unprefixed x = SqlName $ fromMaybe x (stripPrefix prfx' x)
    toRole (rname, description, rolcanlogin, memberin) =
      return
        Role
        { roleName = unprefixed rname
        , roleDescription = fromMaybe "" description
        , roleLogin = preset False rolcanlogin
        , rolePassword = Nothing
        , roleMemberIn = presetEmpty $ map unprefixed $ fromPGArray memberin
        }
    qry =
      [sql|
      SELECT
        r.rolname,
        pg_catalog.shobj_description(r.oid, 'pg_authid') AS desc,
        r.rolcanlogin,
        ARRAY(
          SELECT b.rolname
          FROM pg_catalog.pg_auth_members m
          JOIN pg_catalog.pg_roles b ON m.roleid = b.oid
          WHERE m.member = r.oid) as memberof
      FROM pg_catalog.pg_roles r
      WHERE rolname LIKE ?
      ORDER BY rolname
    |]

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

-- *** Tables
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
        ORDER BY relname
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
        ORDER BY attnum
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

-- *** Functions
deployedFunctions :: SqlName -> SqlT [Function]
deployedFunctions schema = do
  funs <- psqlQry qry (Only $ toSqlCode schema)
  return $ map toFunction funs
  where
    toFunction ((proname, description, prorettype, proretset, owner, language, prosecdef, source) :. args) =
      Function
      { functionName = proname
      , functionDescription = fromMaybe "" description
      , _functionReturns = rettype prorettype proretset args
      , _functionParameters =
          presetEmpty $ filter ((/= Just "t") . variableMode) $ params args
      , functionTemplates = Nothing
      , functionTemplateData = Nothing
      , functionVariables = Nothing
      , functionPrivExecute = Nothing
      , functionSecurityDefiner = preset False prosecdef
      , functionOwner = owner
      , functionLanguage = Just language
      , functionBody = source
      }
    params (proargnames, proargtypes, proargmodes, proargdefaults) =
      let n = length $ fromPGArray proargtypes
          def v (Just xs) = xs ++ (replicate (n - length xs) v)
          def v Nothing = replicate n v
      in zipWith4
           toVariable
           (fromPGArray proargtypes)
           (def Nothing $ fromPGArray <$> proargnames)
           (def Nothing $ fromPGArray <$> proargdefaults)
           (def 'i' $ fromPGArray <$> proargmodes)
    rettype prorettype proretset args =
      let tParams = filter ((== Just "t") . variableMode) $ params args
      in if null tParams
           then if proretset
                  then ReturnTypeSetof prorettype
                  else ReturnType prorettype
           else ReturnTypeTable $ map variableToParameter tParams
    qry =
      [sql|
        SELECT
          proname,
          pg_catalog.obj_description(p.oid, 'pg_proc')::text AS description,
          prorettype::regtype::text,
          proretset,
          CASE WHEN proowner<>current_user::regrole
           THEN proowner::regrole::text END,
          lanname,
          prosecdef,
          prosrc,
          -- function arguments
          proargnames,
          COALESCE(
            proallargtypes::regtype[]::text[],
            ARRAY(SELECT UNNEST(proargtypes::regtype[]::text[]))
          ) AS argtypes,
          proargmodes,
          ARRAY(SELECT pg_get_function_arg_default(p.oid, n)
            FROM generate_series(1, pronargs) t(n)) AS argdefault
        FROM pg_catalog.pg_proc AS p
        JOIN pg_catalog.pg_language AS l
          ON p.prolang = l.oid
        -- for check if functions belongs to an extension
        LEFT JOIN pg_depend AS d
          ON d.objid = p.oid AND d.deptype = 'e'
        WHERE
          pronamespace::regnamespace = ?::regnamespace
          -- exclude all originating from extensions
          AND d.objid IS NULL
        ORDER BY proname
      |]

variableToParameter :: Variable -> Parameter
variableToParameter v =
  Parameter
  { parameterName = variableName v
  , _parameterType = _variableType v
  , parameterDescription = Nothing
  }

toVariable :: SqlType -> Maybe SqlName -> Maybe Text -> Char -> Variable
toVariable varType varName varDefault varMode =
  Variable
  { variableName = fromMaybe undefined varName
  , variableDescription = Nothing
  , _variableType = varType
  , variableDefault = varDefault
  , variableMode = preset "IN" $ toMode varMode
  }
  where
    toMode 'i' = "IN"
    toMode 'o' = "OUT"
    toMode 'b' = "INOUT"
    toMode 'v' = "VARIADIC"
    toMode x = singleton x

-- *** Domains
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

-- *** Sequences
deployedSequences :: SqlName -> SqlT [Sequence]
deployedSequences schema = do
  seqs <- psqlQry qry (Only $ toSqlCode schema)
  return $ map toSequence seqs
  where
    toSequence (seqname, seqstartvalue, seqincrementby, seqmaxvalue, seqminvalue, seqcachevalue, seqiscycled, seqdesc, ownedby) =
      let positive = seqincrementby > 0
      in Sequence
         { sequenceName = seqname
         , sequenceDescription = fromMaybe "" seqdesc
         , sequenceIncrement = preset 1 seqincrementby
         , sequenceMinValue =
             preset
               (if positive
                  then 1
                  else -9223372036854775807)
               seqminvalue
         , sequenceMaxValue =
             preset
               (if positive
                  then 9223372036854775807
                  else -1)
               seqmaxvalue
         , sequenceStartValue =
             preset
               (if positive
                  then seqminvalue
                  else seqmaxvalue)
               seqstartvalue
         , sequenceCache = preset 1 seqcachevalue
         , sequenceCycle = preset False seqiscycled
         , sequenceOwnedByColumn = ownedby
         }
    qry =
      [sql|
        SELECT
          c.relname,
          s.seqstart,
          s.seqincrement,
          s.seqmax,
          s.seqmin,
          s.seqcache,
          s.seqcycle,
          pg_catalog.obj_description(c.oid, 'pg_class')::text AS seqdesc,
          (
            SELECT d.refobjid::regclass::text || '."' || a.attname || '"'
            FROM pg_depend d
            JOIN pg_catalog.pg_attribute a
              ON a.attrelid = d.refobjid AND a.attnum = d.refobjsubid
            WHERE
              d.classid = 'pg_class'::regclass
              AND d.objid = c.oid
          )
          AS ownedby
        FROM pg_sequence s
        LEFT JOIN pg_class c 
          ON c.oid = s.seqrelid
        WHERE
          c.relnamespace = ?::regnamespace::oid
      |]

-- *** Types
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
