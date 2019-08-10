module Database.YamSql.Internal.Obj.Table where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Commons
import Database.YamSql.Internal.Obj.Check
import Database.YamSql.Internal.Obj.Trigger

data Table =
  Table
    { tableName :: SqlName
    , tableDescription :: Text
    , _tableColumns :: [Column]
    , tablePrimaryKey :: [SqlName]
    , tableUnique :: Maybe [Abbr [SqlName] UniqueConstraint]
    , tableForeignKeys :: Maybe [ForeignKey]
    , tableChecks :: Maybe [Check]
    , tableInherits :: Maybe [SqlName]
    , tablePrivSelect :: Maybe [SqlName]
    , tablePrivInsert :: Maybe [SqlName]
    , tablePrivUpdate :: Maybe [SqlName]
    , tablePrivDelete :: Maybe [SqlName]
    , tableTriggers :: Maybe [Trigger]
    , tableTemplates :: Maybe [SqlName]
    }
  deriving (Data, Generic, Show)

instance FromJSON Table where
  parseJSON = parseYamSql

instance ToJSON Table where
  toJSON = toYamSqlJson

data SQL_TABLE =
  SQL_TABLE
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_TABLE where
  toSqlCode = const "TABLE"

instance (ToJSON a, ToJSON b) => ToJSON (Abbr a b) where
  toJSON = toYamSqlJson

data TableTpl =
  TableTpl
    { tabletplTemplate :: SqlName
    , tabletplDescription :: Text
    , tabletplForeignKeys :: Maybe [ForeignKey]
    , tabletplInherits :: Maybe [SqlName]
    , tabletplColumns :: Maybe [Column]
    , tabletplChecks :: Maybe [Check]
    , tabletplPrivSelect :: Maybe [SqlName]
    , tabletplPrivInsert :: Maybe [SqlName]
    , tabletplPrivUpdate :: Maybe [SqlName]
    , tabletplPrivDelete :: Maybe [SqlName]
    }
  deriving (Generic, Show, Data)

instance FromJSON TableTpl where
  parseJSON = parseYamSql

instance ToJSON TableTpl where
  toJSON = toYamSqlJson

data Column =
  Column
    { columnName :: SqlName
    , _columnType :: SqlType
    , columnDescription :: Text
    , columnDefault :: Maybe Text
    , columnNull :: Maybe Bool
    , columnReferences :: Maybe SqlName
    , columnOnRefDelete :: Maybe Text
    , columnOnRefUpdate :: Maybe Text
    , columnUnique :: Maybe Bool
    , columnChecks :: Maybe [Check]
    }
  deriving (Generic, Show, Data)

instance FromJSON Column where
  parseJSON = parseYamSql

instance ToJSON Column where
  toJSON = toYamSqlJson

data SQL_COLUMN =
  SQL_COLUMN
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_COLUMN where
  toSqlCode = const "COLUMN"

applyTableTpl :: TableTpl -> Table -> Table
applyTableTpl tpl t =
  t
    { _tableColumns = fromMaybe [] (tabletplColumns tpl) <> _tableColumns t
    , tableForeignKeys = tabletplForeignKeys tpl <> tableForeignKeys t
    , tableInherits = tabletplInherits tpl <> tableInherits t
    , tableChecks = tabletplChecks tpl <> tableChecks t
    , tablePrivSelect = tabletplPrivSelect tpl <> tablePrivSelect t
    , tablePrivInsert = tabletplPrivInsert tpl <> tablePrivInsert t
    , tablePrivUpdate = tabletplPrivUpdate tpl <> tablePrivUpdate t
    , tablePrivDelete = tabletplPrivDelete tpl <> tablePrivDelete t
    }

data IndexName
  = IndexNameUnprefixed SqlName
  | IndexNamePrefixed
      { indexnamePrefixed :: SqlName
      }
  deriving (Generic, Show, Data)

instance FromJSON IndexName where
  parseJSON = parseYamSql

instance ToJSON IndexName where
  toJSON = toYamSqlJson

data UniqueConstraint =
  UniqueConstraint
    { uniqueconstraintName :: Maybe IndexName
    , uniqueconstraintColumns :: [SqlName]
    }
  deriving (Generic, Show, Data)

instance FromJSON UniqueConstraint where
  parseJSON = parseYamSql

instance ToJSON UniqueConstraint where
  toJSON = toYamSqlJson

data ForeignKey =
  ForeignKey
    { foreignkeyName :: Maybe IndexName
    , foreignkeyColumns :: [SqlName]
    , foreignkeyRefTable :: SqlName
    , foreignkeyRefColumns :: Maybe [SqlName]
    , foreignkeyOnDelete :: Maybe Text
    , foreignkeyOnUpdate :: Maybe Text
    }
  deriving (Generic, Show, Data)

instance FromJSON ForeignKey where
  parseJSON = parseYamSql

instance ToJSON ForeignKey where
  toJSON = toYamSqlJson

data SQL_TABLE_CONSTRAINT =
  SQL_TABLE_CONSTRAINT
  deriving (SqlObjType, Show)

instance ToSqlCode SQL_TABLE_CONSTRAINT where
  toSqlCode = const "COLUMN"

makeLenses ''Table

makeLenses ''Column
