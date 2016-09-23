{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.YamSql.Internal.Table where

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Check

data Table = Table
  { tableName :: SqlName
  , tableDescription :: Text
  , tableColumns :: [Column]
  , tablePrimaryKey :: [SqlName]
  , tableUnique :: Maybe [UniqueKey]
  , tableForeignKeys :: Maybe [ForeignKey]
  , tableChecks :: Maybe [Check]
  , tableInherits :: Maybe [SqlName]
  , tablePrivSelect :: Maybe [SqlName]
  , tablePrivInsert :: Maybe [SqlName]
  , tablePrivUpdate :: Maybe [SqlName]
  , tablePrivDelete :: Maybe [SqlName]
  , tableTemplates :: Maybe [SqlName]
  , tableTemplateData :: Maybe [TableTpl]
  } deriving (Data, Generic, Show)

instance FromJSON Table where
  parseJSON = parseYamSql

instance ToJSON Table where
  toJSON = toYamSqlJson

instance ToSqlIdPart Table where
  sqlIdPart = tableName
  sqlIdPartType = const "TABLE"

data TableTpl = TableTpl
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
  } deriving (Generic, Show, Data)

instance FromJSON TableTpl where
  parseJSON = parseYamSql

instance ToJSON TableTpl where
  toJSON = toYamSqlJson

data Column = Column
  { columnName :: SqlName
  , columnType :: SqlType
  , columnDescription :: Text
  , columnDefault :: Maybe Text
  , columnNull :: Maybe Bool
  , columnReferences :: Maybe SqlName
  , columnOnRefDelete :: Maybe Text
  , columnOnRefUpdate :: Maybe Text
  , columnUnique :: Maybe Bool
  , columnChecks :: Maybe [Check]
  } deriving (Generic, Show, Data)

instance FromJSON Column where
  parseJSON = parseYamSql

instance ToJSON Column where
  toJSON = toYamSqlJson

instance ToSqlIdPart Column where
  sqlIdPart = columnName
  sqlIdPartType = const "COLUMN"

applyTableTpl :: TableTpl -> Table -> Table
applyTableTpl tpl t =
  t
  { tableColumns = maybeList (tabletplColumns tpl) ++ tableColumns t
  , tableForeignKeys = maybeJoin (tabletplForeignKeys tpl) (tableForeignKeys t)
  , tableInherits = maybeJoin (tabletplInherits tpl) (tableInherits t)
  , tableChecks = maybeJoin (tabletplChecks tpl) (tableChecks t)
  , tablePrivSelect = maybeJoin (tabletplPrivSelect tpl) (tablePrivSelect t)
  , tablePrivInsert = maybeJoin (tabletplPrivInsert tpl) (tablePrivInsert t)
  , tablePrivUpdate = maybeJoin (tabletplPrivUpdate tpl) (tablePrivUpdate t)
  , tablePrivDelete = maybeJoin (tabletplPrivDelete tpl) (tablePrivDelete t)
  }

data UniqueKey = UniqueKey
  { uniquekeyName :: SqlName
  , uniquekeyColumns :: [SqlName]
  } deriving (Generic, Show, Data)

instance FromJSON UniqueKey where
  parseJSON = parseYamSql

instance ToJSON UniqueKey where
  toJSON = toYamSqlJson

data ForeignKey = ForeignKey
  { foreignkeyName :: SqlName
  , foreignkeyColumns :: [SqlName]
  , foreignkeyRefTable :: SqlName
  , foreignkeyRefColumns :: [SqlName]
  , foreignkeyOnDelete :: Maybe Text
  , foreignkeyOnUpdate :: Maybe Text
  } deriving (Generic, Show, Data)

instance FromJSON ForeignKey where
  parseJSON = parseYamSql

instance ToJSON ForeignKey where
  toJSON = toYamSqlJson
