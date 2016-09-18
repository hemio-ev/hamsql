{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
  parseJSON = strictParseYaml

instance ToJSON Table where
  toJSON = genericToJSON myOpt

instance ToSqlIdPart Table where
  sqlIdPart = tableName
  sqlIdPartType _ = "TABLE"

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
  parseJSON = strictParseYaml

instance ToJSON TableTpl where
  toJSON = genericToJSON myOpt

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
  parseJSON = strictParseYaml

instance ToJSON Column where
  toJSON = genericToJSON myOpt

instance ToSqlIdPart Column where
  sqlIdPart = columnName
  sqlIdPartType _ = "COLUMN"

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
  parseJSON = strictParseYaml

instance ToJSON UniqueKey where
  toJSON = genericToJSON myOpt

data ForeignKey = ForeignKey
  { foreignkeyName :: SqlName
  , foreignkeyColumns :: [SqlName]
  , foreignkeyRefTable :: SqlName
  , foreignkeyRefColumns :: [SqlName]
  , foreignkeyOnDelete :: Maybe Text
  , foreignkeyOnUpdate :: Maybe Text
  } deriving (Generic, Show, Data)

instance FromJSON ForeignKey where
  parseJSON = strictParseYaml

instance ToJSON ForeignKey where
  toJSON = genericToJSON myOpt
