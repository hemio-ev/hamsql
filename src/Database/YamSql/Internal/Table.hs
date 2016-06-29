{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Database.YamSql.Internal.Table where

import Data.Aeson          (Object (..), Value (..))
import Data.Data
import Data.HashMap.Strict (insert, keys, member)
import Data.Typeable
import GHC.Generics

import Database.YamSql.Internal.Basic
import Database.YamSql.Internal.Check
import Utils

data Table = Table {
  tableName         :: SqlName,
  tableDescription  :: Text,
  tableColumns      :: [Column],
  tablePrimaryKey   :: [SqlName],
  tableUnique       :: Maybe [UniqueKey],
  tableForeignKeys  :: Maybe [ForeignKey],
  tableChecks       :: Maybe [Check],
  tableInherits     :: Maybe [SqlName],
  tablePrivSelect   :: Maybe [SqlName],
  tablePrivInsert   :: Maybe [SqlName],
  tablePrivUpdate   :: Maybe [SqlName],
  tablePrivDelete   :: Maybe [SqlName],
  tableTemplates    :: Maybe [SqlName],
  tableTemplateData :: Maybe [TableTpl]
} deriving (Data, Generic, Show, Typeable)
instance FromJSON Table where parseJSON = strictParseYaml
instance ToJSON Table where toJSON = genericToJSON myOpt

data TableTpl = TableTpl {
    tabletplTemplate    :: SqlName,
    tabletplDescription :: Text,
    tabletplForeignKeys :: Maybe [ForeignKey],
    tabletplInherits    :: Maybe [SqlName],
    tabletplColumns     :: Maybe [Column],
    tabletplChecks      :: Maybe [Check],
    tabletplPrivSelect  :: Maybe [SqlName],
    tabletplPrivInsert  :: Maybe [SqlName],
    tabletplPrivUpdate  :: Maybe [SqlName],
    tabletplPrivDelete  :: Maybe [SqlName]
} deriving (Generic, Show, Typeable, Data)
instance FromJSON TableTpl where parseJSON = strictParseYaml
instance ToJSON TableTpl where toJSON = genericToJSON myOpt

data Column = Column {
    columnName        :: SqlName,
    columnType        :: SqlType,
    columnDescription :: Text,
    columnDefault     :: Maybe Text,
    columnNull        :: Maybe Bool,
    columnReferences  :: Maybe SqlName,
    columnOnRefDelete :: Maybe Text,
    columnOnRefUpdate :: Maybe Text,
    columnUnique      :: Maybe Bool,
    columnChecks      :: Maybe [Check]
} | ColumnTpl {
    columntplTemplate     :: SqlName,
    columntplTemplateData :: Maybe TableColumnTpl,
    columntplName         :: Maybe SqlName,
    columntplType         :: Maybe SqlType,
    columntplDescription  :: Maybe Text,
    columntplDefault      :: Maybe Text,
    columntplNull         :: Maybe Bool,
    columntplReferences   :: Maybe SqlName,
    columntplOnRefDelete  :: Maybe Text,
    columntplOnRefUpdate  :: Maybe Text,
    columntplUnique       :: Maybe Bool,
    columntplChecks       :: Maybe [Check]
} deriving (Generic, Show, Typeable, Data)
instance FromJSON Column where parseJSON = strictParseYaml.addColumnDefaultTag
instance ToJSON Column where toJSON = genericToJSON myOpt

addColumnDefaultTag :: Value -> Value
addColumnDefaultTag (Object o) = Object $
 if member "tag" o then
  o
 else
  if member "template" o then
   insert "tag" "column_tpl" o
  else
   insert "tag" "column" o

data TableColumnTpl = TableColumnTpl {
    tablecolumntplTemplate    :: SqlName,
    tablecolumntplName        :: SqlName,
    tablecolumntplType        :: SqlType,
    tablecolumntplDescription :: Text,
    tablecolumntplDefault     :: Maybe Text,
    tablecolumntplNull        :: Maybe Bool,
    tablecolumntplReferences  :: Maybe SqlName,
    tablecolumntplOnRefDelete :: Maybe Text,
    tablecolumntplOnRefUpdate :: Maybe Text,
    tablecolumntplUnique      :: Maybe Bool,
    tablecolumntplChecks      :: Maybe [Check]
} deriving (Generic, Show, Typeable, Data)
instance FromJSON TableColumnTpl where parseJSON = strictParseYaml
instance ToJSON TableColumnTpl where toJSON = genericToJSON myOpt

applyTableTpl :: TableTpl -> Table -> Table
applyTableTpl tpl t = t {
    tableColumns     = maybeList (tabletplColumns tpl) ++ tableColumns t,
    tableForeignKeys = maybeJoin (tabletplForeignKeys tpl) (tableForeignKeys t),
    tableInherits    = maybeJoin (tabletplInherits tpl) (tableInherits t),
    tableChecks      = maybeJoin (tabletplChecks tpl) (tableChecks t),
    tablePrivSelect  = maybeJoin (tabletplPrivSelect tpl) (tablePrivSelect t),
    tablePrivInsert  = maybeJoin (tabletplPrivInsert tpl) (tablePrivInsert t),
    tablePrivUpdate  = maybeJoin (tabletplPrivUpdate tpl) (tablePrivUpdate t),
    tablePrivDelete  = maybeJoin (tabletplPrivDelete tpl) (tablePrivDelete t)
  }

applyColumnTpl :: TableColumnTpl -> Column -> Column
applyColumnTpl tmp c = Column {
    columnName        = maybeRight' (tablecolumntplName tmp) (columntplName c),
    columnType        = maybeRight' (tablecolumntplType tmp) (columntplType c),
    columnDescription = maybeRight' (tablecolumntplDescription tmp) (columntplDescription c),
    columnDefault     = maybeRight (tablecolumntplDefault tmp) (columntplDefault c),
    columnNull        = maybeRight (tablecolumntplNull tmp) (columntplNull c),
    columnReferences  = maybeRight (tablecolumntplReferences tmp) (columntplReferences c),
    columnOnRefDelete = maybeRight (tablecolumntplOnRefDelete tmp) (columntplOnRefDelete c),
    columnOnRefUpdate = maybeRight (tablecolumntplOnRefUpdate tmp) (columntplOnRefUpdate c),
    columnUnique      = maybeRight (tablecolumntplUnique tmp) (columntplUnique c),
    columnChecks      = maybeRight (tablecolumntplChecks tmp) (columntplChecks c)
  }
  where
    maybeRight' :: a -> Maybe a -> a
    maybeRight' x Nothing = x
    maybeRight' _ (Just y) = y

data UniqueKey = UniqueKey {
  uniquekeyName    :: SqlName,
  uniquekeyColumns :: [SqlName]
} deriving (Generic, Show, Typeable, Data)
instance FromJSON UniqueKey where parseJSON = strictParseYaml
instance ToJSON UniqueKey where toJSON = genericToJSON myOpt

data ForeignKey = ForeignKey {
  foreignkeyName       :: SqlName,
  foreignkeyColumns    :: [SqlName],
  foreignkeyRefTable   :: SqlName,
  foreignkeyRefColumns :: [SqlName],
  foreignkeyOnDelete   :: Maybe Text,
  foreignkeyOnUpdate   :: Maybe Text
} deriving (Generic, Show, Typeable, Data)
instance FromJSON ForeignKey where parseJSON = strictParseYaml
instance ToJSON ForeignKey where toJSON = genericToJSON myOpt

