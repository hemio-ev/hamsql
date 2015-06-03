{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Table where

import Data.Aeson.Types
import GHC.Generics
import Data.Data
import Data.Typeable
import Data.HashMap.Strict (member,insert,keys)

import Parser.Basic
import Utils
import Parser.Check

data Table = Table {
  tableName         :: SqlName,
  tableDescription  :: String,
  tableColumns      :: [Column],
  tablePrimaryKey   :: [SqlName],
  tableUnique       :: Maybe [[SqlName]],
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
    tabletplDescription :: String,
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
    columnName            :: SqlName,
    columnType            :: SqlType,
    columnDescription     :: String,
    columnDefault         :: Maybe String,
    columnNull            :: Maybe Bool,
    columnReferences      :: Maybe SqlName,
    columnOnRefDelete     :: Maybe String,
    columnOnRefUpdate     :: Maybe String,
    columnUnique          :: Maybe Bool,
    columnChecks          :: Maybe [Check]
} | ColumnTpl {
    columntplTemplate     :: SqlName,
    columntplTemplateData :: Maybe TableColumnTpl,
    columntplName         :: Maybe SqlName,
    columntplType         :: Maybe SqlType,
    columntplDescription  :: Maybe String,
    columntplDefault      :: Maybe String,
    columntplNull         :: Maybe Bool,
    columntplReferences   :: Maybe SqlName,
    columntplOnRefDelete  :: Maybe String,
    columntplOnRefUpdate  :: Maybe String,
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
    tablecolumntplTemplate     :: SqlName,
    tablecolumntplName         :: SqlName,
    tablecolumntplType         :: SqlType,
    tablecolumntplDescription  :: String,
    tablecolumntplDefault      :: Maybe String,
    tablecolumntplNull         :: Maybe Bool,
    tablecolumntplReferences   :: Maybe SqlName,
    tablecolumntplOnRefDelete  :: Maybe String,
    tablecolumntplOnRefUpdate  :: Maybe String,
    tablecolumntplUnique       :: Maybe Bool,
    tablecolumntplChecks       :: Maybe [Check]
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
      
data ForeignKey = ForeignKey {
  foreignkeyName        :: SqlName,
  foreignkeyColumns :: [SqlName],
  foreignkeyRefTable    :: SqlName,
  foreignkeyRefColumns  :: [SqlName],
  foreignkeyOnDelete    :: Maybe String,
  foreignkeyOnUpdate    :: Maybe String
} deriving (Generic, Show, Typeable, Data)
instance FromJSON ForeignKey where parseJSON = strictParseYaml
instance ToJSON ForeignKey where toJSON = genericToJSON myOpt

