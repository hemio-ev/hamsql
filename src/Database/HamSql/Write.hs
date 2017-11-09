module Database.HamSql.Write
  ( schemaToDirTree
  , toYml
  , doWrite
  ) where

import qualified Data.ByteString as B
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Yaml
import Data.Yaml.Pretty
import System.Directory.Tree

import Database.HamSql.Internal.Utils
import Database.YamSql

schemaToDirTree :: Schema -> DirTree B.ByteString
schemaToDirTree schema =
  Dir
    (filePath $ schemaName schema)
    ([File "schema.yml" (toYml schema {schemaTables = Nothing})] ++
     catMaybes
       [Dir "tables.d" . map (toFile tableName) <$> (schemaTables schema)])
  where
    toFile getName obj = File (filePath $ getName obj) (toYml obj)
    filePath :: SqlName -> FilePath
    filePath = T.unpack . T.replace "\"" "" . unsafePlainName

toYml :: ToJSON a => a -> B.ByteString
toYml =
  encodePretty $
  (setConfCompare $ comparing ymlOrd) $ (setConfDropNull True) defConfig

doWrite :: FilePath -> DirTree B.ByteString -> IO (AnchoredDirTree ())
doWrite p x = writeDirectoryWith B.writeFile (p :/ x)

ymlOrd :: Text -> Text
ymlOrd x
  | x == "name" = "00"
  | x == "description" = "05"
  | x == "type" = "10"
  | x == "ref_table" = "ref_00"
  | otherwise = x
