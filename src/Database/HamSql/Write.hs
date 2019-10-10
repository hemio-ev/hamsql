module Database.HamSql.Write
  ( schemaToDirTree
  , setupToDirTree
  , toYml
  , doWrite
  ) where

import qualified Data.ByteString as B
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml
import Data.Yaml.Pretty
import System.Directory.Tree

import Database.HamSql.Internal.Utils
import Database.HamSql.Setup
import Database.YamSql

setupToDirTree :: FilePath -> Setup -> DirTree B.ByteString
setupToDirTree d s =
  let setupFile = File "setup.yml" (toYml s {_setupSchemaData = Nothing})
   in Dir
        d
        (setupFile : (map schemaToDirTree $ fromMaybe [] $ _setupSchemaData s))

schemaToDirTree :: Schema -> DirTree B.ByteString
schemaToDirTree schema =
  let schemaFile =
        File
          "schema.yml"
          (toYml schema {_schemaTables = Nothing, _schemaFunctions = Nothing})
   in Dir
        (filePath $ schemaName schema)
        (schemaFile :
         catMaybes
           [ Dir "domains.d" . map (toYamlFile domainName) <$>
             _schemaDomains schema
           , Dir "sequences.d" . map (toYamlFile sequenceName) <$>
             _schemaSequences schema
           , Dir "types.d" . map (toYamlFile typeName) <$> _schemaTypes schema
           , Dir "tables.d" . map (toYamlFile tableName) <$>
             _schemaTables schema
           , Dir "functions.d" .
             map
               (\x ->
                  toFrontmatterFile
                    functionName
                    (x {functionBody = Nothing})
                    (functionBody x)) <$>
             _schemaFunctions schema
           ])
  where
    toYamlFile getName obj = File (filePath (getName obj) <> ".yml") (toYml obj)
    toFrontmatterFile getName obj src =
      File
        (filePath (getName obj) <> ".sql")
        ("---\n" <> toYml obj <> "---\n" <> encodeUtf8 (fromMaybe "" src))
    filePath :: SqlName -> FilePath
    filePath = T.unpack . T.replace "\"" "" . unsafePlainName

toYml :: ToJSON a => a -> B.ByteString
toYml =
  encodePretty $
  setConfCompare (comparing ymlOrd) $ setConfDropNull True defConfig

doWrite :: FilePath -> DirTree B.ByteString -> IO (AnchoredDirTree ())
doWrite p x = writeDirectoryWith B.writeFile (p :/ x)

ymlOrd :: Text -> Text
ymlOrd x
  | x == "name" = "00"
  | x == "description" = "05"
  | x == "type" = "10"
  | x == "ref_table" = "ref_00"
  | otherwise = x
