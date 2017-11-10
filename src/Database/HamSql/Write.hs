module Database.HamSql.Write
  ( schemaToDirTree
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
import Database.YamSql

schemaToDirTree :: Schema -> DirTree B.ByteString
schemaToDirTree schema =
  let schemaFile =
        File
          "schema.yml"
          (toYml schema {schemaTables = Nothing, schemaFunctions = Nothing})
  in Dir
       (filePath $ schemaName schema)
       (schemaFile :
        catMaybes
          [ Dir "domains.d" . map (toYamlFile domainName) <$>
            schemaDomains schema
          , Dir "sequences.d" . map (toYamlFile sequenceName) <$>
            schemaSequences schema
          , Dir "types.d" . map (toYamlFile typeName) <$> schemaTypes schema
          , Dir "functions.d" .
            map
              (\x ->
                 toFrontmatterFile
                   functionName
                   (x {functionBody = Nothing})
                   (functionBody x)) <$>
            schemaFunctions schema
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
  setConfCompare (comparing ymlOrd) $ (setConfDropNull True) defConfig

doWrite :: FilePath -> DirTree B.ByteString -> IO (AnchoredDirTree ())
doWrite p x = writeDirectoryWith B.writeFile (p :/ x)

ymlOrd :: Text -> Text
ymlOrd x
  | x == "name" = "00"
  | x == "description" = "05"
  | x == "type" = "10"
  | x == "ref_table" = "ref_00"
  | otherwise = x
