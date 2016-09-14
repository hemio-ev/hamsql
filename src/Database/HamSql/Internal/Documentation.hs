-- This file is part of HamSql
--
-- Copyright 2014 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.HamSql.Internal.Documentation where

import Data.FileEmbed
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T.IO
import System.FilePath
import Text.Pandoc.Templates

import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Utils
import Database.HamSql.Setup
import Database.YamSql (Schema(..), SqlName(..))

templateFromFile :: FilePath -> IO Template
templateFromFile "DEFAULT.rst" = return templateDefaultSchema
templateFromFile fname = do
  str <- T.IO.readFile fname
  return $ templateCompile str

templateCompile :: Text -> Template
templateCompile str =
  case compileTemplate str of
    (Left e) -> err $ tshow e
    (Right t) -> t

docWrite :: OptDoc -> Setup -> IO ()
docWrite optDoc s = do
  t <- templateFromFile (optTemplate optDoc)
  _ <- mapM (docWriteSchema optDoc t) (maybeList $ setupSchemaData s)
  return ()

docWriteSchema :: OptDoc -> Template -> Schema -> IO ()
docWriteSchema optDoc t m = T.IO.writeFile path (renderTemplate t m)
  where
    path =
      optOutputDir optDoc </> getName (schemaName m) <.>
      takeExtension (optTemplate optDoc)
    getName (SqlName n) = T.unpack n

templateDefaultSchema :: Template
templateDefaultSchema =
  templateCompile $ decodeUtf8 $(embedFile "data/doc-template.rst")
