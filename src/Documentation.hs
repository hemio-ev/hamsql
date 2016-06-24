-- This file is part of HamSql
--
-- Copyright 2014 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Documentation where

import qualified Data.Text             as T
import qualified Data.Text.IO          as T.IO
import           Text.Pandoc.Templates
import           System.FilePath

import Option
import Parser
import Parser.Basic (SqlName (..))
import Parser.Module
import Utils

templateFromFile :: FilePath -> IO Template
templateFromFile fname = do
  str <- T.IO.readFile fname
  return $
   case compileTemplate str of
    (Left e) -> err $ tshow e
    (Right t) -> t

docWrite :: OptDoc -> Setup -> IO ()
docWrite optDoc s = do
  t <- templateFromFile (optTemplate optDoc)
  _ <- mapM (docWriteModule optDoc t) (setupModuleData (setupInternal s))
  return ()

docWriteModule :: OptDoc -> Template -> Module -> IO ()
docWriteModule optDoc t m = T.IO.writeFile path (renderTemplate t m)
 where
  path = optOutputDir optDoc
   </> getName (moduleName m)
   <.> takeExtension (optTemplate optDoc)
  getName (SqlName n) = T.unpack n
