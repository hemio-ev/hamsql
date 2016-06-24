-- This file is part of HamSql
--
-- Copyright 2014 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Documentation where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import     qualified      Data.Text as T
import        qualified   Data.Text.IO as T.IO
import           GHC.Generics
import Text.Pandoc.Templates

import Option
import Parser
import Parser.Module
import Parser.Basic
import Utils
import Sql

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
  _ <- sequence $ map (docWriteModule t) $ setupModuleData (setupInternal s)
  return ()

docWriteModule :: Template -> Module -> IO ()
docWriteModule t m = do
  error $ renderTemplate t m

