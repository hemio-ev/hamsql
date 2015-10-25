-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Documentation where

import Data.Text
import Data.Aeson
import Text.Pandoc.Templates
import GHC.Generics
import Text.Pandoc
import Data.Text.IO
import qualified Data.ByteString.Char8 as B
import Text.Regex.Posix

import Option
import Parser

ensureFileExt :: String -> FilePath -> FilePath
ensureFileExt ext xs
  | xs =~ ("\\." ++ ext ++ "$") =
    xs
  | otherwise =
    xs ++ "." ++ ext
  

toSetupDoc :: OptDoc -> Setup -> IO (Text)
toSetupDoc optDoc s = do
  let fileName =  (ensureFileExt "html") $ optTemplate optDoc
  template <- Data.Text.IO.readFile fileName
  let t = either error id $ compileTemplate template
  return $ renderTemplate t $ object [
    "modules" .= (setupModuleData $ setupInternal s),
    "setup" .= s
    ]

markdownToRST :: String -> String
markdownToRST =
   (writeHtmlString def {
        writerHtml5 = True,
        writerSectionDivs = True,
        writerReferenceLinks = True,
        writerTableOfContents=False,
        writerStandalone = True,
        writerTemplate ="<!DOCTYPE html><html><head><meta charset='utf-8' /><link rel='stylesheet' href='s.css' /></head><body>$if(toc)$$toc$$endif$\n$body$</body></html>",
        writerTOCDepth=3
    }) . readHtml def

getDoc d = markdownToRST d

getGraphDoc :: OptDoc -> Setup -> IO (Text)
getGraphDoc optDoc s = do
    let fileName =  (ensureFileExt "dot") $ optTemplate optDoc
    template <- Data.Text.IO.readFile fileName
    let t = either error id $ compileTemplate template
    return $ renderTemplate t $ object [
        "modules" .= (setupModuleData $ setupInternal s),
        "setup" .= s
        ]

