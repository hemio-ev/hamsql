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
import Parser
import Text.Pandoc
import Data.Text.IO
import qualified Data.ByteString.Char8 as B


--setupDocMask :: Template
--setupDocMask = either error id $ compileTemplate
--   "$for(setup.module_data)$# Module $setup.module_data.name$\n$setup.module_data.description$\n\n$endfor$"
--   "This is module '$a.namex$' $for(employee)$Hi, $employee.name.first$. $if(employee.salary)$You make $employee.salary$.$else$No salary data.$endif$$sep$\n$endfor$"

toSetupDoc :: Setup -> IO (Text)
toSetupDoc s = do
  template <- Data.Text.IO.readFile "doc-template.html"
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
        writerTableOfContents=True,
        writerStandalone = True,
        writerTemplate ="<!DOCTYPE html><html><head><meta charset='utf-8' /><link rel='stylesheet' href='s.css' /></head><body>$if(toc)$$toc$$endif$\n$body$</body></html>",
        writerTOCDepth=3
    }) . readHtml def

getDoc d = markdownToRST d
