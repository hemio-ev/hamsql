-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Main where

import Database.HamSql.Cli

main :: IO ()
main = parseArgv >>= run
