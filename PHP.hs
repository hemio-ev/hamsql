-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

module PHP where

import Options
import Parser
import Utils

import Data.Maybe
import Data.String.Utils

data PhpCode = PhpCode {
    phpCode :: String
} deriving Show

-- Setup

getSetupPhpClasses :: Opt -> Setup -> String
getSetupPhpClasses opts s = concat (map getStr (map (getModulePhpClass opts) (setupModuleData $ setupInternal s)))
  where
    getStr x = (phpCode x)
-- Module

getModulePhpClass :: Opt -> Module -> PhpCode
getModulePhpClass opts m =
    PhpCode
    (classPrelude ++
    (concat $ maybeMap (getFunctionPhpCode opts) (moduleFunctions m)) ++
    classPostlude)

    where
        classPrelude =
            "namespace " ++ toSql (moduleName m) ++ ";\n\n" ++
            "class DbApi extends \\edentata\\DbApi {\n"
        classPostlude =
            "}"

-- Function

getFunctionPhpCode :: Opt -> Function -> String
getFunctionPhpCode opts f = code

    where
    code =
        sep ++ "\nfunction " ++ str (functionName f) ++ "(" ++
        (join ", ") (maybeMap phpDefParam (functionParameters f)) ++
        ") {\n" ++
        sep ++ sep ++ "$this->call('" ++ str (functionName f) ++ ", [" ++
        (join ", ") (maybeMap phpArrParam (functionParameters f)) ++
        "]);\n" ++
        sep ++ "}\n"
    
    def Nothing = ""
    def _       = " = null"
    phpDefParam v = "$" ++ str (variableName v) ++ def (variableDefault v)
    phpArrParam v = "'" ++ str (variableName v) ++ "' => $" ++ str (variableName v)
    
    sep = "    "

    str (SqlName xs) = xs

