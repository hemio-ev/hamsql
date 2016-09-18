-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Function where

import Data.Maybe
import qualified Data.Text as T

--import Database.HamSql
import Database.HamSql.Internal.Stmt
import Database.HamSql.Internal.Stmt.Commons
import Database.HamSql.Setup
import Database.YamSql

stmtsDropFunction' :: SqlId -> [SqlStmt]
stmtsDropFunction' x =
  [newSqlStmt SqlDropFunction x $ "DROP FUNCTION " <> toSqlCode x]

stmtsDropFunction :: SqlIdContentSqoArgtypes -> [SqlStmt]
stmtsDropFunction x = stmtsDropFunction' $ sqlId x

instance ToSqlStmts (SqlContextSqoArgtypes Function) where
  toSqlStmts = stmtsDeployFunction

stmtsDeployFunction :: SetupContext
                    -> SqlContextSqoArgtypes Function
                    -> [SqlStmt]
stmtsDeployFunction SetupContext {setupContextSetup = setup} obj@SqlContextSqoArgtypes {sqlSqoArgtypesObject = f} =
  stmtCreateFunction :
  sqlSetOwner (functionOwner f) :
  stmtComment : map sqlStmtGrantExecute (maybeList $ functionPrivExecute f)
--name = schemaName m <.> functionName f
  where
    sqlStmtGrantExecute u = newSqlStmt SqlPriv obj $ sqlGrantExecute u
    sqlGrantExecute u =
      "GRANT EXECUTE ON FUNCTION \n" <> sqlIdCode obj <> "\nTO " <>
      prefixedRole setup u
    stmtCreateFunction =
      newSqlStmt SqlCreateFunction obj $
      --(maybeMap variableType (functionParameters f)) $
      "CREATE OR REPLACE FUNCTION " <> sqlFunctionIdentifierDef <> "\n" <>
      "RETURNS" <->
      toSqlCode (functionReturns f) <>
      sqlReturnsColumns (functionReturnsColumns f) <>
      "\nLANGUAGE " <>
      sqlLanguage (functionLanguage f) <>
      "\nSECURITY " <>
      sqlSecurity (functionSecurityDefiner f) <>
      "\nAS\n$BODY$\n" <>
      sqlBody <>
      "\n$BODY$\n"
    stmtComment =
      stmtCommentOn "FUNCTION" obj $ toSqlCodeString (functionDescription f)
    sqlSetOwner (Just o) =
      newSqlStmt SqlPriv obj $
      "ALTER FUNCTION " <> sqlIdCode obj <> "OWNER TO " <> prefixedRole setup o
    sqlSetOwner Nothing = SqlStmtEmpty
    sqlFunctionIdentifierDef =
      (toSqlCode . sqlIdNameOnly) obj <> "(\n" <>
      T.intercalate ",\n" (maybeMap sqlParameterDef (functionParameters f)) <>
      "\n)"
    -- function parameter
    sqlParameterDef p =
      toSqlCode (variableName p) <-> toSqlCode (variableType p) <->
      sqlParamDefault (variableDefault p)
      where
        sqlParamDefault Nothing = ""
        sqlParamDefault (Just x) = "DEFAULT" <-> x
    -- If function returns a table, use service for field definition
    sqlReturnsColumns cs
      | toSqlCode (functionReturns f) == "TABLE" =
        " (" <\> T.intercalate ",\n" (maybeMap sqlReturnsColumn cs) <> ") "
      | otherwise = ""
    sqlReturnsColumn c =
      toSqlCode (parameterName c) <> " " <> toSqlCode (parameterType c)
    -- If language not defined, use service for variable definitions
    sqlBody
      | isNothing (functionLanguage f) =
        "DECLARE" <\> sqlVariables (functionVariables f) <> "BEGIN" <\> body <\>
        "END;"
      | otherwise = body
      where
        body =
          T.intercalate "\n" preludes <> maybeText (functionBody f) <>
          T.intercalate "\n" postludes
        preludes :: [Text]
        preludes =
          catMaybes $maybeMap functiontplBodyPrelude (functionTemplateData f)
        postludes :: [Text]
        postludes =
          catMaybes $maybeMap functiontplBodyPostlude (functionTemplateData f)
    -- Service for variable definitions
    sqlVariables Nothing = ""
    sqlVariables (Just vs) = T.concat (map sqlVariable vs)
    sqlVariable v =
      toSqlCode (variableName v) <-> toSqlCode (variableType v) <->
      sqlVariableDefault (variableDefault v) <>
      ";\n"
    sqlVariableDefault Nothing = ""
    sqlVariableDefault (Just d) = ":=" <-> d
    -- SECURITY
    sqlSecurity (Just True) = "DEFINER"
    sqlSecurity _ = "INVOKER"
    -- LANGUAGE
    sqlLanguage Nothing = "plpgsql"
    sqlLanguage (Just lang) = lang
