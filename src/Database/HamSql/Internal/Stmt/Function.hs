-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
{-# LANGUAGE FlexibleInstances #-}

module Database.HamSql.Internal.Stmt.Function where

import Data.Maybe
import qualified Data.Text as T

import Database.HamSql.Internal.Stmt.Basic

stmtsDropFunction' :: SqlId -> [SqlStmt]
stmtsDropFunction' x =
  catMaybes [newSqlStmt SqlDropFunction x $ "DROP FUNCTION " <> toSqlCode x]

stmtsDropFunction :: SqlObj SQL_FUNCTION (SqlName, [SqlType]) -> [Maybe SqlStmt]
stmtsDropFunction x = map Just $ stmtsDropFunction' $ sqlId x

instance ToSqlStmts (SqlContext (Schema, Function)) where
  toSqlStmts SetupContext {setupContextSetup = setup} obj@(SqlContext (s, f)) =
    stmtCreateFunction :
    sqlSetOwner (functionOwner f) :
    stmtComment : maybeMap sqlStmtGrantExecute (functionPrivExecute f)
  --name = schemaName m <.> functionName f
    where
      sqlStmtGrantExecute u = newSqlStmt SqlPriv obj $ sqlGrantExecute u
      sqlGrantExecute u =
        "GRANT EXECUTE ON FUNCTION \n" <>
        sqlIdCode obj <> "\nTO " <> prefixedRole setup u
      stmtCreateFunction =
        newSqlStmt SqlCreateFunction obj $
        --(maybeMap _variableType (_functionParameters f)) $
        "CREATE OR REPLACE FUNCTION " <>
        sqlFunctionIdentifierDef <>
        "\n" <>
        "RETURNS" <-> sqlReturns (_functionReturns f) <>
        "\nLANGUAGE " <>
        sqlLanguage (functionLanguage f) <>
        "\nSECURITY " <>
        sqlSecurity (functionSecurityDefiner f) <>
        "\nAS\n$BODY$" <> sqlBody <> "$BODY$\n"
      stmtComment = stmtCommentOn obj $ functionDescription f
      sqlSetOwner (Just o) =
        newSqlStmt SqlPriv obj $
        "ALTER FUNCTION " <>
        sqlIdCode obj <> "OWNER TO " <> prefixedRole setup o
      sqlSetOwner Nothing = Nothing
      sqlFunctionIdentifierDef =
        toSqlCode (schemaName s <.> functionName f) <>
        "(\n" <>
        T.intercalate ",\n" (maybeMap sqlParameterDef (_functionParameters f)) <>
        "\n)"
      -- function parameter
      sqlParameterDef p =
        toSqlCode (variableName p) <-> toSqlCode (_variableType p) <->
        sqlParamDefault (variableDefault p)
        where
          sqlParamDefault Nothing = ""
          sqlParamDefault (Just x) = "DEFAULT" <-> x
      -- If function returns a table, use service for field definition
      sqlReturns (ReturnType rt) = toSqlCode rt
      sqlReturns (ReturnTypeSetof rt) = "SETOF" <-> toSqlCode rt
      sqlReturns (ReturnTypeTable cs) =
        "TABLE (" <\> T.intercalate ",\n" (map sqlReturnsColumn cs) <> ") "
      sqlReturnsColumn c =
        toSqlCode (parameterName c) <> " " <> toSqlCode (_parameterType c)
      -- If language not defined, use service for variable definitions
      sqlBody
        | isNothing (functionLanguage f) =
          "DECLARE" <\> sqlVariables (functionVariables f) <>
          "BEGIN" <\> body <\> "END;"
        | otherwise = body
        where
          body =
            T.intercalate "\n" preludes <>
            fromMaybe "" (functionBody f) <> T.intercalate "\n" postludes
          preludes :: [Text]
          preludes =
            catMaybes $ maybeMap functiontplBodyPrelude (functionTemplateData f)
          postludes :: [Text]
          postludes =
            catMaybes $
            maybeMap functiontplBodyPostlude (functionTemplateData f)
      -- Service for variable definitions
      sqlVariables Nothing = ""
      sqlVariables (Just vs) = T.concat (map sqlVariable vs)
      sqlVariable v =
        toSqlCode (variableName v) <-> toSqlCode (_variableType v) <->
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
