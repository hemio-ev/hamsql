-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Function where

import           Data.Maybe
import qualified Data.Text  as T

--import Database.HamSql
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Sql
import Database.HamSql.Internal.Stmt.Commons
import Database.HamSql.Setup
import Database.YamSql

stmtsDeployFunction :: OptCommon -> Setup -> Schema -> Function -> [SqlStatement]
stmtsDeployFunction _ setup m f =
    stmtCreateFunction:
    sqlSetOwner (functionOwner f):
    stmtComment:
    map sqlStmtGrantExecute (maybeList $ functionPrivExecute f)

    where
        name = schemaName m <.> functionName f

        sqlStmtGrantExecute u = SqlStmt SqlPriv name $ sqlGrantExecute u
        sqlGrantExecute u = "GRANT EXECUTE ON FUNCTION \n" <>
            sqlFunctionIdentifier <> "\nTO " <> prefixedRole setup u

        stmtCreateFunction = SqlStmtFunction SqlCreateFunction name
            (maybeMap variableType (functionParameters f))
            $
            "CREATE OR REPLACE FUNCTION " <> sqlFunctionIdentifierDef <>
            "\n" <>
            "RETURNS" <-> toSql (functionReturns f) <> sqlReturnsColumns (functionReturnsColumns f) <>
            "\nLANGUAGE " <> sqlLanguage (functionLanguage f) <>
            "\nSECURITY " <> sqlSecurity (functionSecurityDefiner f) <>
            "\nAS\n$BODY$\n" <>
                sqlBody <>
            "\n$BODY$\n"

        stmtComment = SqlStmt SqlComment (functionName f) $
          "COMMENT ON FUNCTION " <> sqlFunctionIdentifier <>
          " IS " <> toSqlString (functionDescription f)

        sqlSetOwner (Just o) = SqlStmt SqlPriv name $
            "ALTER FUNCTION " <> sqlFunctionIdentifier <>
            "OWNER TO " <> prefixedRole setup o
        sqlSetOwner Nothing = SqlStmtEmpty

        sqlFunctionIdentifierDef =
            toSql name
                <> "(\n" <>
                T.intercalate ",\n" (maybeMap sqlParameterDef (functionParameters f)) <>
                "\n)"

        sqlFunctionIdentifier =
            toSql name
                <> "(" <\>
                T.intercalate ",\n" (maybeMap sqlParameter (functionParameters f)) <\>
                ")"

        -- function parameter
        sqlParameter p =
            toSql(variableName p)
            <-> toSql(variableType p)

        sqlParameterDef p =
            toSql(variableName p)
            <-> toSql(variableType p)
            <-> sqlParamDefault (variableDefault p)
            where
            sqlParamDefault Nothing = ""
            sqlParamDefault (Just x) = "DEFAULT" <-> x

        -- If function returns a table, use service for field definition
        sqlReturnsColumns cs
         | toSql (functionReturns f) == "TABLE" =
            " (" <\>
            T.intercalate ",\n" (maybeMap sqlReturnsColumn cs) <>
            ") "
         | otherwise = ""

        sqlReturnsColumn c = toSql (parameterName c) <> " " <> toSql (parameterType c)

        -- If language not defined, use service for variable definitions
        sqlBody
            | isNothing (functionLanguage f) =
                "DECLARE" <\>
                sqlVariables (functionVariables f) <>
                "BEGIN" <\>
                body <\>
                "END;"
            | otherwise =
                body
            where
                body =
                    T.intercalate "\n" preludes <>
                    maybeText (functionBody f) <>
                    T.intercalate "\n" postludes

                preludes :: [Text]
                preludes = catMaybes$maybeMap functiontplBodyPrelude (functionTemplateData f)

                postludes :: [Text]
                postludes = catMaybes$maybeMap functiontplBodyPostlude (functionTemplateData f)


        -- Service for variable definitions
        sqlVariables Nothing = ""
        sqlVariables (Just vs) = T.concat (map sqlVariable vs)

        sqlVariable v =
            toSql (variableName v) <-> toSql(variableType v)
            <-> sqlVariableDefault (variableDefault v) <> ";\n"

        sqlVariableDefault Nothing = ""
        sqlVariableDefault (Just d) = ":=" <-> d

        -- SECURITY
        sqlSecurity (Just True) = "DEFINER"
        sqlSecurity _           = "INVOKER"

        -- LANGUAGE
        sqlLanguage Nothing     = "plpgsql"
        sqlLanguage (Just lang) = lang

