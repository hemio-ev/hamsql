-- This file is part of HamSql
--
-- Copyright 2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.Trigger where

import qualified Data.Text as T

import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Sql
import Database.HamSql.Internal.Stmt.Function
import Database.HamSql.Setup
import Database.YamSql


createTrigger :: OptCommon -> Setup -> Schema -> Trigger -> [SqlStatement]
createTrigger optCom setup s t =
    getFunctionStatements optCom setup s triggerFunction ++
    map triggerStmt (triggerTables t)

    where

        triggerFunction = Function {
            functionName            = triggerName t,
            functionDescription     = triggerDescription t,
            functionReturns         = SqlType "trigger",
            functionParameters      = Nothing,
            functionTemplates       = Nothing,
            functionTemplateData    = Nothing,
            functionReturnsColumns  = Nothing,
            functionVariables       = triggerVariables t,
            -- TODO: trigger owner?
            functionPrivExecute     = Just [],
            functionSecurityDefiner = Just True,
            -- TODO: trigger owner?
            functionOwner           = Nothing,
            functionLanguage        = triggerLanguage t,
            functionBody            = triggerBody t
        }

        triggerStmt tbl = SqlStmt SqlCreateTrigger name $
            "CREATE TRIGGER " <> toSql (triggerName t) <> " " <> triggerMoment t <> " " <>
            T.intercalate " OR " (triggerEvents t) <>
            " ON " <> toSql tbl <>
            " FOR EACH " <> triggerForEach t <>
            condition (triggerCondition t) <>
            " EXECUTE PROCEDURE " <> toSql name <> "()"

        condition Nothing = ""
        condition (Just x) = " WHEN " <> x <> " "

        name = schemaName s <.> triggerName t


