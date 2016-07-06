-- This file is part of HamSql
--
-- Copyright 2015-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Stmt.CreateSequence where

--import Database.HamSql
import Database.HamSql.Internal.Option
import Database.HamSql.Internal.Sql
import Database.HamSql.Setup
import Database.YamSql

(+++) a b = a <> " " <> b

createSequence :: OptCommon -> Setup -> Schema -> Sequence -> [SqlStatement]
createSequence _ _ m s =

    [
    SqlStmt SqlCreateSequence name $
        "CREATE SEQUENCE" +++ toSql name
    ,
    SqlStmt SqlAlterSequence name $
        "ALTER SEQUENCE" +++ toSql name +++
        incrementBy (sequenceIncrement s) +++
        minValue (sequenceMinValue s) +++
        maxValue (sequenceMaxValue s) +++
        startValue (sequenceStartValue s) +++
        cache (sequenceCache s) +++
        cycle (sequenceCycle s) +++
        ownedByColumn (sequenceOwnedByColumn s)
    ]

    where
        incrementBy Nothing = "INCREMENT BY 1"
        incrementBy (Just i) = "INCREMENT BY " <> tshow i

        minValue Nothing = "NO MINVALUE"
        minValue (Just i) = "MINVALUE " <> tshow i

        maxValue Nothing = "NO MAXVALUE"
        maxValue (Just i) = "MAXVALUE " <> tshow i

        startValue Nothing = ""
        startValue (Just i) = "START WITH " <> tshow i

        cache Nothing = "CACHE 1"
        cache (Just i) = "CACHE " <> tshow i

        cycle Nothing = "NO CYCLE"
        cycle (Just False) = "NO CYCLE"
        cycle (Just True) = "CYCLE"

        ownedByColumn Nothing = "OWNED BY NONE"
        ownedByColumn (Just n) = "OWNED BY " <> toSql n

        name = schemaName m <.> sequenceName s


