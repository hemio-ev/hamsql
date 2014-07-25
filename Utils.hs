-- This file is part of HamSql
--
-- Copyright 2014 by it's authors. 
-- Some rights reserved. See COPYING, AUTHORS.

module Utils where

import System.Exit
import System.IO
import Data.Char
import Debug.Trace


-- err :: [Char] -> IO a
-- err msg = do putStrLn $ "error: "++msg
--              exitWith $ ExitFailure 1

err xs = do
  hPutStrLn stderr ("error: " ++ xs)
  exitWith $ ExitFailure 1
             
--- Maybe Utils

-- Makes list out of Maybe
maybeList :: Maybe [a] -> [a]
maybeList Nothing = []
maybeList (Just xs) = xs

-- Joins two Maybe lists
maybeJoin :: Maybe [a] -> Maybe [a] -> Maybe [a]
maybeJoin Nothing Nothing = Nothing
maybeJoin xs ys = Just (maybeList xs ++ maybeList ys)

-- Joins two Maybe lists
maybeLeftJoin :: Maybe [a] -> [a] -> Maybe [a]
maybeLeftJoin Nothing ys = Just ys
maybeLeftJoin xs ys = Just (maybeList xs ++ ys)

-- Takes the right value, if Just there
maybeRight :: Maybe a -> Maybe a -> Maybe a
maybeRight _ (Just r) = Just r
maybeRight l _        = l

appendToMaybe :: Maybe [a] -> a -> Maybe [a]
appendToMaybe Nothing x = Just [x]
appendToMaybe (Just xs) x = Just (xs ++ [x])

maybeMap :: (a -> b) -> Maybe [a] -> [b]
maybeMap _ Nothing = []
maybeMap f (Just xs) = map f xs

upper :: String -> String
upper xs = map toUpper xs

fromJustReason :: String -> Maybe a -> a
fromJustReason _ (Just x) = x
fromJustReason reason Nothing = error $ "fromJust failed: " ++ reason

tr x = trace (show x) x
