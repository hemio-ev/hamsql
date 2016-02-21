-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

module Utils where

import Control.Monad
import Data.Char
import Data.List        (group, intercalate, sort)
import Debug.Trace
import System.Exit
import System.IO
import System.IO.Unsafe

import Option

join = intercalate

err :: String -> a
err xs = unsafePerformIO $ do
  hPutStrLn stderr ("error: " ++ xs)
  exitWith $ ExitFailure 1

inf = msg "info"
warn = msg "warning"
warn' = msg' "warning"

msg typ xs ys = unsafePerformIO $ do
  msg' typ xs
  return ys

msg' typ xs = hPutStrLn stderr (typ ++ ": " ++ xs)

debug opt
  | optVerbose opt = msg "debug"
  | otherwise = id'
      where
        id' _ = id

info :: OptCommon -> String -> IO ()
info opts xs = when(optVerbose opts) $
    msg' "debug" xs

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

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
upper = map toUpper

fromJustReason :: String -> Maybe a -> a
fromJustReason _ (Just x) = x
fromJustReason reason Nothing = err $ "fromJust failed: " ++ reason

selectUniqueReason :: String -> [a] -> a
selectUniqueReason _ [x] = x
selectUniqueReason msg [] = err $ "No element found while trying to find exactly one: " ++ msg
selectUniqueReason msg xs = err $
  "More then one element (" ++ show (length xs) ++") found while trying to extrac one: " ++ msg

tr x = trace (show x ++ "\n") x

