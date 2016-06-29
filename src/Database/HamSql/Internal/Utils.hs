-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.

{-# LANGUAGE OverloadedStrings #-}

module Database.HamSql.Internal.Utils (module Database.HamSql.Internal.Utils, Text (..), (<>)) where

import           Control.Monad
import           Data.Char
import           Data.List        (group, intercalate, sort)
import           Data.Monoid      ((<>))
import           Data.Text        (Text (..), pack)
import qualified Data.Text        as T
import           Data.Text.IO
import           Debug.Trace
import           System.Exit
import           System.IO        (stderr)
import           System.IO.Unsafe

import Database.HamSql.Internal.Option

join = intercalate

err :: Text -> a
err xs = unsafePerformIO $ do
  hPutStrLn stderr ("error: " <> xs)
  exitWith $ ExitFailure 1

inf = msg "info"
warn = msg "warning"
warn' = msg' "warning"

msg typ xs ys = unsafePerformIO $ do
  msg' typ xs
  return ys

msg' typ xs = hPutStrLn stderr (typ <> ": " <> xs)

debug opt
  | optVerbose opt = msg "debug"
  | otherwise = id'
      where
        id' _ = id

info :: OptCommon -> Text -> IO ()
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

maybeText :: Maybe Text -> Text
maybeText Nothing = ""
maybeText (Just text) = ""

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

fromJustReason :: Text -> Maybe a -> a
fromJustReason _ (Just x) = x
fromJustReason reason Nothing = err $ "fromJust failed: " <> reason

selectUniqueReason :: Text -> [a] -> a
selectUniqueReason _ [x] = x
selectUniqueReason msg [] = err $ "No element found while trying to find exactly one: " <> msg
selectUniqueReason msg xs = err $
  "More then one element (" <> tshow (length xs) <> ") found while trying to extrac one: " <> msg

tshow :: (Show a) => a -> Text
tshow = pack . show

tr x = trace (show x <> "\n") x

isIn :: Char -> Text -> Bool
isIn c t = T.singleton c `T.isInfixOf` t

(<->) a b = a <> " " <> b
(<\>) a b = a <> "\n" <> b

