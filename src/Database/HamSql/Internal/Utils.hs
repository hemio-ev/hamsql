-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.HamSql.Internal.Utils
  ( module Data.Maybe
  , module Database.HamSql.Internal.Utils
  , Text
  , (<>)
  ) where

import Data.List (group, intercalate, sort)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import System.Exit
import System.IO (stderr)
import System.IO.Unsafe
import Text.Groom

import Database.HamSql.Internal.Option

join :: [a] -> [[a]] -> [a]
join = intercalate

err :: Text -> a
err xs =
  unsafePerformIO $
  do TIO.hPutStrLn stderr ("error: " <> xs)
     exitWith $ ExitFailure 1

warn :: Text -> a -> a
warn = msg "warning"

warn' :: Text -> IO ()
warn' = msg' "warning"

msg :: Text -> Text -> a -> a
msg typ xs ys =
  unsafePerformIO $
  do msg' typ xs
     return ys

msg' :: Text -> Text -> IO ()
msg' typ xs = TIO.hPutStrLn stderr (typ <> ": " <> xs)

info :: OptCommon -> Text -> a -> a
info opts xs
  | optVerbose opts = msg "info" xs
  | otherwise = id

debug :: OptCommon -> Text -> a -> a
debug opts xs
  | optDebug opts = msg "debug" xs
  | otherwise = id

removeDuplicates
  :: (Ord a)
  => [a] -> [a]
removeDuplicates = map head . group . sort

--- Maybe Utils
maybeMap :: (a -> b) -> Maybe [a] -> [b]
maybeMap f = maybe [] (map f)

maybePrefix :: Text -> Maybe Text -> Text
maybePrefix _ Nothing = ""
maybePrefix p (Just x) = p <> x

-- | Joins two Maybe lists
maybeJoin :: Maybe [a] -> Maybe [a] -> Maybe [a]
maybeJoin Nothing Nothing = Nothing
maybeJoin xs ys = Just (fromMaybe [] xs ++ fromMaybe [] ys)

-- | Takes the right value, if Just there
maybeRight :: Maybe a -> Maybe a -> Maybe a
maybeRight _ (Just r) = Just r
maybeRight l _ = l

fromJustReason :: Text -> Maybe a -> a
fromJustReason _ (Just x) = x
fromJustReason reason Nothing = err $ "fromJust failed: " <> reason

selectUniqueReason :: Text -> [a] -> a
selectUniqueReason _ [x] = x
selectUniqueReason msgt [] =
  err $ "No element found while trying to find exactly one: " <> msgt
selectUniqueReason msgt xs =
  err $
  "More then one element (" <> tshow (length xs) <>
  ") found while trying to extrac one: " <>
  msgt

tshow
  :: (Show a)
  => a -> Text
tshow = T.replace "\\\"" "“" . pack . groom

showCode :: Text -> Text
showCode = T.replace "\n" "\n  " . T.cons '\n'

tr
  :: Show a
  => a -> a
tr x = trace (show x <> "\n") x

isIn :: Char -> Text -> Bool
isIn c t = T.singleton c `T.isInfixOf` t

(<->) :: Text -> Text -> Text
(<->) a b = a <> " " <> b

(<\>) :: Text -> Text -> Text
(<\>) a b = a <> "\n" <> b
