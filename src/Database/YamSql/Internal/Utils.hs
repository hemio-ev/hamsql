-- This file is part of HamSql
--
-- Copyright 2014-2016 by it's authors.
-- Some rights reserved. See COPYING, AUTHORS.
module Database.YamSql.Internal.Utils
  ( tshow
  , Text
  , isIn
  , (<>)
  , asum
  , fromMaybe
  , makeLenses
  , LensLike'
  , makePrisms
  ) where

import Control.Lens (LensLike', makeLenses, makePrisms)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Text.Pretty.Simple (pShow)

tshow :: (Show a) => a -> Text
tshow = T.replace "\\\"" "“" . toStrict . pShow

isIn :: Char -> Text -> Bool
isIn c t = T.singleton c `T.isInfixOf` t
