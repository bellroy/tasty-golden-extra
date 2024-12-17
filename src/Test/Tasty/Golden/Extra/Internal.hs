-- |
--
-- Module      : Test.Tasty.Golden.Extra.Internal
-- Copyright   : (C) 2024 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- Common types and functions  used by the other modules in `tasty-golden-extra`.
module Test.Tasty.Golden.Extra.Internal
  ( checkJsonDifference,
    maybeDifference,
    JsonDifference (..),
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Diff (diff)
import Data.Aeson.Patch (patchOperations)
import Data.Text.Lazy qualified as TL
import Text.Pretty.Simple (pShowNoColor)

-- | Represents the result of comparing two JSON values - either the JSON is
-- identical, or there are differences and you are given an error message
-- containing the differences.
data JsonDifference
  = JsonIdentical
  | JsonDifferent String

-- | Convert a `JsonDifference` to a `Maybe String` containing the error message.
maybeDifference :: JsonDifference -> Maybe String
maybeDifference JsonIdentical = Nothing
maybeDifference (JsonDifferent diffString) = Just diffString

-- | Compare two JSON values and return a `JsonDifference` representing the result.
checkJsonDifference ::
  Aeson.Value ->
  Aeson.Value ->
  JsonDifference
checkJsonDifference goldenJson actualJson =
  case patchOperations $ diff goldenJson actualJson of
    [] -> JsonIdentical
    ds ->
      JsonDifferent $
        "Files contain different JSON values:\n" <> TL.unpack (pShowNoColor ds)
