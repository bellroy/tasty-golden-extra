-- |
--
-- Module      : Test.Tasty.Golden.Extra.GoldenVsString
-- Copyright   : (C) 2024 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- These helpers are useful for creating golden tests for functions that
-- produce textual output.
module Test.Tasty.Golden.Extra.GoldenVsString
  ( GoldenVsString (..),
    goldenVsString,
  )
where

import Data.ByteString.Lazy (ByteString)
import Test.Tasty.Discover qualified as Discover
import Test.Tasty.Golden

-- | Tasty-discoverable type for creating golden tests for functions that produce
-- textual output.
--
-- Example use:
--
-- @
--  import MySchemasWithShowAndToJSONInstances.Person (convertToCSVText)
--  import Data.Aeson qualified as Aeson
--  import System.FilePath ((\</\>))
--  import Test.Tasty.Golden.Extra.GoldenVsString (GoldenVsString (..))
--
--  tasty_FromJSON_ToJSON :: GoldenVsString
--  tasty_FromJSON_ToJSON =
--    GoldenVsString (goldenFilesPath \</\> "Person.golden.csv") $
--      maybe "Error" convertToCSVText <$>
--        Aeson.decodeFileStrict' (goldenFilesPath \</\> "Person.json")
-- @
data GoldenVsString = GoldenVsString FilePath (IO ByteString)

instance Discover.Tasty GoldenVsString where
  tasty info (GoldenVsString ref act) = pure $ goldenVsString (Discover.nameOf info) ref act
