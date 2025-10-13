{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
--
-- Module      : Test.Tasty.Golden.Extra.GoldenVsToYAML
-- Copyright   : (C) 2024 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- These helpers are useful for creating golden tests for @ToJSON@ instances,
-- that you want to convert to YAML using the @Data.Yaml@ package.
module Test.Tasty.Golden.Extra.GoldenVsToYAML
  ( goldenVsToYaml,
    GoldenVsToYAML (..),
  )
where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BL
import qualified Data.Yaml as Yaml
import Test.Tasty
import qualified Test.Tasty.Discover as Discover
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.Golden.Extra.Internal (checkJsonDifference, maybeDifference)

-- | Tasty-discoverable type for creating golden tests for @ToJSON@ instances,
-- that you want to convert to YAML using the @Data.Yaml@ package.
--
-- Example use:
--
-- @
--  import MySchemasWithToJSONInstances.Person (Person)
--  import qualified Data.Aeson as Aeson
--  import System.FilePath ((\</\>))
--  import Test.Tasty.Golden.Extra.GoldenVsToYAML (GoldenVsToYAML (..))
--
--  tasty_FromJSON_ToYAML :: GoldenVsToYAML
--  tasty_FromJSON_ToYAML =
--    GoldenVsToYAML (goldenFilesPath \</\> "Person.golden.yaml") $
--      Aeson.eitherDecodeFileStrict @Person (goldenFilesPath \</\> "Person.json")
-- @
data GoldenVsToYAML = forall a. (Aeson.ToJSON a) => GoldenVsToYAML FilePath (IO a)

instance Discover.Tasty GoldenVsToYAML where
  tasty info (GoldenVsToYAML ref act) = pure $ goldenVsToYaml (Discover.nameOf info) ref act

-- | Helper function for creating a @TestTree@ for @ToJSON@-to-YAML golden tests.
-- Use when you want to test @ToJSON@ instances against a golden example of YAML
-- on disk.
--
-- Example use:
--
-- @
--  import MySchemasWithToJSONInstances.Person (Person)
--  import qualified Data.Aeson as Aeson
--  import System.FilePath ((\</\>))
--  import Test.Tasty.Golden.Extra.GoldenVsToYAML (goldenVsToYAML)
--
--  test_ToYAML :: TestTree
--  test_ToYAML = do
--    let inputFile = goldenFilesPath \</\> "Person.yaml"
--    goldenVsToYAML
--      "Test YAML serialization for Person"
--      (goldenFilesPath \</\> "Person.golden.yaml")
--      (Aeson.decodeFileStrict' @Person inputFile)
-- @
goldenVsToYaml ::
  forall a.
  (ToJSON a) =>
  -- | test name
  TestName ->
  -- | path to the «golden» file (the file that contains correct output)
  FilePath ->
  -- | action that returns an instance of the type whose instance is being tested
  IO a ->
  -- | the test verifies that the returned string is the same as the golden file contents
  TestTree
goldenVsToYaml name fp act =
  goldenTest
    name
    (Yaml.decodeFileEither fp >>= orFailTest fp)
    (Aeson.toJSON <$> act)
    (\a b -> pure . maybeDifference $ checkJsonDifference a b)
    (BL.writeFile fp . Yaml.encode)

orFailTest :: FilePath -> Either Yaml.ParseException a -> IO a
orFailTest fp =
  either
    ( error
        . (\t -> mconcat ["Failed to decode file ", fp, "\n", t])
        . Yaml.prettyPrintParseException
    )
    pure
