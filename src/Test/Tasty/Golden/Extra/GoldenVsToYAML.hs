{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

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
    goldenVsToYamlFile,
    GoldenVsToYAML (..),
  )
where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BL
import Data.String.Interpolate (i)
import Data.Yaml qualified as Yaml
import Test.Tasty
import Test.Tasty.Discover qualified as Discover
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.Golden.Extra.Internal (assertJsonEqual)

-- | Tasty-discoverable type for creating golden tests for @ToJSON@ instances,
-- that you want to convert to YAML using the @Data.Yaml@ package.
--
-- Example use:
--
-- @
--  import MySchemasWithToJSONInstances.Person (Person)
--  import Data.Aeson qualified as Aeson
--  import System.FilePath ((\</\>))
--  import Test.Tasty.Golden.Extra.GoldenVsToYAML (GoldenVsToYAML (..))
--
--  tasty_FromJSON_ToJSON :: GoldenVsToYAML
--  tasty_FromJSON_ToJSON =
--    GoldenVsToYAML (goldenFilesPath \</\> "Person.golden.yaml") $
--      Aeson.eitherDecodeFileStrict @Person (goldenFilesPath \</\> "Person.json")
-- @
data GoldenVsToYAML = forall a. (Aeson.ToJSON a) => GoldenVsToYAML FilePath (IO a)

instance Discover.Tasty GoldenVsToYAML where
  tasty info (GoldenVsToYAML ref act) = pure $ goldenVsToYaml (Discover.nameOf info) ref act

-- | Helper function for creating a @TestTree@ for @ToJSON@-to-YAML golden tests.
-- Use when you want to test code that produces a YAML file as a side effect.
--
-- Example use:
--
-- @
--  import MySchemasWithToJSONInstances.Person (Person, gen)
--  import Data.Aeson qualified as Aeson
--  import System.FilePath ((\</\>))
--  import Test.Tasty.Golden.Extra.GoldenVsToYAML (GoldenVsToYAML (..))
--
--  test_ToYAML :: TestTree
--  test_ToYAML = do
--    person <- gen
--    let outputFile = goldenFilesPath \</\> "Person.yaml"
--    goldenVsToYamlFile
--      "Test YAML serialization for Person"
--      (goldenFilesPath \</\> "Person.golden.yaml")
--      outputFile
--      (Aeson.encodeFile outputFile person)
-- @
goldenVsToYamlFile ::
  -- | test name
  TestName ->
  -- | path to the «golden» file (the file that contains correct output)
  FilePath ->
  -- | path to the output file
  FilePath ->
  -- | action that creates the output file
  IO () ->
  TestTree
goldenVsToYamlFile name ref new act =
  goldenVsToYaml @Aeson.Value name ref $ do
    act
    eJson <- Yaml.decodeFileEither new
    orFailTest new eJson

-- | Helper function for creating a @TestTree@ for @ToJSON@-to-YAML golden tests.
-- Use when you want to test @ToJSON@ instances against a golden example of YAML
-- on disk.
--
-- Example use:
--
-- @
--  import MySchemasWithToJSONInstances.Person (Person)
--  import Data.Aeson qualified as Aeson
--  import System.FilePath ((\</\>))
--  import Test.Tasty.Golden.Extra.GoldenVsToYAML (GoldenVsToYAML (..))
--
--  test_ToYAML :: TestTree
--  test_ToYAML = do
--    let inputFile = goldenFilesPath \</\> "Person.yaml"
--    goldenVsToYAML
--      "Test YAML serialization for Person"
--      (goldenFilesPath \</\> "Person.golden.yaml")
--      (Aeson.decodeFileStrict' inputFile)
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
    assertJsonEqual
    (BL.writeFile fp . Yaml.encode)

orFailTest :: FilePath -> Either Yaml.ParseException a -> IO a
orFailTest fp =
  either
    ( fail
        . ([i|Failed to decode file #{fp}|] <>)
        . Yaml.prettyPrintParseException
    )
    pure
