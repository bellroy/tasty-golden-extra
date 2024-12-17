-- |
--
-- Module      : Test.Tasty.Golden.Extra.GoldenVsToJSON
-- Copyright   : (C) 2024 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- These helpers are useful for creating golden tests for @ToJSON@ instances.
module Test.Tasty.Golden.Extra.GoldenVsToJSON
  ( GoldenVsToJSON (..),
    goldenVsToJson,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Discover qualified as Discover
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.Golden.Extra.Internal (checkJsonDifference, maybeDifference)

-- | Tasty-discoverable type for creating golden tests for @ToJSON@ instances.
--
-- Example use:
--
-- @
--  import MySchemasWithToJSONInstances.Person (Person)
--  import Data.Aeson qualified as Aeson
--  import System.FilePath ((\</\>))
--  import Test.Tasty.Golden.Extra.GoldenVsToJSON (GoldenVsToJSON (..))
--
--  tasty_FromJSON_ToJSON :: GoldenVsToJSON
--  tasty_FromJSON_ToJSON =
--    GoldenVsToJSON (goldenFilesPath \</\> "Person.golden.json") $
--      Aeson.eitherDecodeFileStrict @Person (goldenFilesPath \</\> "Person.json")
-- @
data GoldenVsToJSON = forall a. (Aeson.ToJSON a) => GoldenVsToJSON FilePath (IO a)

instance Discover.Tasty GoldenVsToJSON where
  tasty info (GoldenVsToJSON ref act) = pure $ goldenVsToJson (Discover.nameOf info) ref act

-- | Helper function for creating a @TestTree@ for @ToJSON@ golden tests.
-- Use when you want to test @ToJSON@ instances against a golden example on disk.
--
-- Example use:
--
-- @
--  import MySchemasWithToJSONInstances.Person (Person)
--  import Data.Aeson qualified as Aeson
--  import System.FilePath ((\</\>))
--  import Test.Tasty.Golden.Extra.GoldenVsToJSON (goldenVsToJSON)
--
--  test_ToJSON :: TestTree
--  test_ToJSON = do
--    let inputFile = goldenFilesPath \</\> "Person.json"
--    goldenVsToJSON
--      "Test ToJSON instance for Person"
--      (goldenFilesPath \</\> "Person.golden.json")
--      (Aeson.decodeFileStrict' @Person inputFile)
-- @
goldenVsToJson ::
  forall a.
  (Aeson.ToJSON a) =>
  -- | test name
  TestName ->
  -- | path to the «golden» file (the file that contains correct output)
  FilePath ->
  -- | action that returns an instance of the type whose instance is being tested
  IO a ->
  -- | the test verifies that the returned string is the same as the golden file contents
  TestTree
goldenVsToJson name fp act =
  goldenTest
    name
    (Aeson.decodeFileStrict fp >>= orFailTest ("Couldn't decode golden JSON file:" <> fp))
    (Aeson.toJSON <$> act)
    (\a b -> pure . maybeDifference $ checkJsonDifference a b)
    (BL.writeFile fp . encodePretty)

orFailTest :: String -> Maybe a -> IO a
orFailTest msg = maybe (error msg) pure
