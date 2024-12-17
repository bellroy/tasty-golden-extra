-- |
--
-- Module      : Test.Tasty.Golden.Extra.GoldenVsShow
-- Copyright   : (C) 2024 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- These helpers are useful for creating golden tests for @Show@ instances.
module Test.Tasty.Golden.Extra.GoldenVsShow
  ( goldenVsShow,
    GoldenVsShow (..),
  )
where

import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding (encodeUtf8)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Discover qualified as Discover
import Test.Tasty.Golden
import Text.Show.Pretty (ppShow)

-- | Tasty-discoverable type for creating golden tests for @Show@ instances.
--
-- Example use:
--
-- @
--  import MySchemasWithShowAndToJSONInstances.Person (Person)
--  import Data.Aeson qualified as Aeson
--  import System.FilePath ((\</\>))
--  import Test.Tasty.Golden.Extra.GoldenVsShow (GoldenVsShow (..))
--
--  tasty_GoldenVsShow :: GoldenVsShow
--  tasty_GoldenVsShow =
--    GoldenVsShow (goldenFilesPath \</\> "Person.golden.txt") $
--      Aeson.eitherDecodeFileStrict @Person (goldenFilesPath \</\> "Person.json")
-- @
data GoldenVsShow = forall a. (Show a) => GoldenVsShow FilePath (IO a)

instance Discover.Tasty GoldenVsShow where
  tasty info (GoldenVsShow ref act) =
    pure $ goldenVsShow (Discover.nameOf info) ref act

-- | Helper function for creating a @TestTree@ for @Show@ golden tests.
-- Use when you want to test @Show@ instances against a golden example on disk.
--
-- Example use:
--
-- @
--  import MySchemasWithShowAndShowInstances.Person (Person)
--  import Data.Aeson qualified as Aeson
--  import System.FilePath ((\</\>))
--  import Test.Tasty.Golden.Extra.GoldenVsShow (goldenVsShow)
--
--  test_Show :: TestTree
--  test_Show = do
--    let inputFile = goldenFilesPath \</\> "Person.json"
--    goldenVsShow
--      "Test Show instance for Person"
--      (goldenFilesPath \</\> "Person.golden.txt")
--      (Aeson.decodeFileStrict' @Person inputFile)
-- @
goldenVsShow ::
  (Show a) =>
  -- | test name
  TestName ->
  -- | path to the «golden» file (the file that contains correct output)
  FilePath ->
  -- | action that returns an instance of the type whose instance is being tested
  IO a ->
  -- | the test verifies that the returned string is the same as the golden file contents
  TestTree
goldenVsShow name ref =
  goldenVsString name ref . fmap (encodeUtf8 . Text.pack . ppShow)
