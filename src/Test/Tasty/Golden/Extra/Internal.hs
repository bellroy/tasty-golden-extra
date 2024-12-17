module Test.Tasty.Golden.Extra.Internal
  ( assertJsonEqual,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Diff (diff)
import Data.Aeson.Patch (patchOperations)
import Data.Text.Lazy qualified as TL
import Text.Pretty.Simple (pShowNoColor)

assertJsonEqual :: Aeson.Value -> Aeson.Value -> IO (Maybe String)
assertJsonEqual goldenJson actualJson =
  pure $ case patchOperations $ diff goldenJson actualJson of
    [] -> Nothing
    ds -> Just $ "Files contain different JSON values: " <> TL.unpack (pShowNoColor ds)
