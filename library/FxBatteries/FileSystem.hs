module FxBatteries.FileSystem
where

import FxBatteries.Prelude as Prelude
import Fx
import qualified Data.Text as Text
import qualified System.Directory as Dir


{-|
Wrapper around @`Dir.createDirectoryIfMissing` `True`@.
-}
createDirectoryRecursively :: Text -> Fx env IOError ()
createDirectoryRecursively path = runExceptionalIO (Dir.createDirectoryIfMissing True (Text.unpack path))

{-|
Wrapper around `Dir.listDirectory`.
-}
listDirectory :: Text -> Fx env IOError [Text]
listDirectory path = runExceptionalIO (fmap (fmap Text.pack) (Dir.listDirectory (Text.unpack path)))
