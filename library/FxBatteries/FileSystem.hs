module FxBatteries.FileSystem
where

import FxBatteries.Prelude as Prelude
import Fx
import qualified System.Directory as Dir


{-|
Wrapper around @`Dir.createDirectoryIfMissing` `True`@.
-}
createDirectoryRecursively :: FilePath -> Fx env IOError ()
createDirectoryRecursively path = runExceptionalIO (Dir.createDirectoryIfMissing True path)

{-|
Wrapper around `Dir.listDirectory`.
-}
listDirectory :: FilePath -> Fx env IOError [FilePath]
listDirectory path = runExceptionalIO (Dir.listDirectory path)

{-|
Wrapper around `Dir.removeFile`.
-}
removeFile :: FilePath -> Fx env IOError ()
removeFile path = runExceptionalIO (Dir.removeFile path)
