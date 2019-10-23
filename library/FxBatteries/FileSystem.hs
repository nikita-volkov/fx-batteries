module FxBatteries.FileSystem
where

import FxBatteries.Prelude as Prelude
import Fx
import Path
import qualified Data.Text as Text
import qualified System.Directory as Dir


type DirOrFilePath b = Either (Path b Dir) (Path b File)

{-|
Wrapper around @`Dir.createDirectoryIfMissing` `True`@.
-}
createDirectoryRecursively :: Path b Dir -> Fx env IOError ()
createDirectoryRecursively path = runExceptionalIO (Dir.createDirectoryIfMissing True (toFilePath path))

{-|
Wrapper around `Dir.listDirectory`.
-}
listDirectory :: Path b Dir -> Fx env IOError [DirOrFilePath Rel]
listDirectory path = do
  fpList <- runExceptionalIO (Dir.listDirectory (toFilePath path))
  forM fpList $ \ fp -> case parseRelDir fp of
    Right path -> return (Left path)
    Left _ -> case parseRelFile fp of
      Right path -> return (Right path)
      Left exc -> fail ("Unexpected path parsing exception: " <> show exc)

{-|
Wrapper around `Dir.removeFile`.
-}
removeFile :: Path b File -> Fx env IOError ()
removeFile path = runExceptionalIO (Dir.removeFile (toFilePath path))
