module FxBatteries.Handle
where

import FxBatteries.Prelude as Prelude
import Fx
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString as ByteString


{-|
Access a file.

Wrapper over `openFile` and `hClose`.
-}
accessFile :: Text -> IOMode -> Provider IOError Handle
accessFile path ioMode =
  acquireAndRelease
    (runExceptionalIO (openFile (Text.unpack path) ioMode))
    (runExceptionalIO . hClose)

{-|
Write bytes.
-}
putByteString :: ByteString -> Fx Handle IOError ()
putByteString byteString = handleEnv (\ handle -> runExceptionalIO (ByteString.hPut handle byteString))

{-|
Write text, encoding it using UTF-8.
-}
putText :: Text -> Fx Handle IOError ()
putText text = handleEnv (\ handle -> runExceptionalIO (Text.hPutStr handle text))
