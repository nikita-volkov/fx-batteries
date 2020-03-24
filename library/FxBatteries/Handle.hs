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
accessFile :: IOMode -> FilePath -> Provider IOError Handle
accessFile ioMode path =
  acquireAndRelease
    (runExceptionalIO $ const $ do
      handle <- openFile path ioMode
      hSetBuffering handle NoBuffering
      return handle
    )
    (runExceptionalIO hClose)

{-|
Write bytes.
-}
putByteString :: ByteString -> Fx Handle IOError ()
putByteString byteString = runExceptionalIO (\ handle -> ByteString.hPut handle byteString)

{-|
Write text, encoding it using UTF-8.
-}
putText :: Text -> Fx Handle IOError ()
putText text = runExceptionalIO (\ handle -> Text.hPutStr handle text)
