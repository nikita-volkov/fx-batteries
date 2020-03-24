module FxBatteries.MVar
where

import FxBatteries.Prelude as Prelude hiding (take, put, read, modify)
import Fx


{-|
Analogue to `newMVar`.
-}
new :: a -> Provider err (MVar a)
new a = runFx (runTotalIO (const (newMVar a)))

{-|
Analogue to `takeMVar`.
-}
take :: Fx (MVar a) err a
take = runTotalIO takeMVar

{-|
Analogue to `putMVar`.
-}
put :: a -> Fx (MVar a) err ()
put !a = runTotalIO (\ var -> putMVar var a)

{-|
Analogue to `readMVar`.
-}
read :: Fx (MVar a) err a
read = do
  a <- take
  put a
  return a

{-|
Analogue to `modifyMVar_`.
-}
modify :: (a -> a) -> Fx (MVar a) err ()
modify fn = take >>= put . fn

{-|
Analogue to `modifyMVar`.
-}
runState :: State a b -> Fx (MVar a) err b
runState state = do
  a <- take
  case Prelude.runState state a of
    (b, a') -> do
      put a'
      return b

{-|
Helper executing the reader monad.
-}
runReader :: Reader a b -> Fx (MVar a) err b
runReader reader = read & fmap (Prelude.runReader reader)
