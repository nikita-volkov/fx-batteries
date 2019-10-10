module FxBatteries.IORef
where

import FxBatteries.Prelude as Prelude
import Fx


{-|
Wrapper around `newIORef`.
-}
new :: a -> Provider err (IORef a)
new a = runFx (runTotalIO (newIORef a))

{-|
Wrapper around `readIORef`.
-}
read :: Fx (IORef a) err a
read = handleEnv (runTotalIO . readIORef)

{-|
Wrapper around `writeIORef`.
-}
write :: a -> Fx (IORef a) err ()
write !a = handleEnv (\ ref -> runTotalIO (writeIORef ref a))

{-|
Wrapper around `modifyIORef'`.

__Notice:__
To protect from a common pitfall of piling up thunks,
this function is strict and no lazy version is provided.
-}
modify :: (a -> a) -> Fx (IORef a) err ()
modify fn = handleEnv (\ ref -> runTotalIO (modifyIORef' ref fn))

{-|
Wrapper around `atomicModifyIORef'`.

__Notice:__
To protect from a common pitfall of piling up thunks,
this function is strict and no lazy version is provided.
-}
interactAtomically :: (a -> (b, a)) -> Fx (IORef a) err b
interactAtomically fn = handleEnv (\ ref -> runTotalIO (atomicModifyIORef' ref (swap . fn)))

{-|
Adaptation of `interactAtomically` to the `State` monad.
-}
runStateAtomically :: State a b -> Fx (IORef a) err b
runStateAtomically = interactAtomically . runState

{-|
Helper executing the reader monad.
-}
runReader :: Reader a b -> Fx (IORef a) err b
runReader m = handleEnv (runTotalIO . fmap (Prelude.runReader m) . readIORef)
