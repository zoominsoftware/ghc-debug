{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module GHC.Debug.Client.Monad.Class where

import Data.Typeable
import GHC.Debug.Client.BlockCache
import GHC.Debug.Types
import Control.Tracer
import System.IO

class (MonadFail m, Monad m) => DebugMonad m where
  type DebugEnv m
  request :: (Show resp, Typeable resp) => Request resp -> m resp
  requestBlock :: (Show resp, Typeable resp) => BlockCacheRequest resp -> m resp
  traceMsg :: String -> m ()
  printRequestLog :: DebugEnv m -> IO ()
  runDebug :: DebugEnv m -> m a -> IO a
  runDebugTrace :: DebugEnv m -> m a -> IO (a, [String])
  newEnv :: Tracer IO String -> Mode -> IO (DebugEnv m)

  saveCache :: FilePath -> m ()
  loadCache :: FilePath -> m ()

  unsafeLiftIO :: IO a -> m a


data Mode = SnapshotMode FilePath | SocketMode Handle
