{-# LANGUAGE ScopedTypeVariables #-}
module Server
  ( withServer
  , withStartedDebuggee
  , Handles(..)
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.IO
import System.Process
import System.IO.Extra

import GHC.Debug.Client

data Handles = Handles {
  ready :: IO (), -- Block until initialised
  output :: Chan String -- Stdout, by line
}

type TestFunction a = (Handles -> IO a)

withServer :: String  -- ^ executable name
           -> FilePath -- ^ socket name
           -> TestFunction a -- ^ test code
           -> IO a
withServer serverExe socketName f = do
  let cmd:args = words serverExe
  let p = (proc cmd args) {
        std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = CreatePipe,
        env = Just [("GHC_DEBUG_SOCKET",socketName)]
        }
  withCreateProcess p $ \h1 h2 h3 ph -> runTestFunction f h1 h2 h3 ph

runTestFunction :: TestFunction a -- ^ test code
                -> Maybe Handle -- ^ stdin
                -> Maybe Handle -- ^ stdout
                -> Maybe Handle -- ^ stderr
                -> ProcessHandle
                -> IO a
runTestFunction f (Just _) (Just serverOut) (Just serverErr) serverProc = do
  hSetBuffering serverErr NoBuffering
  hSetBinaryMode serverErr True
  line_chan <- newChan
  sink_var <- newEmptyMVar
  let wait_ready = takeMVar sink_var

  let errSinkThread =
        forever $ hGetLine serverErr >>= putStrLn . ("SERVER:E:" ++ )
      outSinkThread =
        forever $ do
          line <- hGetLine serverOut
          case line of
            "\"sync\"" -> putMVar sink_var ()
            out -> do
              writeChan line_chan out
              putStrLn ("SERVER:" ++ out)
      -- Rethrow exception if debuggeee process terminates
      debuggeeProcess = do
        res <- waitForProcess serverProc
        error ("Debuggee process exited unexpectedly: " ++ show res)

      raceImmortal :: IO a -> IO a -> IO a
      raceImmortal p1 p2 = either id id <$> race p1 p2


  let handles = Handles wait_ready line_chan

  -- The things which shouldn't exit
  let immortal_threads = debuggeeProcess `raceImmortal` errSinkThread `raceImmortal` outSinkThread

  -- Then race the immortal threads against the actual action, which should terminate
  -- and produce a value
  either id id <$> race immortal_threads (f handles)

runTestFunction _ _ _ _ _ = error "Starting the process failed"

withStartedDebuggee :: String  -- ^ executable name
             -> (Handles -> Debuggee -> IO a) -- ^ action
             -> IO a
withStartedDebuggee exeName action = withTempDir $ \ tempDirPath -> do
  let socketName = tempDirPath ++ "/ghc-debug"
  withServer exeName socketName $ \handles -> do
    -- TODO wait (programmatically) for the socket to appear
    threadDelay 500000

    withDebuggeeConnect socketName (action handles)
