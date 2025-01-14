module SystemTest where

import Test.Tasty.Hspec

import GHC.Debug.Client
import GHC.Debug.Types
import GHC.Debug.Snapshot
import GHC.Debug.Count
import GHC.Debug.Types.Graph hiding (buildHeapGraph, multiBuildHeapGraph)
import GHC.Debug.Types.Closures
import Data.Text (unpack)
import System.IO

import Server

import Control.Monad

import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad.Extra

import Data.Maybe
import Data.Word
import Data.IORef
import GHC.Clock
import System.Timeout
import Data.List.Extra
import Data.List.NonEmpty(NonEmpty(..))
import System.IO.Temp
import System.FilePath
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "request" $ do
    describe "RequestRoots" $
      it "should return a non-empty result" $
        withStartedDebuggee "debug-test" $ \ _ d -> do
          pause d
          roots <- run d gcRoots
          roots `shouldSatisfy` notNull

{- This test is much more sensitive to timing than the other tests, so disabled for now.
    describe "RequestRoots(fork)" $
      it "should return a non-empty result" $
        withStartedDebuggee "debug-test" $ \ _ d -> do
          fork d
          roots <- run d gcRoots
          roots `shouldSatisfy` notNull
          -}

    describe "RequestClosures" $
      it "should return a non-empty result" $
        withStartedDebuggee "debug-test" $ \ _ d -> do
          pause d
          closures <- run d (gcRoots >>= dereferenceClosures)
          closures `shouldSatisfy` notNull

    describe "RequestSavedObjects" $
      it "should return saved object" $
        withStartedDebuggee "save-one-pause" $ \ h d -> do
          waitForSync h
          pausePoll d
          os@(o:_) <- run d savedObjects
          length os `shouldBe` 1
          hg <- run d $ buildHeapGraph (Just 20) o
          ppHeapGraph (const "") hg `shouldBe` "let x1() = I# 1\nin () r0:() x1\n\n"

    describe "RequestInfoTables" $
      it "should return decodable RawInfoTables" $
        withStartedDebuggee "save-one-pause" $ \ h d -> do
          waitForSync h
          pausePoll d
          sos <- run d savedObjects
          closures <- run d (dereferenceClosures sos)
          let itptrs = map (tableId . info . noSize) closures
          its <- run d $ mapM dereferenceInfoTable itptrs
          length its `shouldBe` 1

    describe "RequestSRT" $
      it "should return decodable SRT" $
        withStartedDebuggee "srts-test-prog" $ \ h d -> do
          waitForSync h
          pausePoll d
          sos <- run d savedObjects
          closures <- run d (dereferenceClosures sos)
          let itptrs = map (tableId . info . noSize) closures
          srts <- run d $ catMaybes <$> mapM (fmap getSrt . dereferenceSRT) itptrs
          srts `shouldSatisfy`  notNull


    describe "RequestConstrDesc" $
      it "should return ConstrDesc of saved value (I# 1)" $
        withStartedDebuggee "save-one-pause" $ \ h d -> do
          waitForSync h
          pausePoll d
          (c:_) <- run d (savedObjects >>= dereferenceClosures)
          let itptr = tableId . info . noSize $ c
          cd <- run d (dereferenceConDesc itptr)
          cd `shouldBe` ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = "I#"}

    describe "RequestSourceInfo" $
      it "should return IPE information for saved object" $
        withStartedDebuggee "save-ipe-pause" $ \ h d -> do
          waitForSync h
          pause d
          (c:_) <- run d (savedObjects >>= dereferenceClosures)
          let itptr = tableId . info . noSize $ c
          cd <- run d (getSourceInfo itptr)
          fmap infoPosition cd `shouldBe` Just "test-progs/SaveIPEPause.hs:13:21-26"

    describe "RequestBlocks" $
      it "should return all blocks" $
        withStartedDebuggee "clock" $ \ h d -> do
          waitForSync h
          pause d
          bs <- run d precacheBlocks
          length bs `shouldSatisfy` (> 10)

    describe "Snapshot" $
      it "creating and loading a snapshot" $
        withSystemTempDirectory "ghc-debug" $ \td -> do
          let ss_fp = (td </> "ghc-debug-cache")
          non_par <- withStartedDebuggee "clock" $ \ h d -> do
            waitForSync h
            pause d
            run d (snapshot ss_fp)
            run d (gcRoots >>= count)
          print "made"
          res <- snapshotRun ss_fp (\d -> run d $ gcRoots >>= count)
          res `shouldBe` non_par

    describe "HeapGraph-Cycles" $
      it "should terminate" $
        withStartedDebuggee "cycles" $ \ _ d -> do
            pause d
            (r:rs) <- run d gcRoots
            _hg <- run d $ multiBuildHeapGraph Nothing (r :| rs)
            return ()

    describe "RequestResume" $
      it "should resume a paused debugee" $
        withStartedDebuggee "clock" $ \ h d -> do
          waitForSync h
          ref <- newIORef []
          withAsync (pipeStreamToListThread ref h) $ \_ -> do
            pause d
            (t:_) <- readIORef ref
            assertNoNewClockTimes ref t

            resume d

            assertNewClockTime ref
            where
              oneSecondInMicros = 1000000

              assertNoNewClockTimes :: IORef [ClockTime] -> ClockTime -> Expectation
              assertNoNewClockTimes ref t0 = do
                result <- timeout fiveSecondsInMicros $ whileM $ do
                  threadDelay oneSecondInMicros
                  (t1:_) <- readIORef ref
                  return $ t0 == t1

                result `shouldBe` Nothing

              assertNewClockTime :: IORef [ClockTime] -> Expectation
              assertNewClockTime ref = do
                now <- getMonotonicTimeNSec
                result <- timeout fiveSecondsInMicros $ whileM $ do
                  threadDelay 5000
                  (t:_) <- readIORef ref
                  return $ t < now

                result `shouldBe` Just ()

fiveSecondsInMicros :: Int
fiveSecondsInMicros = 5000000

waitForSync :: Handles -> IO ()
waitForSync h = do
  result <- timeout fiveSecondsInMicros $ ready h
  case result of
    Nothing -> error "Can not sync!"
    _ -> return ()

type ClockTime = Word64

pipeStreamToListThread :: IORef [ClockTime] -> Handles -> IO ()
pipeStreamToListThread ref h = forever $ do
  l <- readChan (output h)
  timesList <- readIORef ref
  writeIORef ref $ toClockTime l : timesList
  where
    toClockTime :: String -> ClockTime
    toClockTime = read . trim
