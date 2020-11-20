module Main where

import GHC.Debug.Client
import GHC.Debug.Client.Monad
import GHC.Debug.Types.Graph
import GHC.Debug.Types.Closures

import Control.Monad
import Debug.Trace
import Control.Exception
import Control.Concurrent
import Data.Bitraversable
import GHC.Vis
import Data.Monoid
import Control.Applicative

import Data.List.Extra (trim)
import System.Process

saveOnePath :: IO FilePath
saveOnePath = testProgPath "save-one"

debugTestPath :: IO FilePath
debugTestPath = testProgPath "debug-test"

dyePackTestPath :: IO FilePath
dyePackTestPath = testProgPath "dyepack-test"

testProgPath :: String -> IO FilePath
testProgPath progName = do
  path <- readCreateProcess shellCmd []
  return $ trim path
  where
    shellCmd = shell $ "which " ++ progName

main = withDebuggeeSocket "banj" "/tmp/ghc-debug" Nothing (\e -> p18 e >> traceRequestLog e)
{-
main = do
  -- Get the path to the "debug-test" executable
--  prog <- debugTestPath -- Or @dyePackTestPath@
--  print prog
  let prog = "/home/matt/ghc-debug/dist-newstyle/build/x86_64-linux/ghc-9.1.0.20201106/ghc-debug-stub-0.1.0.0/x/debug-test/build/debug-test/debug-test"

  -- Start the program and do some debugging
  let someDebuggingAction = p13
  withDebuggee prog "/tmp/ghc-debug" someDebuggingAction
  -}

-- Test pause/resume
p1 e = pauseDebuggee e ((void $ getChar))


-- Testing error codes
p2 e = runTrace e $ do
  request RequestPause
  traceWrite "req1"
  request RequestPause
  request RequestPause
  request RequestPause

-- Testing get version
p3 e = runTrace e $ do
  ver <- request RequestVersion
  request RequestPause
  request RequestResume
  return ver


-- Testing get roots
p4 e = pauseThen e $ do
  request RequestRoots >>= traceWrite

-- request closures
p5 e = pauseThen e $ do
  r <- request RequestRoots
  traceWrite (length r)
  forM_ [0..length r - 1] $ \i -> do
    let cs = [r !! i]
    traceWrite cs
    dereferenceClosures cs

-- request all closures
p5a e = pauseThen e $ do
  rs <- request RequestRoots
  traceWrite rs
  cs <- request (RequestClosures rs)
  traceWrite cs
  {-
  let it = getInfoTblPtr c
  print it
  (itr:_) <- request d (RequestInfoTables [it])
  print itr
  print c
  print (decodeClosure itr c)
  -}

-- request all closures
p5b e = pauseThen e $ do
  rs <- request RequestRoots
  dereferenceClosures rs





p6 e = do
  -- This blocks until a pause
  run e $ request RequestPoll
  putStrLn "POLL"
  -- Should return already paused
  pause e
  putStrLn "PAUSE"
  -- Now unpause
  resume e
  putStrLn "RESUME"

-- Request saved objects
p7 e = pauseThen e $ do
  request RequestSavedObjects >>= traceWrite

-- request saved objects
p8 e = pauseThen e $ do
  sos <- request RequestSavedObjects
  traceWrite =<< dereferenceClosures sos

-- Using findPtr
{-
p9 d = do
  request d RequestPause
  (s:_) <- request d RequestSavedObjects
  print s
  sos <- request d (RequestFindPtr s)
  print ("FIND_PTR_RES", sos)
  dereferenceClosures d sos

p10 d = do
  request d RequestPause
  (s:_) <- request d RequestRoots
  request d (RequestFindPtr s) >>= print

p11 e = do
  threadDelay 10000000
  pause e
  itb <- runTrace e $ do
    ss <- request d RequestSavedObjects
    [c] <- request d (RequestClosures ss)
    return (getInfoTblPtr c)
  case lookupDwarf d itb of
    Just r -> showFileSnippet d r
    Nothing -> print "No Dwarf!"
  -}

{-
p12 d = do
  request d RequestPoll
  [ss] <- request d RequestSavedObjects
  r <- request d (RequestFindPtr ss)
  print ss
  putStrLn "Retaining closures"
  dcs <- dereferenceClosures d r
  mapM print dcs
  putStrLn ""
  cs <- request d (RequestClosures r)
  forM_ cs $ \c -> do
    let itb = getInfoTblPtr c
    case lookupDwarf d itb of
      Just r -> showFileSnippet d r
      Nothing -> return ()

  print "Following thunk"
  let thunk = r !! 2
  r <- request d (RequestFindPtr thunk)
  putStrLn "Retaining closures 2"
  dereferenceClosures d r >>= mapM print
  putStrLn ""
  cs <- request d (RequestClosures r)
  forM_ cs $ \c -> do
    let itb = getInfoTblPtr c
    case lookupDwarf d itb of
      Just r -> showFileSnippet d r
      Nothing -> return ()
      -}

-- testing stack decoding
p13 e = pauseThen e $ do
  rs <- request RequestRoots
  traceWrite ("NUMBER OF ROOTS = " ++ show (length rs))
  results <- forM (zip rs [0..]) $ \(r, n) -> do
              traceWrite ("ROOT", n, r)
              fullTraversal r
  return results
  traceMsg "Full Traversal complete"
  traceMsg ("Number of roots traversed: " ++ show (length results))
  let counts = map countNodes results
      inclusive_counts = map inclusive results
  forM (zip results [0..]) $ \(re@(MkFix1 r), n) -> do
    traceMsg (show n ++ "(" ++ show (tipe (decodedTable (info (noSize r)))) ++ "): " ++ show (treeSize re))
    --print (inclusive re)
  traceMsg ("Total: " ++ show (sum counts))


p14 e = pauseThen e $ do
  rs <- request RequestSavedObjects
  forM_ rs $ \r -> do
    traceWrite r
    res <- fullTraversal r
    traceWrite res

-- Testing ghc-vis
{-
p15 d = do
  request d RequestPause
  (r:_) <- request d RequestSavedObjects
  vis d
  view r "saved"
  getChar
  -}

-- pretty-print graph
p16 e = do
  pause e
  hg <- run e $ do
          (so:_) <- request RequestSavedObjects
          buildHeapGraph derefFuncM 20 () so
  putStrLn $ ppHeapGraph (const "") hg

-- Testing IPE
p17 e = do
  pause e
  runTrace e $ do
    [so] <- request RequestSavedObjects
    [c] <- request (RequestClosures [so])
    let it = getInfoTblPtr c
    traceWrite c
    traceWrite it
    traceWrite =<< request (RequestSourceInfo it)


-- Use with large-thunk
p18 e = do
  -- Wait fro the pause
  run e $ request RequestPoll
  runTrace e $ do
    rs <- request RequestSavedObjects
    forM_ rs $ \r -> do
      traceWrite r
      fullTraversal r
      traceWrite ("DONE", r)

derefFunc e c = run e $ derefFuncM c

derefFuncM c = do
  c <- dereferenceClosure c
  tritraverse dereferenceConDesc pure pure c

-- Use with large-thunk
p19 e = do
  run e $ request RequestPoll
  hg <- run e $ do
          (so:_) <- request RequestSavedObjects
          hg <- buildHeapGraph derefFuncM 10 () so
          annotateWithSource hg
  putStrLn $ ppHeapGraph (maybe "" concat) hg

-- | Lookup the source location of THUNKs
annotateWithSource :: HeapGraph a -> DebugM (HeapGraph (Maybe [String]))
annotateWithSource hg = traverseHeapGraph go2 hg
  where
    go2 (HeapGraphEntry a1 a2 a3 a4) = HeapGraphEntry a1 a2 a3 <$> go a2
    go (ThunkClosure (StgInfoTableWithPtr i _) _ _) = do
      Just <$> request (RequestSourceInfo i)
    go _ = return Nothing
