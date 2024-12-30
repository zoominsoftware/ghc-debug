{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.State.Strict as State
import Data.Char (chr)
import Data.Containers.ListUtils (nubOrd)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (dropWhileEnd, intercalate, isInfixOf, isPrefixOf, nub, sort, sortOn, (\\), sortBy)
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Ord (Down(..), comparing)
import Data.Semigroup
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Format, formatShow)
import Data.Word (Word64)
import Numeric (showHex)
import System.Environment
import System.Process
import Text.Printf (printf)

import Data.IntSet (IntSet)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import qualified Data.Text.Encoding as TE

import GHC.Debug.Client hiding (DebugM)
import GHC.Debug.Client.Monad.Simple (DebugM(..))
import GHC.Debug.Count (count)
import GHC.Debug.Fragmentation
import GHC.Debug.ObjectEquiv
import GHC.Debug.Profile
import GHC.Debug.Profile.Types (Sample(Sample, getSamples))
import GHC.Debug.Retainers
    ( addLocationToStack,
      addLocationToStack',
      displayRetainerStack',
      displayRetainerStack,
      findRetainers,
      findRetainersOf,
      findRetainersOfConstructor,
      ClosureFilter(..))
import GHC.Debug.Snapshot (snapshot)
import GHC.Debug.Thunks (thunkAnalysis)
import GHC.Debug.TypePointsFrom
import GHC.Debug.Types.Graph (ppClosure)
import GHC.Debug.Types.Ptr (BlockPtr(..), ClosurePtr(..), blockMBlock, isPinnedBlock, rawBlockAddr, arrWordsBS)

import GHC.Utils.Misc (sortWith)
import GHC.Stack (SrcLoc(..), prettySrcLoc)

withPaused :: (Debuggee -> IO a) -> Debuggee -> IO a
withPaused prog target =
  pause target *> prog target <* resume target

takeSnapshots = withDebuggeeConnect "/tmp/ghc-debug" $ \target -> do
  putStrLn "Saving snapshot..."
  pause target
  run target $ snapshot "/tmp/ghc-debug-heap.snap0"
  resume target
  putStrLn "Run test iteration and hit enter to continue..." >> void getLine
  pause target
  run target $ snapshot "/tmp/ghc-debug-heap.snap1"
  resume target

diffSnapshots = do
  -- ghc-debug-brick writes them to XDG dirs
  let snap0 = "/home/ulidtko/.local/share/ghc-debug/debuggee/snapshots/snapshot1"
  let snap1 = "/home/ulidtko/.local/share/ghc-debug/debuggee/snapshots/snapshot2"
  cens0 <- snapshotRun snap0 $ flip runTrace (gcRoots >>= censusClosureType)
  cens1 <- snapshotRun snap1 $ flip runTrace (gcRoots >>= censusClosureType)
  let diff_census = Map.differenceWith diffCensi cens1 cens0
  printClosureCensusDiff diff_census

diffInteractive do_scan do_diff = withDebuggeeConnect "/tmp/ghc-debug" $ \target -> do
  putStrLn "Taking initial heap census..."
  pause target
  cens_init <- runTrace target do_scan
  resume target
  go target cens_init
  where
    go target cens0 = do
      putStrLn "Run test iteration and hit enter to continue..." >> void getLine
      pause target
      cens1 <- run target do_scan
      do_diff cens0 cens1 target
      resume target
      go target cens1

loopDownloadTestWithState :: DebugM a -> (a -> a -> Debuggee -> StateT s IO ()) -> s -> IO bottom
loopDownloadTestWithState do_scan do_diff state0
  = withDebuggeeConnect "/tmp/ghc-debug" $ \target -> do
  putStrLn "Connected to debuggee, taking initial heap census..."
  initial <- flip withPaused target $ flip run do_scan
  State.evalStateT (go target initial) state0
  where
    go target datum0 = do
      datum' <- liftIO $ do
        putStrLn ""
        now <- getCurrentTime
        putStrLn $ "Running test iteration... " <> formatShow iso8601Format now
        runIteration
        putStrLn "Delay for idle GC..." >> threadDelay 2_500_000 >> pause target
        run target do_scan
      liftIO (putStrLn "Analysis...") >> do_diff datum0 datum' target
      liftIO $ resume target
      go target datum'
    -- runIteration = spawnCommand "curl -s http://localhost:3000/admin/downloadbundle/adams_2022_4/enus -o /dev/null -b /tmp/zdocs-cookies" >>= waitForProcess
    runIteration = spawnCommand "curl -s http://localhost:3000/admin/origbundle/adams_modeler_2022.1/enus -o /dev/null -b /tmp/zdocs-cookies" >>= waitForProcess
    -- runIteration = spawnCommand "curl -s http://localhost:3000/admin/origbundle/adams_2022_4/enus -o /dev/null -b /tmp/zdocs-cookies" >>= waitForProcess
    -- TODO /api/categories

loopDownloadTest :: DebugM a -> (a -> a -> Debuggee -> IO ()) -> IO bottom
loopDownloadTest do_scan do_diff
  = loopDownloadTestWithState do_scan (\x0 x1 dbg -> liftIO $ do_diff x0 x1 dbg) ()

oneShot do_analysis do_output = withDebuggeeConnect "/tmp/ghc-debug" $ \target -> do
  analData <- (`run` do_analysis) `withPaused` target
  do_output analData

-- main = takeSnapshots
-- main = diffSnapshots
-- main = diffInteractive (gcRoots >>= censusClosureType) diffSrclocCensi
-- main = loopDownloadTest (gcRoots >>= censusClosureType) diffSrclocCensi
-- main = loopDownloadTest (gcRoots >>= focusCtorsSrcLoc) (diffRetainers "SrcLoc")
-- main = loopDownloadTest (gcRoots >>= focusCtorsAllBS) (diffRetainers "BS")
-- main = snapshotRun "/tmp/ghc-debug-heap.snap0" $ \t -> run t (gcRoots >>= groupedBytestrings) >>= printCtorCensus . filterOutCerts
-- main = loopDownloadTest (liftM2 (,) gatherBsInventory gatherBlockInventory) diffInventory
-- main = loopDownloadTest (gcRoots >>= thunkAnalysis) diffThunks
-- main = diffInteractive gatherBasicHeapProfile diffBasicHeapProfile
-- main = loopDownloadTest gatherSuiteshareInventory summarizeSuiteshareInventory
-- main = oneShot (gatherCtorRetainers "App") outputRetainers
main = loopDownloadTestWithState gatherCorkState summarizeCorkStep mempty

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Filter out data constructors satisfying provided condition
focusCtorsBy :: (ConstrDesc -> [ClosurePtr] -> [Word] -> StgInfoTableWithPtr -> DebugM Bool)
             -> [ClosurePtr] -> DebugM [ClosurePtr]
focusCtorsBy cond = GHC.Debug.Profile.closureFilter $ \sc -> do
  hextraverse pure pure pure dereferenceConDesc pure pure sc <&> noSize >>= \case
    ConstrClosure { info, ptrArgs, dataArgs, constrDesc }
      -> cond constrDesc ptrArgs dataArgs info
    _ -> pure False

focusCtorsSrcLoc = focusCtorsBy $ \ConstrDesc{..} _ _ _ ->
  pure $ pkg == "base" && modl == "GHC.Stack.Types" && name == "SrcLoc"
  -- let (ptrs, ints) = splitAt 3 ptrArgs
  -- intVals <- mapM tryDerefUnliftedInt ints <&> catMaybes
  -- pure $ intVals == [57, 14, 57, 20]
  -- pure $ intVals == [72, 14, 72, 20]

-- | Select all live ByteString's on the heap
focusCtorsAllBS = focusCtorsBy $ \ConstrDesc{..} _ _ _ ->
  pure $ name == "BS" && modl == "Data.ByteString.Internal.Type"

-- | Select ByteString's on the heap backed by ARR_WORDS / PlainPtr ctor of ForeignPtr
focusClosuresBSPlainPtr = focusCtorsBy $ \ConstrDesc{..} ptrArgs _ _ -> if
  | name == "BS" && modl == "Data.ByteString.Internal.Type"
      -> let [ptrArg0] = ptrArgs;
         in tryDerefPlainPtr ptrArg0 <&> isJust
  | otherwise -> pure False

focusClosuresBSMallocPtr = focusCtorsBy $ \ConstrDesc{..} ptrArgs _ _ -> if
  | name == "BS" && modl == "Data.ByteString.Internal.Type"
      -> let [ptrArg0] = ptrArgs
         in tryDerefMallocPtr ptrArg0 <&> isJust
  | otherwise -> pure False

-- | Select live ARR_WORDS ByteString's not containing -----BEGIN CERTIFICATE-----
focusClosuresBAPlainPtrNonCert = focusCtorsBy $ \ConstrDesc{..} ptrArgs dataArgs _ -> if
  | name == "BS" && modl == "Data.ByteString.Internal.Type"
      -> let [ptrArg0] = ptrArgs; [dataArg0, dataArg1] = dataArgs in
         tryDerefPlainPtr ptrArg0 >>= \case
          Nothing -> return False
          Just parrwords -> do
            arrwordsSC <- dereferenceClosure parrwords
            arrwordsDC <- hextraverse pure dereferenceSRT pure dereferenceConDesc pure pure arrwordsSC
            let arrwordsPP = ppClosure (\_ _ -> "■") 0 (noSize arrwordsDC)
            if "-----BEGIN CERTIFICATE-----" `isInfixOf` arrwordsPP
              then return False
              else return True
  | otherwise -> pure False

{- HLINT ignore "Redundant multi-way if" -}

diffCensi cs1 cs2
  | cs1 == cs2 = Nothing
  | otherwise = Just $ CS
  (cscount cs1 - cscount cs2)
  (cssize cs1 - cssize cs2)
  (Max 0)
  (Sample []) -- (Sample $ getSamples (sample cs1) \\ getSamples (sample cs2))

summarizeHeapstatChange cens0 cens1 _target = do
  putStrLn "Traversed all heap. Changes since previous iteration:"
  let count0 = getCount $ cscount cens0
  let count1 = getCount $ cscount cens1
  printf "  closure count: %9d -> %-9d -- %-+8d\n" count0 count1 (count1 - count0)
  let bytes0 = getSize $ cssize cens0
  let bytes1 = getSize $ cssize cens1
  printf "     total size: %9d -> %-9d -- %-+8d\n" bytes0 bytes1 (bytes1 - bytes0)
  let largest0 = getSize . getMax $ csmax cens0
  let largest1 = getSize . getMax $ csmax cens1
  unless (largest0 == largest1) $
    printf "        largest: %9d -> %-9d -- %-+8d\n" largest0 largest1 (largest1 - largest0)
gatherBasicHeapProfile = do
  roots <- gcRoots
  totals <- GHC.Debug.Count.count roots
  census <- censusClosureType roots
  return (totals, census)
diffBasicHeapProfile (totals0, clos0) (totals1, clos1) target = do
  summarizeHeapstatChange totals0 totals1 target
  putStrLn "Changed closures:"
  printClosureCensusDiff $ Map.differenceWith diffCensi clos1 clos0

diffThunks cens0 cens1 _target = do
  let count0 = getCount . sum $ cens0
  let count1 = getCount . sum $ cens1
  let diff = Map.differenceWith $ \(Count c1) (Count c2) -> let d = c1 - c2 in
                                  if d == 0 then Nothing else Just (Count d)
  printf "Change in thunks (total %d -> %d):\n" count0 count1
  printThunkDiff (diff cens1 cens0)

diffRetainers label cens0 cens1 target = do
  (ndiff, retainers) <- run target $ do
    roots <- gcRoots
    let subj = Set.toList $ Set.fromList cens1 `Set.difference` Set.fromList cens0
    retainers <- findRetainersOf (Just 7) roots subj
    (length subj,) <$> traverse (fmap (label <> " retainers:",) . addLocationToStack') retainers
  displayRetainerStack' retainers
  putStrLn $ printf "-- %+d moved %s closures" ndiff label

diffSrclocCensi cens0 cens1 target = do
  let diff_census = Map.differenceWith diffCensi cens1 cens0
  printClosureCensusDiff diff_census
  let newsrclocs = Map.elems
                 . Map.filterWithKey (\(k,_) CS{..} ->
                   prettyProfileKey k == "base:GHC.Stack.Types:SrcLoc" && cscount > 0)
                 $ diff_census
  putStrLn $ "-- new SrcLocs: " <> if null newsrclocs then "none!" else showCensusStats (head newsrclocs)
  srclocsByArgs <- run target (gcRoots >>= groupedSrclocs)
  printCtorCensus $ Map.filterWithKey (\_ CS{..} -> cscount > 3) srclocsByArgs


gatherBsInventory = do
  roots <- gcRoots
  bsAll <- focusCtorsAllBS roots
  bsPlain <- focusClosuresBSPlainPtr bsAll
  bsNoncert <- focusClosuresBAPlainPtrNonCert bsPlain
  censusByFptr <- groupedByFptrCtor bsAll
  censusNoncerts <- groupedBytestrings bsNoncert
  pure (bsAll, bsPlain, bsNoncert, censusByFptr, censusNoncerts)

gatherBlockInventory = do
  allblocks <- precacheBlocks
  roots <- gcRoots
  censusPinned <- censusPinnedBlocks allblocks roots
  let pinnedblocks = filter isPinnedBlock allblocks
  let megablocks = nubOrd . sort . map (blockMBlock . rawBlockAddr) $ allblocks
  let pmegablocks = nubOrd . sort . map (blockMBlock . rawBlockAddr) $ pinnedblocks
  let blockCounts = (length allblocks, length pinnedblocks, length megablocks, length pmegablocks)
  pure (censusPinned, blockCounts, megablocks, pmegablocks)

diffInventory cens0 cens1 target = do
  -- let (bsAll, bsPlain, bsNoncert, pinned) = cens0
  let ( (bsAll', bsPlain', bsNoncert', byFptr', noncerts')
        , (pinned', (bcA, bcP, bcM, bcMP), mbs, pmbs)) = cens1
  let nPinned = length . nub . concatMap projectCptrs . Map.elems $ pinned'
  let fragMB = megablockFrag mbs
  putStrLn "=== === == == === === == == === ==="
  putStrLn . unwords $
    [ show (Map.size pinned'), "blocks are pinned by", show nPinned, "closures" ]
  putStrLn $ printf "    blocks pinned - %4d / %6d\nmegablocks pinned - %4d / %6d" bcP bcA bcMP bcM
  putStrLn $ "megablock fragmentation: " <> show fragMB <> " = " <> showMiB fragMB
  putStrLn $ "pinned megablocks: " <> unwords (map showHex' pmbs)
  putStrLn . unwords $
    [ show (length bsAll'), "ByteStrings alive, among which"
    , show (length bsPlain'), "use PlainPtr, among which"
    , show (length bsNoncert'), "aren't certificates"
    ]
  putStrLn "By ForeignPtr constructor:" >> printCtorCensus byFptr'
  putStrLn "non-cert PlainPtr bytestrings dump:" >> printCtorCensus noncerts'


groupedSrclocs :: [ClosurePtr] -> DebugM (Map.Map T.Text CensusStats)
groupedSrclocs = closureCensusBy go
  where
    go :: ClosurePtr -> SizedClosure -> DebugM (Maybe (T.Text, CensusStats))
    go cptr sc = do
      d <- hextraverse pure pure pure dereferenceConDesc pure pure sc
      ctorToKey (noSize d) <&> fmap (, mkCS cptr (dcSize d))
    ctorToKey ConstrClosure { info, ptrArgs, dataArgs, constrDesc = ConstrDesc{..} }
      | pkg == "base" && modl == "GHC.Stack.Types" && name == "SrcLoc"
        = do
            let (ptrs, ints) = splitAt 3 ptrArgs
            strargs <- fmtIntOrPtr `mapM` ints -- ptrArgs -- ints
            return . Just $ ("SrcLoc _ _ _ " <>) . T.intercalate " " $ strargs
    ctorToKey _ = return Nothing

groupedBytestrings :: [ClosurePtr] -> DebugM (Map.Map T.Text CensusStats)
groupedBytestrings = closureCensusBy go
  where
    go cptr sc = do
      d <- hextraverse pure pure pure dereferenceConDesc pure pure sc
      ctorToKey (noSize d) <&> fmap (\(t, l) -> (t, mkCS cptr (dcSize d + Size (fromIntegral l))))
    ctorToKey ConstrClosure { info, ptrArgs, dataArgs, constrDesc = ConstrDesc{..} }
      | name == "BS" && modl == "Data.ByteString.Internal.Type" -- && pkg == "bytestring-0.11.4.0"
        = let [ptrArg0] = ptrArgs; [dataArg0, dataArg1] = dataArgs in
          tryDerefPlainPtr ptrArg0 >>= \case
            Nothing -> return Nothing
            Just parrwords -> do
              arrwordsSC <- dereferenceClosure parrwords
              arrwordsDC <- hextraverse pure
                                        dereferenceSRT
                                        pure
                                        dereferenceConDesc
                                        pure
                                        pure
                                        arrwordsSC
              return . Just $
                ( T.pack . unwords $
                  [ "BS _", show dataArg1
                  , "-> ForeignPtr", showHex' dataArg0, "_"
                  , "-> PlainPtr _"
                  , "->", showHex' @Word64 (coerce parrwords)
                  , ppClosure (\_ _ -> "■") 0 $ noSize arrwordsDC
                  ]
                , dataArg1)
            -- = let (ptrs, ints) = splitAt 2 ptrArgs
            --   in fmtIntOrPtr `mapM` ptrArgs <&> Just
              -- . (T.pack pkg <>) . (":" <>)
              -- . (T.pack modl <>) . (":" <>)
              -- . (T.pack name <>) . (" [" <>)
              -- . (T.pack (show (length ptrArgs)) <>) . (" ptrArgs, " <>)
              -- . (T.pack (show (length dataArgs)) <>) . (" dataArgs] " <>)
              -- . T.intercalate " "
    ctorToKey _ = return Nothing

groupedByFptrCtor = closureCensusBy go
  where
    go cptr sc = do
      d <- hextraverse pure pure pure dereferenceConDesc pure pure sc
      ctorToKey (noSize d) <&> fmap (, mkCS cptr (dcSize d))
    ctorToKey ConstrClosure { info, ptrArgs, dataArgs, constrDesc = ConstrDesc{..} }
      | name == "BS" && modl == "Data.ByteString.Internal.Type"
        = let [ptrArg0] = ptrArgs in
          tryDerefForeignPtrCtor ptrArg0 <&> fmap T.pack
    ctorToKey _ = return Nothing

filterOutCerts = Map.filterWithKey (\k _ -> "-----BEGIN CERTIFICATE-----" `T.isInfixOf` k)

{- HLINT ignore "Redundant <&>" -}
tryDerefUnliftedInt :: ClosurePtr -> DebugM (Maybe Int)
tryDerefUnliftedInt p =
  dereferenceClosure p >>= hextraverse pure pure pure dereferenceConDesc pure pure
            <&> noSize >>= \case
    ConstrClosure {..} | name constrDesc == "I#"
      -> return . Just . fromIntegral $ head dataArgs
    _ -> return Nothing
fmtIntOrPtr p = tryDerefUnliftedInt p <&> T.pack . maybe (show p) show

tryDerefPlainPtr :: ClosurePtr -> DebugM (Maybe ClosurePtr)
tryDerefPlainPtr p =
  dereferenceClosure p >>= hextraverse pure pure pure dereferenceConDesc pure pure
            <&> noSize >>= \case
    ConstrClosure {..} | name constrDesc == "PlainPtr"
      -> return . Just $ head ptrArgs
    _ -> return Nothing

tryDerefMallocPtr :: ClosurePtr -> DebugM (Maybe ClosurePtr)
tryDerefMallocPtr p =
  dereferenceClosure p >>= hextraverse pure pure pure dereferenceConDesc pure pure
            <&> noSize >>= \case
    ConstrClosure {..} | name constrDesc == "MallocPtr"
      -> return . Just $ head ptrArgs
    _ -> return Nothing

tryDerefForeignPtrCtor :: ClosurePtr -> DebugM (Maybe String)
tryDerefForeignPtrCtor p =
  dereferenceClosure p >>= hextraverse pure pure pure dereferenceConDesc pure pure
            <&> noSize >>= \case
    ConstrClosure {..}
      -> return . Just $ name constrDesc
    _ -> return Nothing


printClosureCensus :: CensusByClosureType -> IO ()
printClosureCensus c = putStrLn header >> mapM_ putStrLn lines
  where
    header = "  count |   total   |   max   | constructor"
    lines = [ printf "%7d | %9d | %7d | %s" n sz ms (showK k)
            | (k, CS (Count n) (Size sz) (Max (Size ms)) (Sample _))
               <- sortOn (Down . cssize . snd)
               $ Map.toList c
            ]
    showK (k, ka) = let short = prettyShortProfileKey k; long = prettyProfileKey k in
                        if short == long then short else short <> " (" <> long <> ") "
                 <> prettyProfileKeyArgs ka

printClosureCensusDiff :: CensusByClosureType -> IO ()
printClosureCensusDiff c = putStrLn header >> mapM_ putStrLn lines
  where
    header = " Δcount |  Δtotal  | closure"
    lines = [ printf "%+7d | %+8d | %s" n sz (showK k)
            | (k, CS (Count n) (Size sz) (Max (Size ms)) (Sample _ss))
              <- sortOn (Down . cssize . snd)
              $ Map.toList c
            , sz /= 0 -- skip +0 lines
            ]
    showK (k, ka) = prettyProfileKey k <> prettyProfileKeyArgs ka

printCtorCensus :: Map.Map T.Text CensusStats -> IO ()
printCtorCensus c = putStrLn header >> mapM_ putStrLn lines
  where
    header = "  count |   total  |"
    lines = [ printf "%7d | %8d | %s" n s name
            | (name, CS (Count n) (Size s) (Max (Size ms)) _samples)
              <- sortOn (Down . cssize . snd)
              $ Map.toList c
            ]

printThunkDiff :: Map.Map (Maybe SourceInformation) Count -> IO ()
printThunkDiff m = putStrLn header >> mapM_ putStrLn lines
  where
    header = " Δcount |  SourceInformation"
    lines = [ printf "%7d | %s" c (maybe "NoLoc" showSourceInfo si)
            | (si, Count c) <- sortBy (comparing $ Down . getCount . snd)
                             $ Map.toList m
            ]

showSourceInfo :: SourceInformation -> String
showSourceInfo SourceInformation{..} = unwords $
  (if infoLabel == "" && infoType == ""
      then [] else [ infoLabel, "::", infoType , "\n--" ])
  ++
  [ show infoClosureType, infoName ]
  ++
  (if infoPosition == ":" then [] else [ "at", infoPosition ]) -- infoModule

indent :: Int -> String -> String
indent nch = let prefix = replicate nch ' ' in
                 dropWhileEnd (=='\n') . unlines . map (prefix <>) . lines

-- data RawBlock = RawBlock BlockPtr Word16 BS.ByteString
rawblockPtr (RawBlock bptr flags _bytes) = bptr

megablockFrag :: [Word64] -> Word64
megablockFrag bps = sum $ zipWith gap bps (tail bps)
  where gap (coerce -> b0) (coerce -> b1) =
          if b1 >= b0 + 1_048_576
            then b1 - b0 - 1_048_576
            else error $ "panic: unexpected alignment of " <> showHex' b0 <> " and " <> showHex' b1

-- https://gitlab.haskell.org/ghc/ghc-debug
-- https://industry.haskell.org/blog/2021/01/fragmentation-deeper-look/

-- | Print block usage summary
p2 :: Debuggee -> IO ()
p2 = withPaused $ \target -> do
  (bs, census) <- run target $ do
    bs <- precacheBlocks
    roots <- gcRoots
    (bs,) <$> censusPinnedBlocks bs roots
  summariseBlocks bs
  -- putStrLn "block histogram by usage level:"
  -- printBlockHistogram census
  -- putStrLn "megablock histogram by usage level:"
  -- printMBlockCensus (fmap projectCs census) -- this is buggy, doesn't add up

printBlockHistogram census = do
  printBlockCensus $ projectCs <$> census

projectCs (PinnedCensusStats (cs, _)) = cs
projectCptrs (PinnedCensusStats (_, pairs)) = fst <$> pairs

-- | Find retainer chains for objects in lowest-utilization blocks
p3 :: Debuggee -> IO ()
p3 = withPaused $ \target -> do
  retainstacks <- run target $ do
    bs <- precacheBlocks
    roots <- gcRoots
    census <- censusPinnedBlocks bs roots
    let lowestUsage = take 30 . sortWith (cssize . projectCs) . Map.elems $ census
    retainers <- traverse (findRetainersOf (Just 5) roots . projectCptrs) lowestUsage
    (\(rs, ix) -> traverse (fmap ("lowest-usage pinned block " ++ show ix,) . addLocationToStack') rs) `traverse` zip retainers ([1..]::[Int])
  putStrLn "retainer chains in 30 lowest-usage pinned blocks"
  mapM_ displayRetainerStack' retainstacks

showCensusStats CS{..} = unwords
  [ show cscount
  , "allocations of total size"
  , show . getSize $ cssize
  , "and largest"
  , show . getSize . getMax $ csmax
  ]

gatherSuiteshareInventory = do
  roots <- gcRoots
  totals <- GHC.Debug.Count.count roots
  suiteshareData <- flip focusCtorsBy roots $ \ConstrDesc{..} _ _ _ -> pure $
    "suiteshare-" `isPrefixOf` pkg
  navs <- flip focusCtorsBy suiteshareData $ \ConstrDesc{..} _ _ _ -> pure $
    modl == "SuiteShare.Types" && name == "Nav"
  lbls <- flip focusCtorsBy suiteshareData $ \ConstrDesc{..} _ _ _ -> pure $
    modl == "SuiteShare.Types" && name == "Label"
  cens <- censusClosureType suiteshareData
       <&> Map.filter ((> 1) . getCount . cscount) -- omit singleton allocs
  hgLbls <- multiBuildHeapGraph (Just 5) (head lbls :| tail lbls)
  pure (totals, suiteshareData, cens, navs, lbls, hgLbls)

summarizeSuiteshareInventory (totals0, ctors0, cens0, navs0, lbls0, lbls0hg)
                             (totals1, ctors1, cens1, navs1, lbls1, lbls1hg)
                             dbg = do
  summarizeHeapstatChange totals0 totals1 dbg
  printf "Data constructor allocations of zdocs package types: %d -> %d\n"
         (length ctors0) (length ctors1)
  printf "Alive count of SuiteShare.Types.Nav: %5d -> %-5d\n" (length navs0) (length navs1)
  printf "Alive count of SuiteShare.Types.Label: %5d -> %-5d\n" (length lbls0) (length lbls1)
  putStrLn "Allocs transitively rooted in zdocs package data-ctors, diff:"
  let diff = Map.differenceWith diffCensi cens1 cens0
  printClosureCensusDiff diff
  unless (length lbls0 >= length lbls1) $ do
    putStrLn "Increase in Label allocs detected, dumping heapgraphs..."
    writeFile "heapgraph.labels.prev.txt" $
      ppAcyclicHeapGraph ((<>" bytes") . show . getSize) lbls0hg
    writeFile "heapgraph.labels.next.txt" $
      ppAcyclicHeapGraph ((<>" bytes") . show . getSize) lbls1hg
    putStrLn "Wrote heapgraph.labels.{prev,next}.txt"

-- | "Cork"-style analysis, see https://dl.acm.org/doi/10.1145/1190216.1190224
type CorkDebugM a = StateT RankMaps IO a
gatherCorkState :: DebugM TypePointsFrom
gatherCorkState = gcRoots >>= typePointsFrom
summarizeCorkStep :: TypePointsFrom -> TypePointsFrom -> Debuggee -> CorkDebugM ()
summarizeCorkStep tpf0 tpf1 dbg = do
  State.modify' (\rm0 -> updateRankMap rm0 tpf0 tpf1)
  rankmaps <- State.get
  let candidates = chooseCandidates (fst rankmaps)
  graphvizSlices <- lift . run dbg
                  $ forM (take 10 candidates)
                  $ findSlice (snd rankmaps)
  liftIO $ do
    outputs <- zip [0..] graphvizSlices & mapM (\(n, g) -> do
      let fn = "slices/" ++ show @Int n ++ ".dot"
      writeFile fn (renderDot g)
      return fn
      )
    putStrLn $ "Written " <> show outputs

gatherCtorRetainers ctor = do
  roots <- gcRoots
  -- suiteshareData <- flip focusCtorsBy roots $ \ConstrDesc{..} _ _ _ -> pure $
  --   "suiteshare-" `isPrefixOf` pkg
  -- let filter = AndFilter (ConstructorDescFilter $ ("Label" ==) . name)
  --                        (NotFilter . InfoFilter $ (STACK ==) . tipe)
  let filter = ConstructorDescFilter $ (ctor ==) . name
  lblRetainers <- findRetainers (Just 2000) filter roots
  traverse addLocationToStack' lblRetainers
outputRetainers = putStrLn . ppRetainerStacks

ppRetainerStacks :: [[(ClosurePtr, SizedClosureP, Maybe SourceInformation)]] -> String
ppRetainerStacks = concat . zipWith fmt_one [0 :: Int ..] where
  fmt_one k stack = unlines $
    [ "sample " <> show k
    , "--------------------------------------------------------------------------------" ]
    ++ map fmt_clo (dropWhileEnd isTSO stack)
    ++ [""]
  fmt_clo (cptr, sc, mloc) =
    printf "%#12Lx: " (coerce @ClosurePtr @Word64 cptr)
    <> ppClosure (const show) 0 (noSize sc)
    <> maybe "" (("\n" <>) . indent 14 . showSourceInfo) mloc
  -- trim the boring & long TSO list at the end
  isTSO (_, noSize -> TSOClosure{}, _) = True
  isTSO _ = False

ppAcyclicHeapGraph :: (a -> String) -> HeapGraph a -> String
ppAcyclicHeapGraph ppUserData (HeapGraph (hgRoot :| hgRoots) hgMap) = unlines [
  -- hexCptr r ++ "(" ++ ppUserData (hgeData re) ++ ") " ++
  ppEntry 0 re
  | r <- hgRoot : hgRoots
  , let re = iToE r
  ]
  where
    ppEntry prec (hgeClosure -> clo)
      | Just i <- isInt clo = show i
      | Just c <- isChar clo = show c
      | Just s <- isString clo = show s
      | Just l <- isList clo = "[" ++ intercalate "," (map (ppRef 0) l) ++ "]"
      | Just t <- isText clo = show t
      | otherwise = ppClosure ppRef prec clo

    ppRef _ Nothing = "…"
    ppRef prec (Just i) = ppEntry prec (iToE i)

    iToE (ClosurePtr i) = hgMap IntMap.! fromIntegral i
    iToE' (ClosurePtr i) = IntMap.lookup (fromIntegral i) hgMap

    isList :: DebugClosure ccs srt p ConstrDesc s (Maybe HeapGraphIndex)
           -> Maybe [Maybe HeapGraphIndex]
    isList c
        | isNil c = return []
        | otherwise = do
            (h, t) <- isCons c
            ti <- t
            e <- iToE' ti
            t' <- isList (hgeClosure e)
            return $ (:) h t'

    isString :: DebugClosure ccs srt p ConstrDesc s (Maybe HeapGraphIndex) -> Maybe String
    isString e = do
        list <- isList e
        if null list -- don't print empty lists as "", isn't guaranteed to be [Char]
        then Nothing
        else mapM (isChar . hgeClosure <=< iToE' <=< id) list

    isText :: DebugClosure ccs srt p ConstrDesc s (Maybe HeapGraphIndex) -> Maybe T.Text
    isText ConstrClosure{ constrDesc = ConstrDesc {pkg = _, modl = "Data.Text.Internal", name = "Text"}, ptrArgs = [arrwords], dataArgs = [offset, len] } = do
      bytesI <- arrwords
      bytesE <- iToE' bytesI
      bytearr <- isArrWords (hgeClosure bytesE)
      pure . TE.decodeUtf8Lenient
           . BSL.toStrict
           . BSL.take (fromIntegral len)
           . BSL.drop (fromIntegral offset)
           $ bytearr
    isText _ = Nothing

isInt :: DebugClosure ccs srt p ConstrDesc s c -> Maybe Int
isInt ConstrClosure{ constrDesc = ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = "I#"}
                   , dataArgs = [i], ptrArgs = []}
        = Just (fromIntegral i)
isInt _ = Nothing

isChar :: DebugClosure ccs srt p ConstrDesc s c -> Maybe Char
isChar ConstrClosure{ constrDesc = ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = "C#"}
                    , dataArgs = [ch], ptrArgs = []}
        = Just (chr (fromIntegral ch))
isChar _ = Nothing

isNil :: DebugClosure ccs srt p ConstrDesc s c -> Bool
isNil ConstrClosure{ constrDesc = ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = "[]"}
                   , dataArgs = _, ptrArgs = []}
        = True
isNil _ = False

isCons :: DebugClosure ccs srt p ConstrDesc s c -> Maybe (c, c)
isCons ConstrClosure{ constrDesc = ConstrDesc {pkg = "ghc-prim", modl = "GHC.Types", name = ":"}
                    , dataArgs = [], ptrArgs = [h, t]}
         = Just (h, t)
isCons _ = Nothing

isArrWords :: DebugClosure ccs srt p ConstrDesc s c -> Maybe BSL.ByteString
isArrWords ArrWordsClosure{..} = return . BSL.take (fromIntegral bytes) . arrWordsBS $ arrWords
isArrWords _ = Nothing

tshow :: Show a => a -> T.Text
tshow = T.pack . show

hexCptr :: ClosurePtr -> String
hexCptr = showHex' @Word64 . coerce

showHex' :: (Integral i, Show i) => i -> String
showHex' n = "0x" <> showHex n ""

showMiB :: Integral i => i -> String
showMiB n = printf "%.1f MiB" (fromIntegral n / 1_048_576 :: Float)
