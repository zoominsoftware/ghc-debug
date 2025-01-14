{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}
module Lib
  ( -- * Running/Connecting to a debuggee
    Debuggee
  , debuggeeRun
  , debuggeeConnect
  , snapshotConnect
  , debuggeeClose
  , withDebuggeeRun
  , withDebuggeeConnect
  , socketDirectory
  , snapshotDirectory

    -- * Pause/Resume
  , GD.pause
  , GD.resume
  , GD.pausePoll
  , GD.withPause

    -- * Querying the paused debuggee
  , rootClosures
  , savedClosures
  , version
  , profilingMode
  , GD.Version

    -- * Closures
  , Closure
  , ClosureType
  , DebugClosure(..)
  , closureShowAddress
  , closureExclusiveSize
  , closureSourceLocation
  , SourceInformation(..)
  , closureReferences
  , closurePretty
  , fillConstrDesc
  , InfoTablePtr
  , ListItem(..)
  , closureInfoPtr
  , infoSourceLocation
  , GD.dereferenceClosure
  , run
  , ccsReferences

    -- * Common initialisation
  , initialTraversal
  , HG.HeapGraph(..)
    -- * Dominator Tree
  , Size(..)
  , RetainerSize(..)
    -- * Reverse Edge Map
  , HG.mkReverseGraph
  , reverseClosureReferences
  , lookupHeapGraph

    -- * Profiling
  , profile
  , thunkAnalysis
  , GD.CensusStats(..)

    -- * Retainers
  , retainersOf
  , findAllChildrenOfCCs

  -- * Counting
  , arrWordsAnalysis
  , stringsAnalysis

    -- * Snapshot
  , snapshot

  -- * Types
  , Ptr(..)
  , CCSPtr
  , CCPayload
  , GenCCSPayload
  , toPtr
  , dereferencePtr
  , ConstrDesc(..)
  , ConstrDescCont
  , GenPapPayload(..)
  , StackCont
  , PayloadCont
  , SrtCont
  , ClosurePtr
  , readClosurePtr
  , CCPtr
  , readCCPtr
  , HG.StackHI
  , HG.PapHI
  , HG.SrtHI
  , HG.HeapGraphIndex
  , ProfHeaderWord
    --
  , EraRange(..)
  , GD.profHeaderInEraRange
  , tipe
  , ClosureFilter(..)
  , GD.profHeaderReferencesCCS
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (mapMaybe)
import qualified GHC.Debug.Types as GD
import           GHC.Debug.Types hiding (Closure, DebugClosure)
import           GHC.Debug.Convention (socketDirectory, snapshotDirectory)
import           GHC.Debug.Client.Monad (request, run, Debuggee)
import qualified GHC.Debug.Client.Monad as GD
import qualified GHC.Debug.Client.Query as GD
import qualified GHC.Debug.Profile as GD
import qualified GHC.Debug.Retainers as GD
import qualified GHC.Debug.CostCentres as GD
import           GHC.Debug.Retainers (EraRange(..), ClosureFilter(..))
import qualified GHC.Debug.Snapshot as GD
import qualified GHC.Debug.Strings as GD
import qualified GHC.Debug.Types.Version as GD
import qualified GHC.Debug.Types.Graph as HG
import qualified GHC.Debug.Thunks as GD
import Control.Monad
import System.FilePath
import System.Directory
import Control.Tracer
import Data.Bitraversable
import Data.Text (Text, pack)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set
import Data.Int
import GHC.Debug.Client.Monad (DebugM)
import Common

initialTraversal :: Debuggee -> IO (HG.HeapGraph Size)
initialTraversal e = run e $ do
    -- Calculate the dominator tree with retainer sizes
    -- TODO perhaps this conversion to a graph can be done in GHC.Debug.Types.Graph
    _ <- GD.precacheBlocks
    rs <- request RequestRoots
    let derefFuncM cPtr = do
          c <- GD.dereferenceClosure cPtr
          hextraverse pure GD.dereferenceSRT GD.dereferencePapPayload GD.dereferenceConDesc (bitraverse GD.dereferenceSRT pure <=< GD.dereferenceStack) pure c
    hg <- case rs of
      [] -> error "Empty roots"
      (x:xs) -> HG.multiBuildHeapGraph derefFuncM Nothing (x :| xs)
    return hg

-- This function is very very very slow, it needs to be optimised.
{-
runAnalysis :: Debuggee -> HG.HeapGraph Size -> IO Analysis
runAnalysis e hg = run e $ do
    let drs :: [G.Tree (ClosurePtr, (Size, RetainerSize))]
        drs = fmap (\ent -> (HG.hgeClosurePtr ent, HG.hgeData ent)) <$> HG.retainerSize hg

        !hmGraph = HM.unions (map snd $ foldTree buildGraphNode <$> (HG.retainerSize hg))

        buildGraphNode :: HG.HeapGraphEntry v
                       -> [(ClosurePtr, HM.HashMap ClosurePtr (v, [ClosurePtr]))]
                       -> (ClosurePtr, HM.HashMap ClosurePtr (v, [ClosurePtr]))
        buildGraphNode hge subtrees =
            (cptr, HM.insert cptr v (HM.unions submaps))
          where
            cptr = HG.hgeClosurePtr hge
            v = (HG.hgeData hge, children)
            (children, submaps) = unzip subtrees

        cPtrToData
          = fromMaybe ((-12221, RetainerSize (-12221)), [])
          -- ^ TODO I would expect the mapping to be complete unless out analysis misses some closures.
          . flip HM.lookup hmGraph

    return $ Analysis
              [drPtr | G.Node (drPtr, _) _ <- drs]
              ((\(_,x) -> x) . cPtrToData)
              ((\(x,_) -> x) . cPtrToData)
              -}

-- | Bracketed version of @debuggeeRun@. Runs a debuggee, connects to it, runs
-- the action, kills the process, then closes the debuggee.
withDebuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
                -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                -> (Debuggee -> IO a)
                -> IO a
withDebuggeeRun exeName socketName action = GD.withDebuggeeRun exeName socketName action

-- | Bracketed version of @debuggeeConnect@. Connects to a debuggee, runs the
-- action, then closes the debuggee.
withDebuggeeConnect :: FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                   -> (Debuggee -> IO a)
                   -> IO a
withDebuggeeConnect socketName action = GD.withDebuggeeConnect socketName action

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeRun :: FilePath  -- ^ path to executable to run as the debuggee
            -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
            -> IO Debuggee
debuggeeRun exeName socketName = GD.debuggeeRun exeName socketName

-- | Run a debuggee and connect to it. Use @debuggeeClose@ when you're done.
debuggeeConnect :: (Text -> IO ())
                -> FilePath  -- ^ filename of socket (e.g. @"/tmp/ghc-debug"@)
                -> IO Debuggee
debuggeeConnect toChan socketName = GD.debuggeeConnectWithTracer (contramap pack $ Tracer (emit toChan)) socketName


snapshotConnect :: (Text -> IO ()) -> FilePath -> IO Debuggee
snapshotConnect toChan snapshotName = GD.snapshotInitWithTracer (contramap pack $ Tracer (emit toChan)) snapshotName

-- | Close the connection to the debuggee.
debuggeeClose :: Debuggee -> IO ()
debuggeeClose = GD.debuggeeClose

-- | Request the debuggee's root pointers.
rootClosures :: Debuggee -> IO [Closure]
rootClosures e = run e $ do
  closurePtrs <- request RequestRoots
  closures <- GD.dereferenceClosures closurePtrs
  return [ Closure closurePtr' closure
            | closurePtr' <- closurePtrs
            | closure <- closures
            ]

version :: Debuggee -> IO GD.Version
version e = run e GD.version

profilingMode :: GD.Version -> Maybe GD.ProfilingMode
profilingMode = GD.v_profiling

-- | A client can save objects by calling a special RTS method
-- This function returns the closures it saved.
savedClosures :: Debuggee -> IO [Closure]
savedClosures e = run e $ do
  closurePtrs <- request RequestSavedObjects
  closures <- GD.dereferenceClosures closurePtrs
  return $ zipWith Closure
            closurePtrs
            closures

profile :: Debuggee -> ProfileLevel -> FilePath -> IO GD.CensusByClosureType
profile dbg lvl fp = do
  c <- run dbg $ do
    roots <- GD.gcRoots
    case lvl of
      OneLevel -> GD.censusClosureType roots
      TwoLevel -> GD.census2LevelClosureType roots
  GD.writeCensusByClosureType fp c
  return c

thunkAnalysis :: Debuggee -> IO (Map.Map (Maybe SourceInformation) GD.Count)
thunkAnalysis dbg = do
  c <- run dbg $ do
    roots <- GD.gcRoots
    GD.thunkAnalysis roots
  return c



snapshot :: Debuggee -> FilePath -> IO ()
snapshot dbg fp = do
  dir <- snapshotDirectory
  createDirectoryIfMissing True dir
  GD.run dbg $ GD.snapshot (dir </> fp)

retainersOf :: Maybe Int -> DebugM ClosureFilter -> Maybe [ClosurePtr] -> Debuggee -> IO [[Closure]]
retainersOf n retainer_filter mroots dbg = do
  run dbg $ do
    roots <- maybe GD.gcRoots return mroots
    closfilter <- retainer_filter
    stack <- GD.findRetainers n closfilter roots
    traverse (\cs -> zipWith Closure cs <$> (GD.dereferenceClosures cs)) stack

findAllChildrenOfCCs :: Int64 -> Debuggee -> IO (Set.Set CCSPtr)
findAllChildrenOfCCs ccId dbg = do
  run dbg $ do
    GD.findAllChildrenOfCC ((ccId ==) . ccID)

arrWordsAnalysis :: Maybe [ClosurePtr] -> Debuggee -> IO (Map.Map BS.ByteString (Set.Set ClosurePtr))
arrWordsAnalysis mroots dbg = do
  run dbg $ do
    roots <- maybe GD.gcRoots return mroots
    arr_words <- GD.arrWordsAnalysis roots
    return arr_words

stringsAnalysis :: Maybe [ClosurePtr] -> Debuggee -> IO (Map.Map String (Set.Set ClosurePtr))
stringsAnalysis mroots dbg = do
  run dbg $ do
    roots <- maybe GD.gcRoots return mroots
    arr_words <- GD.stringAnalysis roots
    return arr_words

-- -- | Request the description for an info table.
-- -- The `InfoTablePtr` is just used for the equality
-- requestConstrDesc :: Debuggee -> PayloadWithKey InfoTablePtr ClosurePtr -> IO ConstrDesc
-- requestConstrDesc (Debuggee e _) = run e $ request RequestConstrDesc

-- -- | Lookup source information of an info table
-- requestSourceInfo :: Debuggee -> InfoTablePtr -> IO [String]
-- requestSourceInfo (Debuggee e _) = run e $ request RequestSourceInfo

-- -- | Request a set of closures.
-- requestClosures :: Debuggee -> [ClosurePtr] -> IO [RawClosure]
-- requestClosures (Debuggee e _) = run e $ request RequestClosures

type Closure = DebugClosure CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr

data ListItem ccs srt a b c d
  = ListData
  | ListOnlyInfo InfoTablePtr
  | ListCCS CCSPtr (GenCCSPayload CCSPtr CCPayload)
  | ListCC CCPayload
  | ListFullClosure (DebugClosure ccs srt a b c d)

data DebugClosure ccs srt p cd s c
  = Closure
    { _closurePtr :: ClosurePtr
    , _closureSized :: DebugClosureWithSize ccs srt p cd s c
    }
  | Stack
    { _stackPtr :: StackCont
    , _stackStack :: GD.GenStackFrames srt c
    }
  deriving Show

toPtr :: DebugClosure ccs srt p cd s c -> Ptr
toPtr (Closure cp _) = CP cp
toPtr (Stack sc _)   = SP sc

data Ptr = CP ClosurePtr | SP StackCont deriving (Eq, Ord)


dereferencePtr :: Debuggee -> Ptr -> IO (DebugClosure CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr)
dereferencePtr dbg (CP cp) = run dbg (Closure <$> pure cp <*> GD.dereferenceClosure cp)
dereferencePtr dbg (SP sc) = run dbg (Stack <$> pure sc <*> GD.dereferenceStack sc)

instance Hextraversable DebugClosure where
  hextraverse p f g h i j (Closure cp c) = Closure cp <$> hextraverse p f g h i j c
  hextraverse _ p _ _ _ h (Stack sp s) = Stack sp <$> bitraverse p h s

closureShowAddress :: DebugClosure ccs srt p cd s c -> String
closureShowAddress (Closure c _) = show c
closureShowAddress (Stack  (StackCont s _) _) = show s

-- | Get the exclusive size (not including any referenced closures) of a closure.
closureExclusiveSize :: DebugClosure ccs srt p cd s c -> Size
closureExclusiveSize (Stack{}) = Size (-1)
closureExclusiveSize (Closure _ c) = (GD.dcSize c)

closureSourceLocation :: Debuggee -> DebugClosure ccs srt p cd s c -> IO (Maybe SourceInformation)
closureSourceLocation _ (Stack _ _) = return Nothing
closureSourceLocation e (Closure _ c) = run e $ do
  request (RequestSourceInfo (tableId (info (noSize c))))

closureInfoPtr :: DebugClosure ccs srt p cd s c -> Maybe InfoTablePtr
closureInfoPtr (Stack {}) = Nothing
closureInfoPtr (Closure _ c) = Just (tableId (info (noSize c)))

infoSourceLocation :: Debuggee -> InfoTablePtr -> IO (Maybe SourceInformation)
infoSourceLocation e ip = run e $ request (RequestSourceInfo ip)

-- | Get the directly referenced closures (with a label) of a closure.
closureReferences :: Debuggee -> DebugClosure CCSPtr SrtCont PayloadCont ConstrDesc StackCont ClosurePtr -> IO [(String, ListItem CCSPtr SrtCont PayloadCont ConstrDescCont StackCont ClosurePtr)]
closureReferences e (Stack _ stack) = run e $ do
  stack' <- bitraverse GD.dereferenceSRT pure stack
  let action (GD.SPtr ptr) = ("Pointer", ListFullClosure . Closure ptr <$> GD.dereferenceClosure ptr)
      action (GD.SNonPtr dat) = ("Data:" ++ show dat, return ListData)

--      frame_items :: DebugStackFrame
--                        (GenSrtPayload ClosurePtr) ClosurePtr -> GD.DebugM [(String, _)]
      frame_items frame = do
          info <- GD.getSourceInfo (tableId (frame_info frame))
          case info of
            Just (SourceInformation {infoName = "stg_orig_thunk_info_frame_info"}) ->
              let [GD.SNonPtr dat] = GD.values frame
              in return [("Blackhole arising from thunk:", (ListOnlyInfo (InfoTablePtr dat)))]
            _ -> traverse sequenceA $

             ("Info: " ++ show (tableId (frame_info frame)), return (ListOnlyInfo (tableId (frame_info frame)))) :
             [ ("SRT: ", ListFullClosure . Closure srt <$> GD.dereferenceClosure srt)  | Just srt <- [getSrt (frame_srt frame)]]
             ++ map action (GD.values frame)

      add_frame_ix ix (lbl, x) = ("Frame " ++ show ix ++ " " ++ lbl, x)
  lblAndPtrs <- sequence [ map (add_frame_ix frameIx) <$> (frame_items frame)
                            | (frameIx, frame) <- zip [(0::Int)..] (GD.getFrames stack')
                            ]
  return (concat lblAndPtrs)
  {-
  return $ zipWith (\(lbl,ptr) c -> (lbl, Closure ptr c))
            lblAndPtrs
            closures
            -}
closureReferences e (Closure _ closure) = run e $ do
  closure' <- hextraverse pure GD.dereferenceSRT GD.dereferencePapPayload pure pure pure closure
  let wrapClosure cPtr = do
        refClosure' <- GD.dereferenceClosure cPtr
        return $ ListFullClosure $ Closure cPtr refClosure'
      wrapStack sPtr = do
        refStack' <- GD.dereferenceStack sPtr
        return $ ListFullClosure $ Stack sPtr refStack'
      wrapCCS ccsPtr = do
        refCCS <- do
          GD.dereferenceCCS ccsPtr >>= \ccs ->
            bitraverse pure GD.dereferenceCC ccs
        return $ ListCCS ccsPtr refCCS
  closureReferencesAndLabels wrapClosure
                             wrapStack
                             wrapCCS
                             (unDCS closure')

ccsReferences :: Debuggee -> GenCCSPayload CCSPtr CCPayload -> IO [ListItem ccs srt a b c d]
ccsReferences e initialCcs = run e $ (ListCC (ccsCc initialCcs) :) <$> go initialCcs
  where
    go ccs = do
      case ccsPrevStack ccs of
        Nothing -> pure [ListCC (ccsCc ccs)]
        Just ccsPtr -> do
          child <- GD.dereferenceCCS ccsPtr
          child' <- bitraverse pure GD.dereferenceCC child
          children <- go child'
          return (ListCC (ccsCc child') : children)

reverseClosureReferences :: HG.HeapGraph Size
                         -> HG.ReverseGraph
                         -> Debuggee
                         -> DebugClosure CCSPtr HG.SrtHI HG.PapHI ConstrDesc HG.StackHI (Maybe HG.HeapGraphIndex)
                         -> IO [(String, DebugClosure
                                            CCSPtr
                                            HG.SrtHI
                                            HG.PapHI
                                            ConstrDesc HG.StackHI
                                            (Maybe HG.HeapGraphIndex))]
reverseClosureReferences hg rm _ c =
  case c of
    Stack {} -> error "Nope - Stack"
    Closure cp _ -> case (HG.reverseEdges cp rm) of
                      Nothing -> return []
                      Just es ->
                        let revs = mapMaybe (flip HG.lookupHeapGraph hg) es
                        in return [(show n, Closure (HG.hgeClosurePtr hge)
                                               (DCS (HG.hgeData hge) (HG.hgeClosure hge) ))
                                    | (n, hge) <- zip [0 :: Int ..] revs]

lookupHeapGraph :: HG.HeapGraph Size -> ClosurePtr -> Maybe (DebugClosure CCSPtr HG.SrtHI HG.PapHI ConstrDesc HG.StackHI (Maybe HG.HeapGraphIndex))
lookupHeapGraph hg cp =
  case HG.lookupHeapGraph cp hg of
    Just (HG.HeapGraphEntry ptr d s) -> Just (Closure ptr (DCS s d))
    Nothing -> Nothing

fillConstrDesc :: Debuggee
               -> DebugClosure ccs srt pap ConstrDescCont s c
               -> IO (DebugClosure ccs srt pap ConstrDesc s c)
fillConstrDesc e closure = do
  run e $ GD.hextraverse pure pure pure GD.dereferenceConDesc pure pure closure

-- | Pretty print a closure
closurePretty :: Debuggee -> DebugClosure CCSPtr InfoTablePtr PayloadCont ConstrDesc s ClosurePtr ->  IO String
closurePretty _ (Stack _ frames) = return $ (show (length frames) ++ " frames")
closurePretty dbg (Closure _ closure) = run dbg $  do
  closure' <- hextraverse pure GD.dereferenceSRT GD.dereferencePapPayload pure pure pure closure
  return $ HG.ppClosure
    (\_ refPtr -> show refPtr)
    0
    (unDCS closure')

-- Internal Stuff
--

closureReferencesAndLabels :: Monad m => (pointer -> m a) -> (stack -> m a) -> (ccs -> m a) -> GD.DebugClosure ccs (GenSrtPayload pointer) PapPayload string stack pointer -> m [(String, a)]
closureReferencesAndLabels pointer stack fccs closure = sequence . map sequence $ [("CCS", fccs (ccs ph)) | Just ph <- pure (profHeader closure)] ++ case closure of
  TSOClosure {..} ->
    [ ("Thread label", pointer lbl) | Just lbl <- pure threadLabel ] ++
    [ ("Stack", pointer tsoStack)
    , ("Link", pointer _link)
    , ("Global Link", pointer global_link)
    , ("TRec", pointer trec)
    , ("Blocked Exceptions", pointer blocked_exceptions)
    , ("Blocking Queue", pointer bq)
    ]
  StackClosure{..} -> [("Frames", stack frames )]
  WeakClosure {..} -> [ ("Key", pointer key)
                      , ("Value", pointer value)
                      , ("C Finalizers", pointer cfinalizers)
                      , ("Finalizer", pointer finalizer)
                      ] ++
                      [ ("Link", pointer link)
                      | Just link <- [mlink] -- TODO do we want to show NULL pointers some how?
                      ]
  TVarClosure {..} -> [("val", pointer current_value)]
  MutPrimClosure {..} -> withArgLables ptrArgs
  PrimClosure{..} -> withArgLables ptrArgs
  ConstrClosure {..} -> withFieldLables ptrArgs
  ThunkClosure {..} ->  [("SRT", pointer cp) | Just cp <- [getSrt srt]]
                     ++ withArgLables ptrArgs
  SelectorClosure {..} -> [("Selectee", pointer selectee)]
  IndClosure {..} -> [("Indirectee", pointer indirectee)]
  BlackholeClosure {..} -> [("Indirectee", pointer indirectee)]
  APClosure {..} -> ("Function", pointer fun) : [] -- TODO withBitmapLables ap_payload
  PAPClosure {..} -> ("Function", pointer fun) : [] -- TODO: withBitmapLables pap_payload
  APStackClosure {..} -> ("Function", pointer fun) : ("Frames", stack payload) : []
  BCOClosure {..} -> [ ("Instructions", pointer instrs)
                      , ("Literals", pointer literals)
                      , ("Byte Code Objects", pointer bcoptrs)
                      ]
  ArrWordsClosure {} -> []
  MutArrClosure {..} -> withIxLables mccPayload
  SmallMutArrClosure {..} -> withIxLables mccPayload
  MutVarClosure {..} -> [("Value", pointer var)]
  MVarClosure {..} -> [ ("Queue Head", pointer queueHead)
                      , ("Queue Tail", pointer queueTail)
                      , ("Value", pointer value)
                      ]
  FunClosure {..} ->
       [ ("SRT", pointer cp) | Just cp <- [getSrt srt]]
    ++ withArgLables ptrArgs
  BlockingQueueClosure {..} -> [ ("Link", pointer link)
                                , ("Black Hole", pointer blackHole)
                                , ("Owner", pointer owner)
                                , ("Queue", pointer queue)
                                ]
  OtherClosure {..} -> ("",) . pointer <$> hvalues
  TRecChunkClosure{}  -> [] --TODO
  UnsupportedClosure {} -> []
  where
  withIxLables elements   = [("[" <> show i <> "]" , pointer x) | (i, x) <- zip [(0::Int)..] elements]
  withArgLables ptrArgs   = [("Argument " <> show i, pointer x) | (i, x) <- zip [(0::Int)..] ptrArgs]
  withFieldLables ptrArgs = [("Field " <> show i   , pointer x) | (i, x) <- zip [(0::Int)..] ptrArgs]
--  withBitmapLables pap = [("Argument " <> show i   , Left x) | (i, SPtr x) <- zip [(0::Int)..] (getValues pap)]

--
