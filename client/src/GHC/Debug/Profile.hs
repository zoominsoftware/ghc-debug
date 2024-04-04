{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

{- | Functions for performing whole heap census in the style of the normal
- heap profiling -}
module GHC.Debug.Profile( censusClosureType
                        , census2LevelClosureType
                        , closureCensusBy
                        , CensusByClosureType
                        , writeCensusByClosureType
                        , CensusStats(..)
                        , ProfileKey(..)
                        , ProfileKeyArgs(..)
                        , prettyProfileKey
                        , prettyShortProfileKey
                        , prettyProfileKeyArgs
                        , prettyProfileKeyArgs'
                        , prettyShortProfileKeyArgs
                        , mkCS
                        , Count(..)
                        , closureToKey
                        , ConstrDescText
                        , packConstrDesc
                        , pkgsText
                        , modlText
                        , nameText
                        ) where

import GHC.Debug.Client.Monad
import GHC.Debug.Client
import GHC.Debug.Trace
import GHC.Debug.ParTrace
import GHC.Debug.Profile.Types

import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Data.List (sortBy)
import Data.Ord
import Data.Text (pack, Text)
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Monoidal.Strict as MMap
import Data.Bitraversable
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Vector as V

--import Control.Concurrent
--import Eventlog.Types
--import Eventlog.Data
--import Eventlog.Total
--import Eventlog.HtmlTemplate
--import Eventlog.Args (defaultArgs, Option(..))

type CensusByClosureType = Map.Map (ProfileKey, ProfileKeyArgs) CensusStats

-- | Perform a heap census in the same style as the -hT profile.
censusClosureType :: [ClosurePtr] -> DebugM CensusByClosureType
censusClosureType = closureCensusBy go
  where
    go :: ClosurePtr -> SizedClosure
       -> DebugM (Maybe ((ProfileKey, ProfileKeyArgs), CensusStats))
    go cp s = do
      d <- hextraverse pure pure pure dereferenceConDesc pure pure s
      let siz :: Size
          siz = dcSize d
          v =  mkCS cp siz
      return $ Just ((closureToProfileKey (noSize d), NoArgs), v)

closureToKey :: DebugClosure ccs srt a ConstrDesc c d -> Text
closureToKey d =
  case d of
     ConstrClosure { constrDesc = ConstrDesc a b c }
       -> pack a <> ":" <> pack b <> ":" <> pack c
     _ -> pack (show (tipe (decodedTable (info d))))

-- | 'ConstrDescText' wraps a 'ConstrDesc' but is backed by a 'Text'.
--
-- More efficient to keep around than 'ConstrDesc'.
newtype ConstrDescText = ConstrDescText
  { descText :: Text
    -- ^ Contains the name, module name and package name. Values are separated by ';'.
  } deriving (Show, Ord, Eq)

pkgsText :: ConstrDescText -> Text
pkgsText desc = case T.splitOn ";" (descText desc) of
  _:_:pkgs:_ -> pkgs
  _ -> error $ "pkgsText: invariant violation: " <> T.unpack (descText desc)


modlText :: ConstrDescText -> Text
modlText desc = case T.splitOn ";" (descText desc) of
  _:modl:_:_ -> modl
  _ -> error $ "modlText: invariant violation: " <> T.unpack (descText desc)

nameText  :: ConstrDescText -> Text
nameText desc = case T.splitOn ";" (descText desc) of
  name:_:_:_ -> name
  _ -> error $ "nameText: invariant violation: " <> T.unpack (descText desc)

packConstrDesc :: ConstrDesc -> ConstrDescText
packConstrDesc constrDesc = ConstrDescText
  { descText = T.intercalate ";" [T.pack (name constrDesc), T.pack (modl constrDesc), T.pack (pkg constrDesc)]
  }

data ProfileKey
  = ProfileConstrDesc !ConstrDescText
  | ProfileClosureDesc !Text
  deriving (Show, Ord, Eq)

-- | Show the full 'ProfileKey', including package and module locations if available.
prettyProfileKey :: ProfileKey -> Text
prettyProfileKey (ProfileClosureDesc k) = k
prettyProfileKey (ProfileConstrDesc desc) = pkgsText desc <> ":" <> modlText desc <> ":" <> nameText desc

-- | Show the 'ProfileKey' in a shortened form if possible.
-- For example, it omits package and module locations for 'ProfileConstrDesc'.
prettyShortProfileKey :: ProfileKey -> Text
prettyShortProfileKey (ProfileClosureDesc k) = k
prettyShortProfileKey (ProfileConstrDesc desc) = nameText desc

closureToProfileKey :: DebugClosure ccs srt a ConstrDesc c d -> ProfileKey
closureToProfileKey d =
  case d of
     ConstrClosure { constrDesc = constrDesc } -> ProfileConstrDesc $ packConstrDesc constrDesc
     _ -> ProfileClosureDesc $ pack (show (tipe (decodedTable (info d))))
data ProfileKeyArgs
  = ArrKeyArgs !ProfileKey !Int
  | AllKeyArgs !(V.Vector ProfileKey)
  | NoArgs
  deriving (Show, Ord, Eq)

prettyProfileKeyArgs :: ProfileKeyArgs -> Text
prettyProfileKeyArgs = prettyProfileKeyArgs' prettyProfileKey

prettyShortProfileKeyArgs :: ProfileKeyArgs -> Text
prettyShortProfileKeyArgs = prettyProfileKeyArgs' prettyShortProfileKey

prettyProfileKeyArgs' :: (ProfileKey -> Text) -> ProfileKeyArgs -> Text
prettyProfileKeyArgs' prettyKey (ArrKeyArgs typ num) = "[" <> prettyKey typ <> ": " <> T.pack (show num) <> "]"
prettyProfileKeyArgs' prettyKey (AllKeyArgs args) = "[" <> T.intercalate "," (map prettyKey $ V.toList args) <> "]"
prettyProfileKeyArgs' _  NoArgs = ""

-- | General function for performing a heap census in constant memory
closureCensusBy :: forall k v . (Semigroup v, Ord k)
                => (ClosurePtr -> SizedClosure -> DebugM (Maybe (k, v)))
                -> [ClosurePtr] -> DebugM (Map.Map k v)
closureCensusBy f cps = do
  () <$ precacheBlocks
  MMap.getMonoidalMap <$> traceParFromM funcs (map (ClosurePtrWithInfo ()) cps)
  where
    funcs = TraceFunctionsIO {
               papTrace = const (return ())
              , srtTrace = const (return ())
              , stackTrace = const (return ())
              , closTrace = closAccum
              , visitedClosVal = const (const (return MMap.empty))
              , visitedCcsVal = const (return MMap.empty)
              , conDescTrace = const (return ())
              , ccsTrace = const (const (return mempty))
            }
    -- Add cos
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> ()
               -> DebugM ((), MMap.MonoidalMap k v, a -> a)
    closAccum cp s () = do
      r <- f cp s
      return . (\s' -> ((), s', id)) $ case r of
        Just (k, v) -> MMap.singleton k v
        Nothing -> MMap.empty

-- | Perform a 2-level census where the keys are the type of the closure
-- in addition to the type of ptrs of the closure. This can be used to
-- distinguish between lists of different type for example.
census2LevelClosureType :: [ClosurePtr] -> DebugM CensusByClosureType
census2LevelClosureType cps = snd <$> runStateT (traceFromM funcs cps) Map.empty
  where
    funcs = justClosures closAccum
    -- Add cos
    closAccum  :: ClosurePtr
               -> SizedClosure
               -> (StateT CensusByClosureType DebugM) ()
               -> (StateT CensusByClosureType DebugM) ()
    closAccum cp s k = do
      s' <- lift $ hextraverse pure dereferenceSRT dereferencePapPayload dereferenceConDesc (bitraverse dereferenceSRT pure <=< dereferenceStack) pure s
      pts <- lift $ mapM dereferenceClosure (allClosures (noSize s'))
      pts' <- lift $ mapM (hextraverse pure pure pure dereferenceConDesc pure pure) pts


      modify' (go cp s' pts')
      k

    closureArgsToKeyArgs (ProfileClosureDesc k) kargs =
      if k `Set.member` mutArrConstants && Set.size (Set.fromList kargs) == 1
        then ArrKeyArgs (head kargs) (length kargs)
        else AllKeyArgs $! V.fromList kargs
    closureArgsToKeyArgs (ProfileConstrDesc _) kargs =
      AllKeyArgs $ V.fromList kargs

    go cp d args =
      let !k = closureToProfileKey (noSize d)
          kargs = map (closureToProfileKey . noSize) args
          !keyArgs = closureArgsToKeyArgs k kargs
      in Map.insertWith (<>) (k, keyArgs) (mkCS cp (dcSize d))

    -- We handle these closure types differently as they can list each entry as an arg.
    -- That leads to huge results, so we try to compress these closure types if and only if
    -- they describe a constructor homogenous array. Thus, it works well for product types
    -- but not for sum types.
    mutArrConstants = Set.fromList $ map (T.pack . show)
      [ MUT_ARR_PTRS_CLEAN
      , MUT_ARR_PTRS_DIRTY
      , MUT_ARR_PTRS_FROZEN_DIRTY
      , MUT_ARR_PTRS_FROZEN_CLEAN
      , SMALL_MUT_ARR_PTRS_CLEAN
      , SMALL_MUT_ARR_PTRS_DIRTY
      , SMALL_MUT_ARR_PTRS_FROZEN_DIRTY
      , SMALL_MUT_ARR_PTRS_FROZEN_CLEAN
      ]

{-
-- | Parallel heap census
parCensus :: [RawBlock] -> [ClosurePtr] -> DebugM (Map.Map Text CensusStats)
parCensus bs cs =  do
  MMap.getMonoidalMap <$> (traceParFromM bs funcs (map (ClosurePtrWithInfo ()) cs))

  where
    nop = const (return ())
    funcs = TraceFunctionsIO nop nop clos  (const (const (return mempty))) nop

    clos :: ClosurePtr -> SizedClosure -> ()
              -> DebugM ((), MMap.MonoidalMap Text CensusStats, DebugM () -> DebugM ())
    clos _cp sc () = do
      d <- hextraverse pure dereferenceConDesc pure pure sc
      let s :: Size
          s = dcSize sc
          v =  mkCS s
      return $ ((), MMap.singleton (closureToKey (noSize d)) v, id)
      -}


writeCensusByClosureType :: FilePath -> CensusByClosureType -> IO ()
writeCensusByClosureType outpath c = do
  let res = sortBy (flip (comparing (cssize . snd))) (Map.toList c)
  T.writeFile outpath (T.unlines $ "key; total; count; max; avg" : map showLine res)
  where
    separator = "; "
    showKey k args = prettyProfileKey k <> prettyProfileKeyArgs args
    showLine ((k, kargs), CS (Count n) (Size s) (Max (Size mn)) _) =
      T.intercalate separator
        [ showKey k kargs
        , T.pack (show s)
        , T.pack (show n)
        , T.pack (show mn)
        , T.pack (show @Double (fromIntegral s / fromIntegral n))
        ]

{-
-- | Peform a profile at the given interval (in seconds), the result will
-- be rendered after each iteration using @eventlog2html@.
profile :: FilePath -> Int -> Debuggee -> IO ()
profile outpath interval e = loop [(0, Map.empty)] 0
  where
    loop :: [(Int, CensusByClosureType)] -> Int -> IO ()
    loop ss i = do
      threadDelay (interval * 1_000_000)
      pause e
      r <- runTrace e $ do
        precacheBlocks
        rs <- gcRoots
        traceWrite (length rs)
        census2LevelClosureType rs
      resume e
      writeCensusByClosureType outpath r
      let new_data = ((i + 1) * interval, r) : ss
      renderProfile new_data
      loop new_data (i + 1)


mkFrame :: (Int, CensusByClosureType) -> Frame
mkFrame (t, m) = Frame (fromIntegral t / 10e6) (Map.foldrWithKey (\k v r -> mkSample k v : r) [] m)

mkSample :: Text -> CensusStats -> Sample
mkSample k (CS _ (Size v) _) =
  Sample (Bucket k) (fromIntegral v)

mkProfData :: [(Int, CensusByClosureType)] -> ProfData
mkProfData raw_fs =
  let fs = map mkFrame raw_fs
      (counts, totals) = total fs
      -- Heap profiles don't contain any other information than the simple bucket name
      binfo = Map.mapWithKey (\(Bucket k) (t,s,g) -> BucketInfo k Nothing t s g) totals
  -- Heap profiles do not support traces
      header = Header "ghc-debug" "" (Just HeapProfBreakdownClosureType) "" "" "" counts Nothing
  in ProfData header binfo mempty fs [] (HeapInfo [] [] []) mempty

renderProfile :: [(Int, CensusByClosureType)] -> IO ()
renderProfile ss = do
  let pd = mkProfData ss
  Run as <- defaultArgs "unused"
  (header, data_json, descs, closure_descs) <- generateJsonData as pd
  let html = templateString header data_json descs closure_descs as
  writeFile "profile/ht.html" html
  return ()
  -}


