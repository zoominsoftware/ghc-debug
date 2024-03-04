module GHC.Debug.Count where

import           GHC.Debug.Types
import GHC.Debug.Client.Monad
import           GHC.Debug.Profile
import           GHC.Debug.Trace
import           GHC.Debug.ParTrace hiding (TraceFunctionsIO(..))
import GHC.Debug.ParTrace (TraceFunctionsIO(TraceFunctionsIO))
import Control.Monad.State


parCount :: [ClosurePtr] -> DebugM CensusStats
parCount = traceParFromM funcs . map (ClosurePtrWithInfo ())
  where
    nop = const (return mempty)
    nop2 = const (return mempty)
    funcs = TraceFunctionsIO nop nop nop clos (const (const (return mempty))) nop2 nop (const nop2)

    clos :: ClosurePtr -> SizedClosure -> ()
              -> DebugM ((), CensusStats, DebugM () -> DebugM ())
    clos cp sc _ = do
      return ((), mkCS cp (dcSize sc), id)

-- | Simple statistics about a heap, total objects, size and maximum object
-- size
count :: [ClosurePtr] -> DebugM CensusStats
count cps = snd <$> runStateT (traceFromM funcs cps) mempty
  where
    funcs = justClosures closAccum

    closAccum  :: ClosurePtr
               -> SizedClosure
               ->  (StateT CensusStats DebugM) ()
               ->  (StateT CensusStats DebugM) ()
    closAccum cp s k = do
      modify' (go cp s)
      k

    go :: ClosurePtr -> SizedClosure -> CensusStats -> CensusStats
    go cp sc cs = mkCS cp (dcSize sc) <> cs
