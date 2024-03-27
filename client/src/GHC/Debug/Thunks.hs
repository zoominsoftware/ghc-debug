module GHC.Debug.Thunks where

import GHC.Debug.Types
import GHC.Debug.Client.Monad
import GHC.Debug.Profile.Types
import qualified Data.Map.Strict as Map
import Control.Monad.RWS
import GHC.Debug.Trace
import GHC.Debug.Client.Query


thunkAnalysis :: [ClosurePtr] -> DebugM (Map.Map (Maybe SourceInformation) Count)
thunkAnalysis rroots = (\(_, r, _) -> r) <$> runRWST (traceFromM funcs rroots) () (Map.empty)
  where
    funcs = justClosures closAccum

    getSourceLoc c = getSourceInfo (tableId (info (noSize c)))

    closAccum  :: ClosurePtr
               -> SizedClosure
               -> (RWST () () (Map.Map (Maybe SourceInformation) Count) DebugM) ()
               -> (RWST () () (Map.Map (Maybe SourceInformation) Count) DebugM) ()
    closAccum _ sc k = do
          case (noSize sc) of
            ThunkClosure {} ->  do
              loc <- lift $ getSourceLoc sc
              modify' (Map.insertWith (<>) loc (Count 1))
              k
            _ -> k
