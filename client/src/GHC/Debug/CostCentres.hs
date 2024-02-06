{-# LANGUAGE BangPatterns #-}
module GHC.Debug.CostCentres
  ( findAllChildrenOfCC
  , findExactlyByCC
  , findAllCCSPayloads
  , traverseCCSPayloads
  -- * Helper functions for working with `IndexTable`'s
  , flattenIndexTable
  , traverseIndexTable
  , foldIndexTable
  -- * Efficient representation of CCSPtr sets
  , CCSSet(..)
  , memberCCSSet
  ) where

import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import GHC.Debug.Client
import GHC.Debug.Types.Ptr (CCSPtr(..))
import Data.Coerce (coerce)

newtype CCSSet = CCSSet IntSet

memberCCSSet :: CCSPtr -> CCSSet -> Bool
memberCCSSet (CCSPtr k) set = IntSet.member (fromIntegral k) (coerce set)

-- | Find all Cost Centre Stacks that reference precisely the cost centre with the given id.
findExactlyByCC :: (CCPayload -> Bool) -> DebugM (Set.Set CCSPtr)
findExactlyByCC isRelevantCC = do
  ccsMain <- requestCCSMain
  collectNode Set.empty ccsMain
  where
    collectNode :: Set.Set CCSPtr -> CCSPtr -> DebugM (Set.Set CCSPtr)
    collectNode !seen !ccsPtr = do
      ccsPl <- dereferenceCCS ccsPtr
      ccPl <- dereferenceCC (ccsCc ccsPl)
      let newSeen = if isRelevantCC ccPl
            then ccsPtr `Set.insert` seen
            else seen
      foldIndexTable (\_ ptr backEdge !seen' -> do
        if backEdge
          then pure seen'
          else collectNode seen' ptr)
        newSeen
        (ccsIndexTable ccsPl)

-- | Find all cost centre stack parts that are transitively children of the cost
-- centre with the given id.
findAllChildrenOfCC :: (CCPayload -> Bool) -> DebugM (Set.Set CCSPtr)
findAllChildrenOfCC isRelevantCC = do
  ccsMain <- requestCCSMain
  findCostCentre Set.empty ccsMain
  where
    findCostCentre :: Set.Set CCSPtr -> CCSPtr -> DebugM (Set.Set CCSPtr)
    findCostCentre !seen !ccsPtr = do
      ccsPl <- dereferenceCCS ccsPtr
      ccPl <- dereferenceCC (ccsCc ccsPl)
      if isRelevantCC ccPl
        then collectNodes seen ccsPtr
        else
          foldIndexTable  (\_ ptr backEdge !seen' -> do
            if backEdge
              then pure seen'
              else findCostCentre seen' ptr)
            seen
            (ccsIndexTable ccsPl)

    collectNodes :: Set.Set CCSPtr -> CCSPtr -> DebugM (Set.Set CCSPtr)
    collectNodes !seen !ccsPtr = do
      ccsPl <- dereferenceCCS ccsPtr
      foldIndexTable  (\_ ptr backEdge !seen' -> do
        let seen'' = ptr `Set.insert` seen'
        if backEdge
          then pure seen''
          else collectNodes seen'' ptr)
        (ccsPtr `Set.insert` seen)
        (ccsIndexTable ccsPl)

findAllCCSPayloads :: DebugM CCSSet
findAllCCSPayloads = do
  ccsMain <- requestCCSMain
  CCSSet <$> collectNodes IntSet.empty ccsMain
  where
    collectNodes :: IntSet.IntSet -> CCSPtr -> DebugM (IntSet.IntSet)
    collectNodes !seen ccsPtr@(CCSPtr w_) = do
      ccsPl <- dereferenceCCS ccsPtr
      foldIndexTable  (\_ ptr@(CCSPtr w) backEdge !seen' -> do
        let seen'' = fromIntegral w `IntSet.insert` seen'
        if backEdge
          then pure seen''
          else collectNodes seen'' ptr)
        (fromIntegral w_ `IntSet.insert` seen)
        (ccsIndexTable ccsPl)

traverseCCSPayloads :: DebugM ()
traverseCCSPayloads = do
  ccsMain <- requestCCSMain
  collectNodes ccsMain
  where
    collectNodes :: CCSPtr -> DebugM ()
    collectNodes ccsPtr = do
      ccsPl <- dereferenceCCS ccsPtr
      foldIndexTable  (\_ ptr backEdge () -> do
        if backEdge
          then pure ()
          else collectNodes ptr)
        ()
        (ccsIndexTable ccsPl)

traverseIndexTable :: Maybe IndexTablePtr -> (CCPtr -> CCSPtr -> Bool -> DebugM a) -> DebugM [a]
traverseIndexTable Nothing _ = pure []
traverseIndexTable (Just ptr) f = do
  idxTable <- dereferenceIndexTable ptr
  x <- f (itCostCentre idxTable) (itCostCentreStack idxTable) (itBackEdge idxTable)
  rest <- traverseIndexTable (itNext idxTable) f
  pure $ x:rest

foldIndexTable :: (CCPtr -> CCSPtr -> Bool -> a -> DebugM a) -> a -> Maybe IndexTablePtr -> DebugM a
foldIndexTable _f acc Nothing = pure acc
foldIndexTable f acc (Just ptr) = do
  idxTable <- dereferenceIndexTable ptr
  acc' <- f (itCostCentre idxTable) (itCostCentreStack idxTable) (itBackEdge idxTable) acc
  foldIndexTable f acc' (itNext idxTable)

-- | Flatten an optional index table pointer into a list of CCS Payloads.
flattenIndexTable :: Maybe IndexTablePtr -> DebugM [CCSPayload]
flattenIndexTable root = traverseIndexTable root (\_ ccsPtr _ -> dereferenceCCS ccsPtr)
