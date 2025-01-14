{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RankNTypes #-}
-- The BlockCache stores the currently fetched blocks
-- and is consulted first to avoid requesting too much
-- from the debuggee. The BlockCache can either be populated
-- via a call to RequestBlocks or on demand on a cache miss.

module GHC.Debug.Client.BlockCache(BlockCache, BlockCacheRequest(..)
                                  , handleBlockReq, emptyBlockCache, bcSize, addBlocks) where

import GHC.Debug.Types.Ptr
import GHC.Debug.Types
import qualified Data.HashMap.Strict as HM
import GHC.Word
import Data.Hashable
import Data.IORef
import Data.Bits
import Data.List (sort)
import Data.Binary
import Control.Tracer

newtype BlockCache = BlockCache (HM.HashMap Word64 RawBlock)

instance Binary BlockCache where
  get = BlockCache . HM.fromList <$> get
  put (BlockCache hm) = put (HM.toList hm)

emptyBlockCache :: BlockCache
emptyBlockCache = BlockCache HM.empty

addBlock :: RawBlock -> BlockCache -> BlockCache
addBlock rb@(RawBlock (BlockPtr bp) _ _) (BlockCache bc) =
  BlockCache (HM.insert bp rb bc)


addBlocks :: [RawBlock] -> BlockCache -> BlockCache
addBlocks bc bs = Prelude.foldr addBlock bs bc

lookupClosure :: ClosurePtr -> BlockCache -> Maybe RawBlock
lookupClosure (ClosurePtr cp) (BlockCache b) =
  HM.lookup (cp .&. complement blockMask) b

bcSize :: BlockCache -> Int
bcSize (BlockCache b) = HM.size b

_bcKeys :: BlockCache -> [ClosurePtr]
_bcKeys (BlockCache b) = sort $ map mkClosurePtr (HM.keys b)

data BlockCacheRequest a where
  LookupClosure :: ClosurePtr -> BlockCacheRequest RawClosure
  PopulateBlockCache :: BlockCacheRequest [RawBlock]

deriving instance Show (BlockCacheRequest a)
deriving instance Eq (BlockCacheRequest a)

instance Hashable (BlockCacheRequest a) where
  hashWithSalt s (LookupClosure cpt) = s `hashWithSalt` (1 :: Int) `hashWithSalt` cpt
  hashWithSalt s PopulateBlockCache  = s `hashWithSalt` (2 :: Int)

handleBlockReq :: Tracer IO String -> (forall a . Request a -> IO a) -> IORef BlockCache -> BlockCacheRequest resp -> IO resp
handleBlockReq _ do_req ref (LookupClosure cp) = do
  bc <- readIORef ref
  let mrb = lookupClosure cp bc
  rb <- case mrb of
               Nothing -> do
                  rb <- do_req (RequestBlock cp)
                  atomicModifyIORef' ref (\bc' -> (addBlock rb bc', ()))
                  return rb
               Just rb -> do
                 return rb
  return (extractFromBlock cp rb)
handleBlockReq tracer do_req ref PopulateBlockCache = do
  blocks <- do_req RequestAllBlocks
--  mapM_ (\rb -> print ("NEW", rawBlockAddr rb)) blocks
  traceWith tracer $ "Populating block cache with " ++ show (length blocks) ++ " blocks"
  atomicModifyIORef' ref ((,()) . addBlocks blocks)
  return blocks



