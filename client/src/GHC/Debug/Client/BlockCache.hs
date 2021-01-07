{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BinaryLiterals #-}
-- The BlockCache stores the currently fetched blocks
-- and is consulted first to avoid requesting too much
-- from the debuggee. The BlockCache can either be populated
-- via a call to RequestBlocks or on demand on a cache miss.

module GHC.Debug.Client.BlockCache(BlockCache, BlockCacheRequest(..)
                                  , handleBlockReq, emptyBlockCache, bcSize) where

import GHC.Debug.Types.Ptr
import GHC.Debug.Types
import qualified Data.HashMap.Strict as HM
import GHC.Word
import Data.Hashable
import Data.IORef
import Control.Concurrent
import System.IO
import GHC.Debug.Decode
import Data.Bits
import Data.List
import Data.Binary
import Data.Binary.Get
import Control.Applicative

data BlockCache = BlockCache (HM.HashMap Word64 RawBlock)

instance Binary BlockCache where
  get = BlockCache . HM.fromList <$> get
  put (BlockCache hm) = put (HM.toList hm)

emptyBlockCache :: BlockCache
emptyBlockCache = BlockCache HM.empty

addBlock :: RawBlock -> BlockCache -> BlockCache
addBlock rb@(RawBlock (BlockPtr bp) _ _) (BlockCache bc) =
  BlockCache (HM.insert bp rb bc)

-- 12 bits
bLOCK_MASK :: Word64
bLOCK_MASK = 0b111111111111

addBlocks :: [RawBlock] -> BlockCache -> BlockCache
addBlocks bc bs = Prelude.foldr addBlock bs bc

lookupClosure :: ClosurePtr -> BlockCache -> Maybe RawBlock
lookupClosure (ClosurePtr cp) (BlockCache b) =
  HM.lookup (cp .&. complement bLOCK_MASK) b

_applyBlockMask :: ClosurePtr -> ClosurePtr
_applyBlockMask (ClosurePtr cp) = ClosurePtr (cp .&. complement bLOCK_MASK)

bcSize :: BlockCache -> Int
bcSize (BlockCache b) = HM.size b

_bcKeys :: BlockCache -> [ClosurePtr]
_bcKeys (BlockCache b) = sort $ map ClosurePtr (HM.keys b)

allBlocks :: BlockCache -> [RawBlock]
allBlocks (BlockCache b) = HM.elems b

data BlockCacheRequest a where
  LookupClosure :: ClosurePtr -> BlockCacheRequest RawClosure
  PopulateBlockCache :: BlockCacheRequest [RawBlock]

deriving instance Show (BlockCacheRequest a)
deriving instance Eq (BlockCacheRequest a)

instance Hashable (BlockCacheRequest a) where
  hashWithSalt s (LookupClosure cpt) = s `hashWithSalt` (1 :: Int) `hashWithSalt` cpt
  hashWithSalt s PopulateBlockCache  = s `hashWithSalt` (2 :: Int)

handleBlockReq :: Maybe (MVar Handle) -> IORef BlockCache -> BlockCacheRequest resp -> IO resp
handleBlockReq mh ref (LookupClosure cp) = do
  bc <- readIORef ref
  let mrb = lookupClosure cp bc
  rb <- case mrb of
               Nothing -> do
                 -- print ("MISS", cp)
                 case mh of
                   Nothing -> error ("Cache Miss:" ++ show cp)
                   Just h -> do
                    rb <- doRequest h (RequestBlock cp)
                    atomicModifyIORef' ref (\bc' -> (addBlock rb bc', ()))
                    return rb
               Just rb -> do
                 return rb
  return (extractFromBlock cp rb)
handleBlockReq mh ref PopulateBlockCache = do
  bc <- readIORef ref
  blocks <- case mh of
              -- Snapshot mode, just return whatever blocks are cached
              Nothing -> return (allBlocks bc)
              -- Otherwise, get the blocks
              Just h -> doRequest h RequestAllBlocks
--  mapM_ (\rb -> print ("NEW", rawBlockAddr rb)) blocks
  print ("CACHING", length blocks)
  atomicModifyIORef' ref ((,()) . addBlocks blocks)
  return blocks


