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
import System.Endian
import Data.Hashable
import Data.IORef
import System.IO
import GHC.Debug.Decode
import Data.Bits

data BlockCache = BlockCache (HM.HashMap Word64 RawBlock)

emptyBlockCache :: BlockCache
emptyBlockCache = BlockCache HM.empty

addBlock :: RawBlock -> BlockCache -> BlockCache
addBlock rb@(RawBlock (BlockPtr (fromBE64 -> bp)) _) (BlockCache bc) =
  BlockCache (HM.insert bp rb bc)

-- 12 bits
bLOCK_MASK :: Word64
bLOCK_MASK = 0b111111111111

addBlocks :: [RawBlock] -> BlockCache -> BlockCache
addBlocks bc bs = Prelude.foldr addBlock bs bc

lookupClosure :: ClosurePtr -> BlockCache -> Maybe RawBlock
lookupClosure (ClosurePtr (fromBE64 -> cp)) (BlockCache b) =
  HM.lookup (cp .&. complement bLOCK_MASK) b
--  snd <$> listToMaybe (toAscList (I.containing b cp))

bcSize :: BlockCache -> Int
bcSize (BlockCache b) = HM.size b

data BlockCacheRequest a where
  LookupClosure :: ClosurePtr -> BlockCacheRequest RawClosure
  PopulateBlockCache :: BlockCacheRequest Int

deriving instance Show (BlockCacheRequest a)
deriving instance Eq (BlockCacheRequest a)

instance Hashable (BlockCacheRequest a) where
  hashWithSalt s (LookupClosure cpt) = s `hashWithSalt` (1 :: Int) `hashWithSalt` cpt
  hashWithSalt s PopulateBlockCache  = s `hashWithSalt` (2 :: Int)

handleBlockReq :: Handle -> IORef BlockCache -> BlockCacheRequest resp -> IO resp
handleBlockReq h ref (LookupClosure cp) = do
  bc <- readIORef ref
  let mrb = lookupClosure cp bc
  rb <- case mrb of
               Nothing -> do
                 rb <- doRequest h (RequestBlock cp)
                 --print ("NEW_BLOCK", bcSize bc, p)
                 atomicModifyIORef' ref (\bc' -> (addBlock rb bc', ()))
                 return rb
               Just rb -> do
                 return rb
  return (extractFromBlock cp rb)
handleBlockReq h ref PopulateBlockCache = do
  blocks <- doRequest h RequestAllBlocks
  --print ("CACHING", length blocks)
  atomicModifyIORef' ref ((,()) . addBlocks blocks)
  return (length blocks)


