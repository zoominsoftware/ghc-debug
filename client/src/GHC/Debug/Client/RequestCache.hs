{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module GHC.Debug.Client.RequestCache(RequestCache
                                    , cacheReq
                                    , lookupReq
                                    , lookupBlocks
                                    , emptyRequestCache
                                    , clearMovableRequests
                                    , putCache
                                    , getCache ) where

import qualified Data.HashMap.Strict as HM
import GHC.Debug.Types
import Unsafe.Coerce
import Data.Binary
import Control.Monad

newtype RequestCache = RequestCache (HM.HashMap AnyReq AnyResp)

instance Binary RequestCache where
  get = getCache
  put = putCache

cacheReq :: Request resp -> resp -> RequestCache -> RequestCache
cacheReq req resp (RequestCache rc)
  -- Don't cache the results of writes, such as pause/unpause
  | isWriteRequest req = RequestCache rc
  | otherwise = RequestCache (HM.insert (AnyReq req) (AnyResp resp (putResponseBinary req)) rc)

lookupReq :: forall resp . Request resp -> RequestCache -> Maybe resp
lookupReq req (RequestCache rc) = coerceResult <$> HM.lookup (AnyReq req) rc
  where
    coerceResult :: AnyResp -> resp
    coerceResult (AnyResp a _) = unsafeCoerce a

lookupBlocks :: RequestCache -> [RawBlock]
lookupBlocks c@(RequestCache rc) =
  let all_blocks = case lookupReq RequestAllBlocks c of
                        Just bs -> bs
                        Nothing -> []

      get_block :: AnyReq -> AnyResp -> [RawBlock] -> [RawBlock]
      get_block (AnyReq (RequestBlock {})) (AnyResp resp _) bs = unsafeCoerce resp : bs
      get_block _ _ bs = bs

      individual_blocks = HM.foldrWithKey get_block [] rc

  in (all_blocks ++ individual_blocks)


emptyRequestCache :: RequestCache
emptyRequestCache = RequestCache HM.empty

-- These get/put functions are a lot like the ones for serialising info
-- to/from the debuggee but we are careful that each one reads a bounded
-- amount of input.

getResponseBinary :: Request a -> Get a
getResponseBinary RequestVersion       = Version <$> get <*> get <*> getProfilingMode <*> get
getResponseBinary (RequestPause {})    = get
getResponseBinary RequestResume        = get
getResponseBinary RequestRoots         = get
getResponseBinary (RequestClosure {}) = get
getResponseBinary (RequestInfoTable{}) = getInfoTable
getResponseBinary (RequestSRT {}) = get
getResponseBinary (RequestStackBitmap {}) = get
getResponseBinary (RequestFunBitmap {}) = get
getResponseBinary (RequestConstrDesc _)  = getConstrDescCache
getResponseBinary RequestPoll          = get
getResponseBinary RequestSavedObjects  = get
getResponseBinary (RequestSourceInfo _c) = getIPE
getResponseBinary RequestAllBlocks = get
getResponseBinary RequestBlock {}  = get
getResponseBinary RequestCCS {}  = getCCS
getResponseBinary RequestCC {}  = getCC
getResponseBinary RequestIndexTable {}  = getIndexTable
getResponseBinary RequestCCSMainPtr {}  = getCCSMainPtr

putResponseBinary :: Request a -> a -> Put
putResponseBinary RequestVersion (Version w1 w2 vprof tntc) = put w1 >> put w2 >> putProfilingMode vprof >> put tntc
putResponseBinary (RequestPause {}) w  = put w
putResponseBinary RequestResume w      = put w
putResponseBinary RequestRoots  rs     = put rs
putResponseBinary (RequestClosure {}) rcs = put rcs
putResponseBinary (RequestInfoTable {}) r = putInfoTable r
putResponseBinary (RequestSRT {}) rcs = put rcs
putResponseBinary (RequestStackBitmap {}) pbm = put pbm
putResponseBinary (RequestFunBitmap {}) pbm = put pbm
putResponseBinary (RequestConstrDesc _) cd  = putConstrDescCache cd
putResponseBinary RequestPoll         r = put r
putResponseBinary RequestSavedObjects os = putList os
putResponseBinary (RequestSourceInfo _c) ipe = putIPE ipe
putResponseBinary RequestAllBlocks rs = put rs
putResponseBinary RequestBlock {} r = put r
putResponseBinary RequestCCS{} r = putCCS r
putResponseBinary RequestCC{} r = putCC r
putResponseBinary RequestIndexTable{} r = putIndexTable r
putResponseBinary RequestCCSMainPtr{} r = putCCSMainPtr r

putConstrDescCache :: ConstrDesc -> Put
putConstrDescCache (ConstrDesc a b c) = do
  put a
  put b
  put c

getConstrDescCache :: Get ConstrDesc
getConstrDescCache =
  ConstrDesc <$> get <*> get <*> get

putLine :: AnyReq -> AnyResp -> Put -> Put
putLine (AnyReq req) (AnyResp resp p) k = putRequest req >> p resp >> k

getCacheLine :: Get (AnyReq, AnyResp)
getCacheLine = do
  AnyReq req <- getRequest
  resp <- getResponseBinary req
  return (AnyReq req, AnyResp resp (putResponseBinary req))

putCache :: RequestCache -> Put
putCache (RequestCache rc) = do
  put (HM.size rc)
  HM.foldrWithKey putLine (return ()) rc

getCache :: Get RequestCache
getCache = do
  n <- get
  RequestCache . HM.fromList <$> replicateM n getCacheLine

-- | Clear the part of the cache which will become invalid after pausing
-- For example, we need to clear blocks, but can keep the info table
-- caches.
clearMovableRequests :: RequestCache -> RequestCache
clearMovableRequests (RequestCache rc) = RequestCache (HM.filterWithKey (\(AnyReq r) _ -> isImmutableRequest r) rc)
