{-# LANGUAGE DerivingVia #-}
module GHC.Debug.Profile.Types where

import           GHC.Debug.Types
import Data.Monoid
import Data.Semigroup

newtype Count = Count {getCount :: Int }
                deriving (Semigroup, Monoid, Num) via Sum Int
                deriving (Show, Ord, Eq)

newtype Sample = Sample { getSamples :: [ClosurePtr] } deriving (Show, Eq)

instance Monoid Sample where
  mempty = Sample []

instance Semigroup Sample where
  (Sample a) <> (Sample b) =
    case (take 5 (a ++ b)) of
      xs@(_:_:_:_:_:_) -> Sample xs
      xs -> Sample xs



data CensusStats = CS { cscount :: !Count
                        , cssize :: !Size
                        , csmax :: !(Max Size)
                        , sample :: !Sample } deriving (Show, Eq)

mkCS :: ClosurePtr -> Size -> CensusStats
mkCS cp i = CS (Count 1) i (Max i) (Sample [cp])

instance Monoid CensusStats where
  mempty = CS mempty mempty (Max (Size 0)) mempty

instance Semigroup CensusStats where
  (CS a b c d) <> (CS a1 b1 c1 d1) = CS (a <> a1) (b <> b1) (c <> c1) (d <> d1)
