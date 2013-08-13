{-# LANGUAGE TypeFamilies #-}
module Math.KnotTh.Invariants.Skein.Relation
    ( Skein(..)
    , StateModel(..)
    , SkeinRelation(..)
    , SkeinStructure(..)
    , resultOnStructure
    ) where

import Data.Ix (Ix)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T


--    L+       L-        L0        L∞
--   \ /      \ /       \  /      \ /
--    /        \         ||        =
--   / \      / \       /  \      / \
data Skein = Lplus | Lzero | Linfty


class (Functor s) => StateModel s where
    complexityRank :: s a -> Int
    projection     :: s a -> [(T.NonAlternatingTangle, a)]
    initialize     :: (SkeinRelation r a) => r -> [(Skein, a)] -> s a
    asConst        :: (SkeinRelation r a) => r -> s a -> a
    glueHandle     :: (SkeinRelation r a) => r -> Int -> s a -> (UArray Int Int, s a)
    connect        :: (SkeinRelation r a) => r -> (Int, s a) -> (Int, s a) -> (UArray Int Int, UArray Int Int, s a)
    assemble       :: (SkeinRelation r a) => r -> Array Int (Int, Int) -> Array Int (Array Int Int) -> Array Int (s a) -> a -> s a
    rotate         :: (SkeinRelation r a) => r -> Int -> s a -> s a
    mirror         :: (SkeinRelation r a) => r -> s a -> s a


class (Ord a, Num a, Show a, StateModel (SkeinRelationModel r)) => SkeinRelation r a | r -> a where
    type SkeinRelationModel r :: * -> *

    initialLplus       :: r -> [(Skein, a)]

    circleFactor       :: r -> a

    twistPFactor       :: r -> a
    twistNFactor       :: r -> a

    smoothLplusFactor  :: r -> a
    smoothLzeroFactor  :: r -> a
    smoothLinftyFactor :: r -> a

    finalNormalization :: (SkeinStructure k) => r -> k ArbitraryCrossing -> a -> a
    finalNormalization _ _ = id


class (Knotted k, Ix (Dart k ArbitraryCrossing), Ix (Crossing k ArbitraryCrossing)) => SkeinStructure k where
    type ResultOnStructure k s a :: *
    endpointPlace      :: Dart k ArbitraryCrossing -> Int
    resultFromStateSum :: (SkeinRelation r a) => r -> k ArbitraryCrossing -> SkeinRelationModel r a -> ResultOnStructure k (SkeinRelationModel r) a


resultOnStructure :: (SkeinStructure k, SkeinRelation r a) => r -> k ArbitraryCrossing -> SkeinRelationModel r a -> ResultOnStructure k (SkeinRelationModel r) a
resultOnStructure relation knot =
    resultFromStateSum relation knot . fmap (finalNormalization relation knot)


instance SkeinStructure L.Link where
    type ResultOnStructure L.Link s a = a
    endpointPlace = error "endpointPlace: must be no endpoints for link"
    resultFromStateSum relation _ = asConst relation


instance SkeinStructure T.Tangle where
    type ResultOnStructure T.Tangle s a = s a
    endpointPlace = T.legPlace
    resultFromStateSum _ _ = id
