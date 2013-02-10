{-# LANGUAGE TypeFamilies #-}
module Math.KnotTh.Invariants.Skein.Relation
    ( Skein(..)
    , StateModel(..)
    , SkeinRelation(..)
    , SkeinStructure(..)
    , resultOnStructure
    ) where

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
    initialize :: (Ord a, Num a) => [(Skein, a)] -> s a
    asConst    :: (Num a) => s a -> a
    glueHandle :: (SkeinRelation r a) => r -> Int -> s a -> (UArray Int Int, s a)
    connect    :: (SkeinRelation r a) => r -> (Int, s a) -> (Int, s a) -> (UArray Int Int, UArray Int Int, s a)
    assemble   :: (SkeinRelation r a) => r -> Array Int (Int, Int) -> Array Int (Array Int Int) -> Array Int (s a) -> a -> s a


class (Ord a, Num a, Show a, StateModel (SkeinRelationModel r)) => SkeinRelation r a | r -> a where
    type SkeinRelationModel r :: * -> *

    initialLplus       :: r -> [(Skein, a)]

    circleFactor       :: r -> a

    twistPFactor       :: r -> a
    twistNFactor       :: r -> a

    smoothLplusFactor  :: r -> a
    smoothLzeroFactor  :: r -> a
    smoothLinftyFactor :: r -> a

    finalNormalization :: (SkeinStructure k c d) => r -> k ArbitraryCrossing -> a -> a
    finalNormalization _ _ = id


class (Knotted k c d, Eq (d ArbitraryCrossing), Eq (c ArbitraryCrossing)) => SkeinStructure k c d | k -> c, c -> d, d -> k where
    type ResultOnStructure k s a :: *
    endpointPlace      :: d ArbitraryCrossing -> Int
    resultFromStateSum :: (SkeinRelation r a) => r -> k ArbitraryCrossing -> SkeinRelationModel r a -> ResultOnStructure k (SkeinRelationModel r) a


resultOnStructure :: (SkeinStructure k c d, SkeinRelation r a) => r -> k ArbitraryCrossing -> SkeinRelationModel r a -> ResultOnStructure k (SkeinRelationModel r) a
resultOnStructure relation knot =
    resultFromStateSum relation knot . fmap (finalNormalization relation knot)


instance SkeinStructure L.Link L.Crossing L.Dart where
    type ResultOnStructure L.Link s a = a
    endpointPlace = error "endpointPlace: must be no endpoints for link"
    resultFromStateSum _ _ = asConst


instance SkeinStructure T.Tangle T.Crossing T.Dart where
    type ResultOnStructure T.Tangle s a = s a
    endpointPlace = T.legPlace
    resultFromStateSum _ _ = id
