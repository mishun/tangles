{-# LANGUAGE TypeFamilies #-}
module Math.KnotTh.Invariants.Skein.Relation
    ( SkeinRelation(..)
    , SkeinStructure(..)
    ) where

import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.StateSum
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T


--    L+       L-        L0        L∞
--   \ /      \ /       \  /      \ /
--    /        \         ||        =
--   / \      / \       /  \      / \
class (Ord a, Num a, Show a) => SkeinRelation r a | r -> a where
    initialLplus       :: r -> InitialSum a

    circleFactor       :: r -> a

    twistPFactor       :: r -> a
    twistNFactor       :: r -> a

    smoothLplusFactor  :: r -> a
    smoothLzeroFactor  :: r -> a
    smoothLinftyFactor :: r -> a

    finalNormalization :: (SkeinStructure k c d) => r -> k ArbitraryCrossing -> a -> a
    finalNormalization _ _ = id


class (Knotted k c d, Eq (d ArbitraryCrossing), Eq (c ArbitraryCrossing)) => SkeinStructure k c d | k -> c, c -> d, d -> k where
    type SkeinResult k a :: *
    endpointPlace      :: d ArbitraryCrossing -> Int
    resultFromStateSum :: (SkeinRelation r a) => r -> k ArbitraryCrossing -> StateSum a -> SkeinResult k a


instance SkeinStructure L.Link L.Crossing L.Dart where
    type SkeinResult L.Link a = a
    endpointPlace = error "endpointPlace: must be no endpoints for link"
    resultFromStateSum _ _ = takeAsConst


instance SkeinStructure T.Tangle T.Crossing T.Dart where
    type SkeinResult T.Tangle a = StateSum a
    endpointPlace = T.legPlace
    resultFromStateSum _ _ = id
