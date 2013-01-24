{-# LANGUAGE UndecidableInstances #-}
module Math.KnotTh.Invariants.Skein.Knotted
    ( SkeinKnotted(..)
    , SkeinResult(..)
    ) where

import Data.List (sort)
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T
import Math.KnotTh.Invariants.Skein.StateSum
import Math.KnotTh.Invariants.Skein.StateSum.Operations


class (Knotted k c d, Eq (d ArbitraryCrossing)) => SkeinKnotted k c d | k -> c, c -> d, d -> k where
    endpointPlace :: d ArbitraryCrossing -> Int


class (SkeinKnotted k c d) => SkeinResult a r k c d
        | k a -> r
        , k r -> a
        , k -> c
        , c -> d
        , d -> k
    where
        resultFromStateSum :: k ArbitraryCrossing -> StateSum a -> r


instance SkeinKnotted L.Link L.Crossing L.Dart where
    endpointPlace = error "endpointPlace: must be no endpoints for link"


instance (Ord a, Num a) => SkeinResult a a L.Link L.Crossing L.Dart where
    resultFromStateSum _ s =
        case takeAsConst s of
            Just x -> x
            _      -> error "incorrect end state sum for link"


instance SkeinKnotted T.Tangle T.Crossing T.Dart where
    endpointPlace = T.legPlace


instance (Ord a, Num a) => SkeinResult a (StateSum a) T.Tangle T.Crossing T.Dart where
    resultFromStateSum _ s = sort s
