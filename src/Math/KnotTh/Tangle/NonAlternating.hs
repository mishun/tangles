module Math.KnotTh.Tangle.NonAlternating
    ( module Math.KnotTh.Crossings.Arbitrary
    , module Math.KnotTh.Tangle
    , NonAlternatingTangle
    , NonAlternatingCrossing
    , NonAlternatingDart
    ) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Tangle


type NonAlternatingTangle = Tangle ArbitraryCrossing

type NonAlternatingCrossing = Crossing ArbitraryCrossing

type NonAlternatingDart = Dart ArbitraryCrossing
