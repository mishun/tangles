module Math.KnotTh.Link.NonAlternating
    ( module Math.KnotTh.Crossings.Arbitrary
    , module Math.KnotTh.Link
    , NonAlternatingLink
    , NonAlternatingCrossing
    , NonAlternatingDart
    ) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Link


type NonAlternatingLink = Link ArbitraryCrossing

type NonAlternatingCrossing = Crossing Link ArbitraryCrossing

type NonAlternatingDart = Dart Link ArbitraryCrossing
