module Math.KnotTh.Link
    ( module X
    , LinkProjection
    , NonAlternatingLink
    , NonAlternatingCrossing
    , NonAlternatingDart
    ) where

import Math.KnotTh.Knotted as X
import Math.KnotTh.Link.Definition.Link as X
import Math.KnotTh.Link.Definition.Misc as X
import Math.KnotTh.Link.Definition.GaussCode as X
import Math.KnotTh.Crossings.Projection as X
import Math.KnotTh.Crossings.Arbitrary as X


type LinkProjection = Link ProjectionCrossing


type NonAlternatingLink = Link ArbitraryCrossing

type NonAlternatingCrossing = Crossing Link ArbitraryCrossing

type NonAlternatingDart = Dart Link ArbitraryCrossing
