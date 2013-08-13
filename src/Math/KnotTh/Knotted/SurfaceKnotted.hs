{-# LANGUAGE TypeFamilies #-}
module Math.KnotTh.Knotted.SurfaceKnotted
    ( SurfaceKnotted(..)
    , eulerChar
    , allFaces
    ) where

import Math.KnotTh.Knotted.KnottedDefinition.Knotted


class (Knotted knot) => SurfaceKnotted knot where
    data Face knot ct
    numberOfFaces   :: knot ct -> Int
    nthFace         :: knot ct -> Int -> Face knot ct
    faceOwner       :: Face knot ct -> knot ct
    faceIndex       :: Face knot ct -> Int
    faceDegree      :: Face knot ct -> Int


eulerChar :: (SurfaceKnotted k) => k ct -> Int
eulerChar knot = numberOfCrossings knot + numberOfFaces knot - numberOfEdges knot


{-# INLINE allFaces #-}
allFaces :: (SurfaceKnotted k) => k ct -> [Face k ct]
allFaces knot = map (nthFace knot) [1 .. numberOfFaces knot]
