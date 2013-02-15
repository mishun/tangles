{-# LANGUAGE KindSignatures #-}
module Math.KnotTh.Knotted.SurfaceKnotted
    ( SurfaceKnotted(..)
    , eulerChar
    , allFaces
    ) where

import Math.KnotTh.Knotted.KnottedDefinition.Knotted


class (Knotted knot cross dart) => SurfaceKnotted knot cross (face :: * -> *) dart | knot -> cross, cross -> face, face -> dart, dart -> knot where
    numberOfFaces   :: knot ct -> Int
    nthFace         :: knot ct -> Int -> face ct
    faceOwner       :: face ct -> knot ct
    faceIndex       :: face ct -> Int
    faceDegree      :: face ct -> Int


eulerChar :: (SurfaceKnotted k c f d) => k ct -> Int
eulerChar knot = numberOfCrossings knot + numberOfFaces knot - numberOfEdges knot


{-# INLINE allFaces #-}
allFaces :: (SurfaceKnotted k c f d) => k ct -> [f ct]
allFaces knot = map (nthFace knot) [1 .. numberOfFaces knot]
