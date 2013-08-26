{-# LANGUAGE TypeFamilies #-}
module Math.KnotTh.Knotted.SurfaceKnotted
    ( SurfaceKnotted(..)
    , eulerChar
    , left
    , right
    , allFaces
    ) where

import Math.KnotTh.Knotted.KnottedDefinition.Knotted


class (Knotted knot) => SurfaceKnotted knot where
    data Face knot ct

    numberOfFaces         :: knot ct -> Int
    nthFace               :: knot ct -> Int -> Face knot ct
    faceOwner             :: Face knot ct -> knot ct
    faceIndex, faceDegree :: Face knot ct -> Int

    nthCCWBorderDart, nthCWBorderDart :: Face knot ct -> Int -> Dart knot ct
    faceToTheLeft, faceToTheRight     :: Dart knot ct -> Face knot ct
    placeToTheLeft, placeToTheRight   :: Dart knot ct -> Int

    nthCWBorderDart f p = opposite $ nthCCWBorderDart f p
    faceToTheRight = faceToTheLeft . opposite
    placeToTheRight = placeToTheLeft . opposite


eulerChar :: (SurfaceKnotted k) => k ct -> Int
eulerChar knot = numberOfCrossings knot + numberOfFaces knot - numberOfEdges knot


left, right :: (SurfaceKnotted k) => Dart k ct -> (Face k ct, Int)
left d = (faceToTheLeft d, placeToTheLeft d)
right d = (faceToTheRight d, placeToTheRight d)


{-# INLINE allFaces #-}
allFaces :: (SurfaceKnotted k) => k ct -> [Face k ct]
allFaces knot = map (nthFace knot) [1 .. numberOfFaces knot]
