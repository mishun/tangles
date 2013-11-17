{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Knotted.SurfaceKnotted
    ( SurfaceKnotted
    , faceIndexRange
    , facesRange
    ) where

import Data.Ix (Ix(..))
import Math.Topology.KnotTh.Knotted.Definition.Knotted


class (Knotted knot, SurfaceDiagram knot) => SurfaceKnotted knot where


{-# INLINE faceIndexRange #-}
faceIndexRange :: (SurfaceKnotted k) => k ct -> (Int, Int)
faceIndexRange knot = (1, numberOfFaces knot)


{-# INLINE facesRange #-}
facesRange :: (SurfaceKnotted k, Ix (Face k ct)) => k ct -> (Face k ct, Face k ct)
facesRange knot
    | f > 0      = (nthFace knot 1, nthFace knot f)
    | otherwise  = error "facesRange: no faces"
    where
        f = numberOfFaces knot
