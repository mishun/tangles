{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Algebra.SurfaceDiagram
    ( SurfaceDiagram(..)
    , rightFace
    , rightPlace
    , rightPair
    , nthDartInCWTraverse
    , eulerCharOf
    , genusOf
    ) where

import Control.Arrow ((***), first)
import qualified Data.Ix as Ix
import Math.Topology.KnotTh.Algebra.PlanarAlgebra


class (VertexDiagram a) => SurfaceDiagram a where
    numberOfFaces        :: a t -> Int
    nthFace              :: a t -> Int -> Face a t
    allFaces             :: a t -> [Face a t]

    data Face a t
    faceDegree           :: Face a t -> Int
    faceOwner            :: Face a t -> a t
    faceIndex            :: Face a t -> Int

    leftFace             :: Dart a t -> Face a t
    leftPlace            :: Dart a t -> Int
    leftPair             :: Dart a t -> (Face a t, Int)
    leftPair'            :: Dart a t -> (Int, Int)

    leftFace   = fst . leftPair
    leftPlace  = snd . leftPair
    leftPair d = (leftFace d, leftPlace d)
    leftPair' = first faceIndex . leftPair

    nthDartInCCWTraverse :: Face a t -> Int -> Dart a t
    faceTraverseCCW      :: Face a t -> [Dart a t]

    faceTraverseCCW f = map (nthDartInCCWTraverse f) [0 .. faceDegree f - 1]

    faceIndicesRange     :: a t -> (Int, Int)
    facesRange           :: a t -> (Face a t, Face a t)
    facesRange a | numberOfFaces a > 0  = (nthFace a *** nthFace a) $ faceIndicesRange a
                 | otherwise            = error "facesRange: no faces"

instance (SurfaceDiagram a) => Eq (Face a t) where
    (==) a b = faceIndex a == faceIndex b

instance (SurfaceDiagram a) => Ord (Face a t) where
    compare a b = faceIndex a `compare` faceIndex b

instance (SurfaceDiagram a) => Ix.Ix (Face a t) where
    range     (a, b)   = map (nthFace (faceOwner b)) [faceIndex a .. faceIndex b]
    index     (a, b) c = Ix.index (faceIndex a, faceIndex b) (faceIndex c)
    inRange   (a, b) c = (faceIndex c >= faceIndex a) && (faceIndex c <= faceIndex b)
    rangeSize (a, b)   = max 0 (faceIndex b - faceIndex a + 1)


{-# INLINE rightFace #-}
rightFace :: (SurfaceDiagram a) => Dart a t -> Face a t
rightFace = leftFace . opposite


{-# INLINE rightPlace #-}
rightPlace :: (SurfaceDiagram a) => Dart a t -> Int
rightPlace = leftPlace . opposite


{-# INLINE rightPair #-}
rightPair :: (SurfaceDiagram a) => Dart a t -> (Face a t, Int)
rightPair = leftPair . opposite


nthDartInCWTraverse :: (SurfaceDiagram a) => Face a t -> Int -> Dart a t
nthDartInCWTraverse f p = opposite $ nthDartInCCWTraverse f p


eulerCharOf :: (SurfaceDiagram a) => a t -> Int
eulerCharOf a = numberOfVertices a + numberOfFaces a - numberOfEdges a


genusOf :: (SurfaceDiagram a) => a t -> Int
genusOf a = (2 - eulerCharOf a) `div` 2
