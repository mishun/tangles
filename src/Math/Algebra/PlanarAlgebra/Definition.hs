{-# LANGUAGE TypeFamilies #-}
module Math.Algebra.PlanarAlgebra.Definition
    ( PlanarState(..)
    , PlanarDiagram(..)
    , hasVertices
    , numberOfDarts
    , nextDir
    , endVertex
    , endPlace
    , endVertexM
    , endPair
    , endPair'
    , nthIncomingDart
    , incomingDarts
    , allOutcomingDarts
    , PlanarAlgebra(..)
    , firstLeg
    , lastLeg
    , nextLegBy
    , allLegOpposites
    , SurfaceDiagram(..)
    , rightFace
    , rightPlace
    , rightPair
    , nthDartInCWTraverse
    , eulerChar
    ) where

import Data.Ix (Ix(..))
import Data.Array.Unboxed (UArray)
import Control.Arrow (first, (***))
import qualified Math.Algebra.RotationDirection as R


class PlanarState s where
    stateDegree   :: s -> Int
    rotateState   :: Int -> s -> s
    mirrorState   :: s -> s
    loopState     :: Int -> (s, Int) -> (s, UArray Int Int)
    connectStates :: Int -> (s, Int) -> (s, Int) -> (s, UArray Int Int, UArray Int Int) 


-- Laws:
-- nthVertex (vertexOwner d) (vertexIndex d) == d
class PlanarDiagram a where
    numberOfVertices   :: a t -> Int
    numberOfEdges      :: a t -> Int
    nthVertex          :: a t -> Int -> Vertex a t
    nthDart            :: a t -> Int -> Dart a t
    allVertices        :: a t -> [Vertex a t]

    allEdges           :: a t -> [(Dart a t, Dart a t)]
    allEdges = filter (\ (a, b) -> dartIndex a < dartIndex b) . map (\ d -> (d, opposite d)) . allHalfEdges

    allHalfEdges       :: a t -> [Dart a t]
    allHalfEdges = concatMap (\ (a, b) -> [a, b]) . allEdges

    data Vertex a t
    vertexDegree       :: Vertex a t -> Int
    vertexOwner        :: Vertex a t -> a t
    vertexIndex        :: Vertex a t -> Int
    nthOutcomingDart   :: Vertex a t -> Int -> Dart a t
    outcomingDarts     :: Vertex a t -> [Dart a t]

    data Dart a t
    dartOwner          :: Dart a t -> a t
    dartIndex          :: Dart a t -> Int
    opposite           :: Dart a t -> Dart a t

    isDart             :: Dart a t -> Bool
    isDart _ = True

    beginVertex        :: Dart a t -> Vertex a t
    beginPlace         :: Dart a t -> Int
    beginPair          :: Dart a t -> (Vertex a t, Int)
    beginPair'         :: Dart a t -> (Int, Int)
    beginVertexM       :: Dart a t -> Maybe (Vertex a t)

    beginVertex = fst . beginPair
    beginPlace  = snd . beginPair
    beginPair d = (beginVertex d, beginPlace d)
    beginPair' = first vertexIndex . beginPair
    beginVertexM d | isDart d   = Just $! beginVertex d
                   | otherwise  = Nothing

    nextCCW, nextCW    :: Dart a t -> Dart a t
    nextBy             :: Int -> Dart a t -> Dart a t

    nextCCW = nextBy 1
    nextCW = nextBy (-1)
    nextBy n d = let (v, p) = beginPair d
                 in nthOutcomingDart v $ (p + n) `mod` vertexDegree v

    vertexIndicesRange :: a t -> (Int, Int)
    verticesRange      :: (Ix (Vertex a t)) => a t -> (Vertex a t, Vertex a t)

    verticesRange a | numberOfVertices a > 0  = (nthVertex a *** nthVertex a) $ vertexIndicesRange a
                    | otherwise               = error "verticesRange: no vertices"

    dartIndicesRange   :: a t -> (Int, Int)
    dartsRange         :: (Ix (Dart a t)) => a t -> (Dart a t, Dart a t)

    dartsRange a | numberOfDarts a > 0  = (nthDart a *** nthDart a) $ dartIndicesRange a
                 | otherwise            = error "dartsRange: no darts"


{-# INLINE hasVertices #-}
hasVertices :: (PlanarDiagram a) => a t -> Bool
hasVertices = (> 0) . numberOfVertices


{-# INLINE numberOfDarts #-}
numberOfDarts :: (PlanarDiagram a) => a t -> Int
numberOfDarts = (* 2) . numberOfEdges


{-# INLINE nextDir #-}
nextDir :: (PlanarDiagram a) => R.RotationDirection -> Dart a t -> Dart a t
nextDir dir | R.isClockwise dir  = nextCW
            | otherwise          = nextCCW


{-# INLINE endVertex #-}
endVertex :: (PlanarDiagram a) => Dart a t -> Vertex a t
endVertex = beginVertex . opposite


{-# INLINE endPlace #-}
endPlace :: (PlanarDiagram a) => Dart a t -> Int
endPlace = beginPlace . opposite


{-# INLINE endVertexM #-}
endVertexM :: (PlanarDiagram a) => Dart a t -> Maybe (Vertex a t)
endVertexM = beginVertexM . opposite


{-# INLINE endPair #-}
endPair :: (PlanarDiagram a) => Dart a t -> (Vertex a t, Int)
endPair = beginPair . opposite


{-# INLINE endPair' #-}
endPair' :: (PlanarDiagram a) => Dart a t -> (Int, Int)
endPair' = beginPair' . opposite


{-# INLINE nthIncomingDart #-}
nthIncomingDart :: (PlanarDiagram a) => Vertex a t -> Int -> Dart a t
nthIncomingDart v i = opposite $ nthOutcomingDart v i


{-# INLINE incomingDarts #-}
incomingDarts :: (PlanarDiagram a) => Vertex a t -> [Dart a t]
incomingDarts = map opposite . outcomingDarts


{-# INLINE allOutcomingDarts #-}
allOutcomingDarts :: (PlanarDiagram a) => a t -> [Dart a t]
allOutcomingDarts = concatMap outcomingDarts . allVertices


class (PlanarDiagram a) => PlanarAlgebra a where
    numberOfLegs :: a t -> Int
    nthLeg       :: a t -> Int -> Dart a t
    allLegs      :: a t -> [Dart a t]
    legPlace     :: Dart a t -> Int
    isLeg        :: Dart a t -> Bool


{-# INLINE firstLeg #-}
firstLeg :: (PlanarAlgebra a) => a t -> Dart a t
firstLeg t = nthLeg t 0


{-# INLINE lastLeg #-}
lastLeg :: (PlanarAlgebra a) => a t -> Dart a t
lastLeg t = nthLeg t (-1)


{-# INLINE nextLegBy #-}
nextLegBy :: (PlanarAlgebra a) => Int -> Dart a t -> Dart a t
nextLegBy n d = nthLeg (dartOwner d) (legPlace d + n)


{-# INLINE allLegOpposites #-}
allLegOpposites :: (PlanarAlgebra a) => a t -> [Dart a t]
allLegOpposites = map opposite . allLegs


class (PlanarDiagram a) => SurfaceDiagram a where
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
    facesRange           :: (Ix (Face a t)) => a t -> (Face a t, Face a t)

    facesRange a | numberOfFaces a > 0  = (nthFace a *** nthFace a) $ faceIndicesRange a
                 | otherwise            = error "facesRange: no faces"


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


eulerChar :: (SurfaceDiagram a) => a t -> Int
eulerChar a = numberOfVertices a + numberOfFaces a - numberOfEdges a
