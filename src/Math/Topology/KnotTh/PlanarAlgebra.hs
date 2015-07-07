{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.PlanarAlgebra
    ( PlanarAlgebra(..)
    , DartDiagram(..)
    , VertexDiagram(..)
    , hasVertices
    , allOutcomingDarts
    , LeggedDiagram(..)
    , hasLegs
    , firstLeg
    , lastLeg
    , nextLegBy
    , allLegOpposites
    , SurfaceDiagram(..)
    , rightFace
    , rightPlace
    , rightPair
    , nthDartInCWTraverse
    , eulerCharOf
    , genusOf
    ) where

import Control.Arrow ((***), first)
import Data.Bits (shiftL, shiftR)
import qualified Data.Ix as Ix
import Text.Printf
import Math.Topology.KnotTh.Dihedral


class (RotationAction a) => PlanarAlgebra a where
    planarDegree                   :: a -> Int
    planarEmpty                    :: a
    planarLoop                     :: a
    planarPropagator               :: Int -> a
    horizontalCompositionUnchecked :: Int -> (a, Int) -> (a, Int) -> a
    horizontalLooping              :: Int -> (a, Int) -> a

    planarLoop = horizontalComposition 2 (planarPropagator 1, 0) (planarPropagator 1, 0)

    horizontalLooping gl (x, pos) =
        horizontalComposition (2 * gl) (x, pos) (planarPropagator gl, 0)

    {-# INLINE horizontalComposition #-}
    horizontalComposition :: (PlanarAlgebra a) => Int -> (a, Int) -> (a, Int) -> a
    horizontalComposition !gl (!a, !posA) (!b, !posB)
        | gl < 0      = error $ printf "horizontalComposition: gl (%i) must be non-negative" gl
        | gl > legsA  = error $ printf "horizontalComposition: gl (%i) exceeds number of legs of the first argument (%i)" gl legsA
        | gl > legsB  = error $ printf "horizontalComposition: gl (%i) exceeds number of legs of the second argument (%i)" gl legsB
        | otherwise   = horizontalCompositionUnchecked gl (a, posA) (b, posB)
        where legsA = planarDegree a
              legsB = planarDegree b

    horizontalCompositionUnchecked = horizontalComposition


class DartDiagram d where
    data Dart d   :: * -> *
    dartOwner     :: Dart d a -> d a
    dartIndex     :: Dart d a -> Int
    opposite      :: Dart d a -> Dart d a

    nextCCW       :: Dart d a -> Dart d a
    nextCW        :: Dart d a -> Dart d a
    nextDir       :: RotationDirection -> Dart d a -> Dart d a
    nextBy        :: Int -> Dart d a -> Dart d a

    numberOfDarts :: d a -> Int
    numberOfEdges :: d a -> Int
    nthDart       :: d a -> Int -> Dart d a
    allDarts      :: d a -> [Dart d a]
    allEdges      :: d a -> [(Dart d a, Dart d a)]

    nextCCW = nextBy 1
    nextCW  = nextBy (-1)
    nextDir dir = nextBy (directionSign dir)

    numberOfEdges d = numberOfDarts d `shiftR` 1
    numberOfDarts d = numberOfEdges d `shiftL` 1

    allEdges = filter (\ (a, b) -> dartIndex a < dartIndex b) . map (\ d -> (d, opposite d)) . allDarts
    allDarts d = map (nthDart d) [0 .. numberOfDarts d - 1]

    -- TODO: remove it?
    dartIndicesRange :: d a -> (Int, Int)
    dartsRange       :: d a -> (Dart d a, Dart d a)
    dartsRange a | numberOfDarts a > 0  = (nthDart a *** nthDart a) $ dartIndicesRange a
                 | otherwise            = error "dartsRange: no darts"

instance (DartDiagram d) => Eq (Dart d a) where
    (==) a b = dartIndex a == dartIndex b

instance (DartDiagram d) => Ord (Dart d a) where
    compare a b = dartIndex a `compare` dartIndex b

instance (DartDiagram d) => Ix.Ix (Dart d a) where
    range     (a, b)   = map (nthDart (dartOwner b)) [dartIndex a .. dartIndex b]
    index     (a, b) c = Ix.index (dartIndex a, dartIndex b) (dartIndex c)
    inRange   (a, b) c = (dartIndex c >= dartIndex a) && (dartIndex c <= dartIndex b)
    rangeSize (a, b)   = max 0 (dartIndex b - dartIndex a + 1)


class (DartDiagram d) => VertexDiagram d where
    data Vertex d    :: * -> *
    vertexOwner      :: Vertex d a -> d a
    vertexIndex      :: Vertex d a -> Int
    vertexDegree     :: Vertex d a -> Int
    nthOutcomingDart :: Vertex d a -> Int -> Dart d a
    nthIncomingDart  :: Vertex d a -> Int -> Dart d a

    numberOfVertices :: d a -> Int
    nthVertex        :: d a -> Int -> Vertex d a
    allVertices      :: d a -> [Vertex d a]

    maybeBeginVertex :: Dart d a -> Maybe (Vertex d a)
    maybeEndVertex   :: Dart d a -> Maybe (Vertex d a)
    beginVertex      :: Dart d a -> Vertex d a
    endVertex        :: Dart d a -> Vertex d a
    beginVertexIndex :: Dart d a -> Int
    endVertexIndex   :: Dart d a -> Int
    beginPlace       :: Dart d a -> Int
    endPlace         :: Dart d a -> Int
    beginPair        :: Dart d a -> (Vertex d a, Int)
    endPair          :: Dart d a -> (Vertex d a, Int)
    beginPair'       :: Dart d a -> (Int, Int)
    endPair'         :: Dart d a -> (Int, Int)

    outcomingDarts          :: Vertex d a -> [Dart d a]
    incomingDarts           :: Vertex d a -> [Dart d a]
    --forMOutcomingDarts      :: (Monad m) => Vertex d a -> (Dart d a -> m ()) -> m ()
    --foldMOutcomingDarts     :: (Monad m) => Vertex d a -> (s -> Dart d a -> m s) -> s -> m s
    --foldMOutcomingDartsFrom :: (Monad m) => Dart d a -> RotationDirection -> (s -> Dart d a -> m s) -> s -> m s

    nthIncomingDart v i = opposite $ nthOutcomingDart v i

    maybeEndVertex = maybeBeginVertex . opposite
    beginVertex = fst . beginPair
    endVertex   = beginVertex . opposite
    beginVertexIndex = vertexIndex . beginVertex
    endVertexIndex   = beginVertexIndex . opposite
    beginPlace  = snd . beginPair
    endPlace    = beginPlace . opposite
    beginPair d = (beginVertex d, beginPlace d)
    endPair     = beginPair . opposite
    beginPair' = first vertexIndex . beginPair
    endPair' = beginPair' . opposite

    outcomingDarts v = map (nthOutcomingDart v) [0 .. vertexDegree v - 1]
    incomingDarts = map opposite . outcomingDarts

    --forMOutcomingDarts v = forM_ (outcomingDarts v)
    --foldMOutcomingDarts v f x0 = foldM f x0 (outcomingDarts v)
    --foldMOutcomingDartsFrom

    -- TODO: remove it?
    isDart             :: Dart d a -> Bool
    vertexIndicesRange :: d a -> (Int, Int)
    verticesRange      :: d a -> (Vertex d a, Vertex d a)
    isDart _ = True
    verticesRange a | numberOfVertices a > 0  = (nthVertex a *** nthVertex a) $ vertexIndicesRange a
                    | otherwise               = error "verticesRange: no vertices"

instance (VertexDiagram d) => Eq (Vertex d a) where
    (==) a b = vertexIndex a == vertexIndex b

instance (VertexDiagram d) => Ord (Vertex d a) where
    compare a b = vertexIndex a `compare` vertexIndex b

instance (VertexDiagram d) => Ix.Ix (Vertex d a) where
    range     (a, b)   = map (nthVertex (vertexOwner b)) [vertexIndex a .. vertexIndex b]
    index     (a, b) c = Ix.index (vertexIndex a, vertexIndex b) (vertexIndex c)
    inRange   (a, b) c = (vertexIndex c >= vertexIndex a) && (vertexIndex c <= vertexIndex b)
    rangeSize (a, b)   = max 0 (vertexIndex b - vertexIndex a + 1)


{-# INLINE hasVertices #-}
hasVertices :: (VertexDiagram d) => d a -> Bool
hasVertices = (> 0) . numberOfVertices


{-# INLINE allOutcomingDarts #-}
allOutcomingDarts :: (VertexDiagram d) => d a -> [Dart d a]
allOutcomingDarts = concatMap outcomingDarts . allVertices

{-
{-# INLINE forMIncomingDarts #-}
forMIncomingDarts :: (Monad m, VertexDiagram d) => Vertex d a -> (Dart d a -> m ()) -> m ()
forMIncomingDarts c f = forMOutcomingDarts c (f . opposite)


{-# INLINE foldMIncomingDarts #-}
foldMIncomingDarts :: (Monad m, VertexDiagram d) => Vertex d a -> (s -> Dart d a -> m s) -> s -> m s
foldMIncomingDarts c f = foldMOutcomingDarts c (\ s d -> f s $! opposite d)


{-# INLINE foldMIncomingDartsFrom #-}
foldMIncomingDartsFrom :: (Monad m, VertexDiagram d) => Dart d a -> RotationDirection -> (s -> Dart d a -> m s) -> s -> m s
foldMIncomingDartsFrom dart direction f = foldMOutcomingDartsFrom dart direction (\ s d -> f s $! opposite d)
-}

class (DartDiagram d) => LeggedDiagram d where
    numberOfLegs  :: d a -> Int
    isLeg         :: Dart d a -> Bool
    legPlace      :: Dart d a -> Int
    nthLeg        :: d a -> Int -> Dart d a
    allLegs       :: d a -> [Dart d a]

    allLegs d = map (nthLeg d) [0 .. numberOfLegs d - 1]


{-# INLINE hasLegs #-}
hasLegs :: (LeggedDiagram d) => d a -> Bool
hasLegs = (> 0) . numberOfLegs


{-# INLINE firstLeg #-}
{-# INLINE lastLeg #-}
firstLeg, lastLeg :: (LeggedDiagram d) => d a -> Dart d a
firstLeg t = nthLeg t 0
lastLeg t = nthLeg t (-1)


{-# INLINE nextLegBy #-}
nextLegBy :: (LeggedDiagram d) => Int -> Dart d a -> Dart d a
nextLegBy n d = nthLeg (dartOwner d) (legPlace d + n)


{-# INLINE allLegOpposites #-}
allLegOpposites :: (LeggedDiagram d) => d a -> [Dart d a]
allLegOpposites = map opposite . allLegs


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
