{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Link.Definition.Link
    ( Link
    , emptyLink
    , linkToTangle
    , tangleToLink
    , LinkProjection
    , LinkProjectionVertex
    , LinkProjectionDart
    , LinkDiagram
    , LinkDiagramVertex
    , LinkDiagramDart
    ) where

import Control.Arrow ((***))
import Text.Printf
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Tangle


newtype Link a = L (Tangle a)


instance PlanarDiagram Link where
    numberOfVertices (L t) = numberOfVertices t
    numberOfEdges (L t) = numberOfEdges t
    numberOfDarts (L t) = numberOfDarts t
    nthVertex (L t) n = V (nthVertex t n)
    nthDart (L t) n = D (nthDart t n)
    allVertices (L t) = map V (allVertices t)
    allEdges (L t) = map (D *** D) (allEdges t)
    allHalfEdges (L t) = map D (allHalfEdges t)

    newtype Vertex Link a = V (Vertex Tangle a)
    vertexDegree (V v) = vertexDegree v
    vertexOwner (V v) = L (vertexOwner v)
    vertexIndex (V v) = vertexIndex v
    nthOutcomingDart (V v) n = D (nthOutcomingDart v n)
    outcomingDarts (V v) = map D (outcomingDarts v)

    newtype Dart Link a = D (Dart Tangle a)
    dartOwner (D d) = L (dartOwner d)
    dartIndex (D d) = dartIndex d
    opposite (D d) = D (opposite d)
    beginVertex (D d) = V (beginVertex d)
    beginPlace (D d) = beginPlace d
    nextCCW (D d) = D (nextCCW d)
    nextCW (D d) = D (nextCW d)
    nextBy n (D d) = D (nextBy n d)

    vertexIndicesRange (L t) = vertexIndicesRange t
    dartIndicesRange (L t) = dartIndicesRange t


instance Functor Link where
    fmap f (L t) = L (fmap f t)


instance Knotted Link where
    vertexCrossing (V v) = vertexCrossing v

    mapCrossings f (L t) = L $ mapCrossings (\ v -> f (V v)) t

    type ExplodeType Link a = (Int, [([(Int, Int)], a)])
    explode (L t) = let (f, [], l) = explode t in (f, l)
    implode (f, l) = L (implode (f, [], l))

    unrootedHomeomorphismInvariant (L t) = unrootedHomeomorphismInvariant t

    isConnected (L t) = isConnected t


instance KnottedPlanar Link where
    numberOfFreeLoops (L t) = numberOfFreeLoops t
    changeNumberOfFreeLoops n (L t) = L (changeNumberOfFreeLoops n t)
    emptyKnotted = L emptyKnotted


instance KnottedDiagram Link where
    isReidemeisterReducible (L t) = isReidemeisterReducible t
    tryReduceReidemeisterI (L t) = tangleToLink `fmap` tryReduceReidemeisterI t
    tryReduceReidemeisterII (L t) = tangleToLink `fmap` tryReduceReidemeisterII t
    goReidemeisterIII (L t) = tangleToLink `fmap` goReidemeisterIII t


instance KnottedWithPrimeTest Link where
    isPrime (L t) = isPrime t


instance (Show a) => Show (Link a) where
    show = printf "implode %s" . show . explode


instance (Show a) => Show (Vertex Link a) where
    show v =
        printf "(Crossing %i %s [ %s ])"
            (vertexIndex v)
            (show $ vertexCrossing v)
            (unwords $ map (show . opposite) $ outcomingDarts v)


instance Show (Dart Link a) where
    show d = let (c, p) = beginPair' d
             in printf "(Dart %i %i)" c p


emptyLink :: Link a
emptyLink = emptyKnotted


linkToTangle :: Link a -> Tangle a
linkToTangle (L t) = t


tangleToLink :: Tangle a -> Link a
tangleToLink t | l == 0     = L t
               | otherwise  = error $ printf "tangleToLink: expected 0 legs, found %i" l
    where
        l = numberOfLegs t


type LinkProjection = Link ProjectionCrossing
type LinkProjectionVertex = Vertex Link ProjectionCrossing
type LinkProjectionDart = Dart Link ProjectionCrossing


type LinkDiagram = Link DiagramCrossing
type LinkDiagramVertex = Vertex Link DiagramCrossing
type LinkDiagramDart = Dart Link DiagramCrossing
