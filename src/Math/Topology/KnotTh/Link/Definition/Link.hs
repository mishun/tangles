{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Math.Topology.KnotTh.Link.Definition.Link
    ( Link
    , emptyLink
    , linkToTangle
    , tangleToLink
    ) where

import Data.Ix (Ix)
import Control.Arrow ((***))
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.TH.Show
import Math.Topology.KnotTh.Tangle


newtype Link ct = L (Tangle ct)


instance PlanarDiagram Link where
    numberOfVertices (L t) = numberOfVertices t
    numberOfEdges (L t) = numberOfEdges t
    nthVertex (L t) n = V (nthVertex t n)
    nthDart (L t) n = D (nthDart t n)
    allVertices (L t) = map V (allVertices t)
    allEdges (L t) = map (D *** D) (allEdges t)
    allHalfEdges (L t) = map D (allHalfEdges t)

    newtype Vertex Link ct = V (Vertex Tangle ct) deriving (Eq, Ord, Ix)
    vertexDegree (V v) = vertexDegree v
    vertexOwner (V v) = L (vertexOwner v)
    vertexIndex (V v) = vertexIndex v
    nthOutcomingDart (V v) n = D (nthOutcomingDart v n)
    outcomingDarts (V v) = map D (outcomingDarts v)

    newtype Dart Link ct = D (Dart Tangle ct) deriving (Eq, Ord, Ix)
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


instance Knotted Link where
    numberOfFreeLoops (L t) = numberOfFreeLoops t
    mapCrossings f (L t) = L (mapCrossings f t)
    crossingState (V v) = crossingState v

    type ExplodeType Link ct = (Int, [([(Int, Int)], CrossingState ct)])
    explode (L t) = let (f, [], l) = explode t in (f, l)
    implode (f, l) = L (implode (f, [], l))


produceShowDart ''Link ''Dart (const [])
produceShowCrossing ''Link ''Vertex
produceShowKnot ''Link


emptyLink :: Link ct
emptyLink = L emptyTangle


linkToTangle :: Link ct -> Tangle ct
linkToTangle (L t) = t


tangleToLink :: Tangle ct -> Link ct
tangleToLink t | numberOfLegs t == 0  = L t
               | otherwise            = error "tangleToLink: tangle must have 0 legs"
