module Math.Topology.KnotTh.Crossings.Arbitrary
    ( module X
    , invertCrossings
    , is1stOr2ndReidemeisterReducible
    ) where

import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Arbitrary.Arbitrary as X
import Math.Topology.KnotTh.Crossings.Arbitrary.Writhe as X
import Math.Topology.KnotTh.Crossings.Arbitrary.Alternating as X


invertCrossings :: (Knotted k) => k ArbitraryCrossing -> k ArbitraryCrossing
invertCrossings = mapCrossings invertCrossing


is1stOr2ndReidemeisterReducible :: (Knotted k, Eq (Dart k ArbitraryCrossing)) => k ArbitraryCrossing -> Bool
is1stOr2ndReidemeisterReducible knot = or $ do
    c <- allVertices knot
    a <- outcomingDarts c
    let b = opposite a
        r1 = nextCCW a == b
        r2 = isDart b && (passOver a == passOver b) && (nextCCW a == opposite (nextCW b))
    return $! r1 || r2
 