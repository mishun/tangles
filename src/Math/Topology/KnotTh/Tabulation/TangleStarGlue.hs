{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.Tabulation.TangleStarGlue
    ( tangleStarGlue
    ) where

import Control.DeepSeq
import qualified Control.Monad.State.Strict as State
import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Data.Map.Strict as Map
import Math.Topology.KnotTh.Algebra.Dihedral.Dn
import Math.Topology.KnotTh.ChordDiagram
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.EmbeddedLink


tangleStarGlue
    :: (NFData a, Crossing a, ChordDiagram cd) => (Int -> [(cd, (Bool, Int))])
        -> (forall m'. (Monad m') => ((Tangle a, (SubGroup Dn, x)) -> m' ()) -> m' ())
            -> [EmbeddedLink a]

tangleStarGlue starGenerator tangleGenerator =
    let diagrams = map starGenerator [0 ..]
        tangles = State.execState (tangleGenerator $ \ (!tangle, (!symmetry, _)) -> State.modify' ((tangle, symmetry) :)) []
    in Map.elems $ Map.unions $ parMap rdeepseq
        (\ (tangle, tangleSymmetry) ->
            Map.fromList $ do
                let l = numberOfLegs tangle
                (star, (starMirror, starPeriod)) <- diagrams !! (l `div` 2)
                rot <- [0 .. gcd starPeriod (rotationPeriod tangleSymmetry) - 1]
                mir <- False : [True | not starMirror && not (hasReflectionPart tangleSymmetry)]
                let g = fromReflectionRotation l (mir, rot)
                    link = fromTangleAndStar star $ transform g tangle
                return (unrootedHomeomorphismInvariant link, link)
        ) tangles
