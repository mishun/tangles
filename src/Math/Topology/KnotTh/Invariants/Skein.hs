module Math.Topology.KnotTh.Invariants.Skein
    ( module X
    , evaluateSkeinRelation
    ) where

import Math.Topology.KnotTh.Crossings.Arbitrary
import Math.Topology.KnotTh.Invariants.Skein.Relation as X
import Math.Topology.KnotTh.Invariants.Skein.SkeinM
import Math.Topology.KnotTh.Invariants.Skein.StateModels.PlanarDiagramsSum as X
import Math.Topology.KnotTh.Invariants.Skein.StateModels.ChordDiagramsSum as X


evaluateSkeinRelation :: (SkeinRelation r a, SkeinStructure k) => r -> k ArbitraryCrossing -> ResultOnStructure k (SkeinRelationModel r) a
evaluateSkeinRelation relation =
    runSkeinStrategy relation $
        let try [] = error "evaluateSkeinRelation: impossible happened"
            try ((v, i) : t) = do
                (u, _) <- neighbour (v, i)
                if u /= v
                    then return $! Contract (v, i)
                    else try t
        in try
