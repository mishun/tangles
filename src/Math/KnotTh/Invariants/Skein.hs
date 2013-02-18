module Math.KnotTh.Invariants.Skein
    ( module X
    , evaluateSkeinRelation
    ) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.Relation as X
import Math.KnotTh.Invariants.Skein.SkeinM
import Math.KnotTh.Invariants.Skein.StateModels.PlanarDiagramsSum as X
import Math.KnotTh.Invariants.Skein.StateModels.ChordDiagramsSum as X


evaluateSkeinRelation :: (SkeinRelation r a, SkeinStructure k c d) => r -> k ArbitraryCrossing -> ResultOnStructure k (SkeinRelationModel r) a
evaluateSkeinRelation relation =
    runSkeinStrategy relation $
        let try [] = error "evaluateSkeinRelation: impossible happened"
            try ((v, i) : t) = do
                (u, _) <- neighbour (v, i)
                if u /= v
                    then return $! Contract (v, i)
                    else try t
        in try
