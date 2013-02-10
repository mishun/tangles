module Math.KnotTh.Invariants.Skein
    ( module Math.KnotTh.Invariants.Skein.Relation
    , module Math.KnotTh.Invariants.Skein.StateModels.ChordDiagramsSum
    , evaluateSkeinRelation
    ) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.SkeinM
import Math.KnotTh.Invariants.Skein.StateModels.ChordDiagramsSum


evaluateSkeinRelation :: (SkeinRelation r a, SkeinStructure k c d) => r -> k ArbitraryCrossing -> ResultOnStructure k (SkeinRelationModel r) a
evaluateSkeinRelation relation = runSkein relation $ \ starts -> do
    let try [] = error "impossible"
        try ((v, i) : t) = do
            (u, _) <- neighbour (v, i)
            if u /= v
                then contract (v, i)
                else try t
    try starts
