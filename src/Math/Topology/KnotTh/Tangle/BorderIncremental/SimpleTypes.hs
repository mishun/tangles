module Math.Topology.KnotTh.Tangle.BorderIncremental.SimpleTypes
    ( module Math.Topology.KnotTh.Tangle.BorderIncremental.IncrementalGluing
    , primeProjectionType
    , reducedProjectionType
    , templateProjectionType
    , primeDiagramType
    , primeIrreducibleDiagramType
    , triangleBoundedType
    ) where

import qualified Math.Algebra.Group.Dn as Dn
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tangle.BorderIncremental.IncrementalGluing
import Math.Topology.KnotTh.Tangle.BorderIncremental.IncrementalTests


primeProjectionType :: GluingType ProjectionCrossing Dn.DnSubGroup Dn.DnSubGroup
primeProjectionType = GluingType
    { preGlueTest  = \ _ _ _ -> True
    , postGlueTest = \ _ _ _ s -> return $! s
    }


reducedProjectionType :: GluingType ProjectionCrossing Dn.DnSubGroup Dn.DnSubGroup
reducedProjectionType = GluingType
    { preGlueTest  = const testNoMultiEdges
    , postGlueTest = \ _ _ _ s -> return $! s
    }


templateProjectionType :: GluingType ProjectionCrossing Dn.DnSubGroup Dn.DnSubGroup
templateProjectionType = GluingType
    { preGlueTest  = \ _ leg gl ->
        let t = dartTangle leg
            n = numberOfCrossings t
            l = numberOfLegs t
        in (n == 1 || l > 4) && testNoMultiEdges leg gl
    , postGlueTest = \ root gl _ s ->
        if gl < 3 || testFlow4 root
            then return $! s
            else Nothing
    }


primeDiagramType :: GluingType ArbitraryCrossing Dn.DnSubGroup Dn.DnSubGroup
primeDiagramType = GluingType
    { preGlueTest  = \ _ _ _ -> True
    , postGlueTest = \ _ _ _ s -> return $! s
    }


primeIrreducibleDiagramType :: GluingType ArbitraryCrossing Dn.DnSubGroup Dn.DnSubGroup
primeIrreducibleDiagramType = GluingType
    { preGlueTest  = testNo2ndReidemeisterReduction
    , postGlueTest = \ _ _ _ s -> return $! s
    }


triangleBoundedType :: Int -> GluingType ct a b -> GluingType ct a b
triangleBoundedType maxN gt = gt
    { preGlueTest = \ cr leg gl ->
        let t = dartTangle leg
        in (diagonalIndex (1 + numberOfCrossings t) (nextNumberOfLegs (numberOfLegs t) gl) <= diagonalIndex maxN 4)
            && preGlueTest gt cr leg gl
    }
