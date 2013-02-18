{-# LANGUAGE TypeFamilies #-}
module Math.KnotTh.Invariants.HomflyPolynomial
    ( homflyPolynomial
    ) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein
import Math.KnotTh.Invariants.Util.Poly


a, a', z' :: Poly2
a  = monomial 1 "a" 1
a' = monomial 1 "a" (-1)
z' = monomial 1 "z" (-1)


data HomflyRelation = HomflyRelation


instance SkeinRelation HomflyRelation Poly2 where
    type SkeinRelationModel HomflyRelation = ChordDiagramsSum

    circleFactor _ = (a + a') * z' - 1

    initialLplus _ = [(Lplus, 1)]

    twistPFactor _ = a
    twistNFactor _ = a'

    smoothLplusFactor  = undefined
    smoothLzeroFactor  = undefined
    smoothLinftyFactor = undefined

    finalNormalization _ knot =
        let factor =
                let w = selfWrithe knot
                in (if w <= 0 then a else a') ^ abs w
        in (factor *)


homflyPolynomial :: (SkeinStructure k c d) => k ArbitraryCrossing -> ResultOnStructure k ChordDiagramsSum Poly2
homflyPolynomial = evaluateSkeinRelation HomflyRelation
