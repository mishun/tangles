module Math.KnotTh.Invariants.HomflyPolynomial
    ( homflyPolynomial
    ) where

import qualified Data.Map as M
import qualified Math.Algebra.Field.Base as B
import qualified Math.Projects.KnotTheory.LaurentMPoly as LP
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.Applied


type Poly = LP.LaurentMPoly Int


monomial :: Int -> String -> B.Q -> Poly
monomial a var d = LP.LP [(LP.LM $ M.fromList [(var, d)], a)]


data HomflyRelation a = HomflyRelation a a a a


instance (Ord a, Num a, Show a) => SkeinRelation (HomflyRelation a) a where
    circleFactor (HomflyRelation a a' _ z') = (a + a') * z' - 1

    initialLplus _ = InitialSum { ofLplus = 1, ofLzero = 0, ofLinfty = 0 }

    twistPFactor (HomflyRelation a _ _ _) = a
    twistNFactor (HomflyRelation _ a' _ _) = a'

    smoothLplusFactor  = undefined
    smoothLzeroFactor  = undefined
    smoothLinftyFactor = undefined

    finalNormalization (HomflyRelation a a' _ _) knot =
        let factor =
                let w = selfWrithe knot
                in (if w <= 0 then a else a') ^ (abs w)
        in (factor *)


homflyPolynomial :: (SkeinStructure k c d) => k ArbitraryCrossing -> SkeinResult k Poly
homflyPolynomial = evaluateSkeinRelation $
    HomflyRelation
        (monomial 1 "a" 1) (monomial 1 "a" (-1))
        (monomial 1 "z" 1) (monomial 1 "z" (-1))
