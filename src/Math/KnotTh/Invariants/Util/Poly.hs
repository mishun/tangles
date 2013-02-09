module Math.KnotTh.Invariants.Util.Poly
    ( Poly2
    , monomial2
    , invert2
    , normalizeBy2
    , Poly
    , monomial
    , invert
    , normalizeBy
    ) where

import Data.Ratio ((%), numerator)
import qualified Data.Map as M
import qualified Math.Algebra.Field.Base as B
import qualified Math.Projects.KnotTheory.LaurentMPoly as LMP


type Poly2 = LMP.LaurentMPoly Int


monomial2 :: Int -> String -> B.Q -> Poly2
monomial2 a var d = LMP.LP [(LMP.LM $ M.fromList [(var, d)], a)]


invert2 :: String -> Poly2 -> Poly2
invert2 var (LMP.LP monomials) = sum $ do
    (LMP.LM vars, coeff) <- monomials
    let modify p@(x, d)
            | x == var   = (x, -d)
            | otherwise  = p
    return $! LMP.LP [(LMP.LM $ M.fromList $ map modify $ M.toList vars, coeff)]

normalizeBy2 :: Poly -> Poly -> Poly
normalizeBy2 (LMP.LP denominator) (LMP.LP monomials)
    | remainder /= 0  = error "normalizeBy: non-divisible"
    | otherwise       =
        let (LMP.LP q') = factor * quotient
        in LMP.LP $ map (\ (a, b) -> (a, numerator b)) q'
    where
        factor = LMP.LP [(LMP.LM $ M.map (+ (-1)) $ foldl (\ a (LMP.LM b, _) -> M.unionWith min a b) M.empty monomials, 1)]
        (quotient, remainder) = LMP.quotRemLP
            (recip factor * (LMP.LP $ map (\ (a, b) -> (a, b % 1)) monomials))
            (LMP.LP $ map (\ (a, b) -> (a, b % 1)) denominator)


type Poly = LMP.LaurentMPoly Int


monomial :: Int -> String -> B.Q -> Poly
monomial a var d = LMP.LP [(LMP.LM $ M.fromList [(var, d)], a)]


invert :: String -> Poly -> Poly
invert var (LMP.LP monomials) = sum $ do
    (LMP.LM vars, coeff) <- monomials
    let modify p@(x, d)
            | x == var   = (x, -d)
            | otherwise  = p
    return $! LMP.LP [(LMP.LM $ M.fromList $ map modify $ M.toList vars, coeff)]


normalizeBy :: Poly -> Poly -> Poly
normalizeBy (LMP.LP denominator) (LMP.LP monomials)
    | remainder /= 0  = error "normalizeBy: non-divisible"
    | otherwise       =
        let (LMP.LP q') = factor * quotient
        in LMP.LP $ map (\ (a, b) -> (a, numerator b)) q'
    where
        factor = LMP.LP [(LMP.LM $ M.map (+ (-1)) $ foldl (\ a (LMP.LM b, _) -> M.unionWith min a b) M.empty monomials, 1)]
        (quotient, remainder) = LMP.quotRemLP
            (recip factor * (LMP.LP $ map (\ (a, b) -> (a, b % 1)) monomials))
            (LMP.LP $ map (\ (a, b) -> (a, b % 1)) denominator)
