module Math.KnotTh.Invariants.Util.Poly
    ( Poly2
    , monomial2
    , normalizeKauffmanF
    , Poly
    , monomial
    , invert
    , normalizeJones
    ) where

import Data.Ratio ((%), numerator)
import qualified Data.Map as M
import qualified Math.Algebra.Field.Base as B
import qualified Math.Projects.KnotTheory.LaurentMPoly as LMP


type Poly2 = LMP.LaurentMPoly Int


monomial2 :: Int -> String -> B.Q -> Poly2
monomial2 a var d = LMP.LP [(LMP.LM $ M.fromList [(var, d)], a)]


normalizeKauffmanF :: Poly2 -> Poly2
normalizeKauffmanF (LMP.LP mono) = let (LMP.LP q') = recip big * q in LMP.LP $ map (\ (a', b') -> (a', numerator b')) q'
    where
        a = LMP.var "a"
        z = LMP.var "z"
        big = (a * z) ^ (100 :: Int)
        p = LMP.LP $ map (\ (a', b') -> (a', b' % 1)) mono
        (q, _) = LMP.quotRemLP (big * a * z * p) (a * a + 1 - a * z)


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


normalizeJones :: Poly -> Poly
normalizeJones (LMP.LP mono) = let (LMP.LP q') = recip big * q in LMP.LP $ map (\ (a, b) -> (a, numerator b)) q'
    where
        t = LMP.var "t"
        big = t ^ (100 :: Int)
        p = LMP.LP $ map (\ (a, b) -> (a, b % 1)) mono
        (q, _) = LMP.quotRemLP (big * p * (-LMP.sqrtvar "t")) (1 + t)
