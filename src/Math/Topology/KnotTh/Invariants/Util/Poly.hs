module Math.Topology.KnotTh.Invariants.Util.Poly
    ( Poly2
    , monomial2
    , invert2
    , normalizeBy2
    , Poly
    , monomial
    , invert
    , normalizeBy
    , kauffmanXToJones
    ) where

import Data.Ratio (Ratio, (%), numerator)
import qualified Data.Map as M
import Control.Arrow (second)
import qualified Math.Algebra.Field.Base as B
import qualified Math.Projects.KnotTheory.LaurentMPoly as LMP


monomialPoly :: a -> String -> Ratio Integer -> LMP.LaurentMPoly a
monomialPoly a var d = LMP.LP [(LMP.LM $ M.fromList [(var, B.Q d)], a)]


invertPoly :: (Integral a) => String -> LMP.LaurentMPoly a -> LMP.LaurentMPoly a
invertPoly var (LMP.LP monomials) = sum $ do
    (LMP.LM vars, coeff) <- monomials
    let modify x d | x == var   = -d
                   | otherwise  = d
    return $! LMP.LP [(LMP.LM $ M.mapWithKey modify vars, coeff)]


normalizePoly :: (Integral a) => LMP.LaurentMPoly a -> LMP.LaurentMPoly a -> LMP.LaurentMPoly a
normalizePoly (LMP.LP denominator) (LMP.LP monomials)
    | remainder /= 0  = error "normalizeBy: non-divisible"
    | otherwise       =
        let (LMP.LP q') = factor * quotient
        in LMP.LP $ map (second numerator) q'
    where
        factor =
            let mono = M.map (min 0) $ foldl (\ a (LMP.LM b, _) -> M.unionWith min a b) M.empty monomials
            in LMP.LP [(LMP.LM mono, 1)]

        (quotient, remainder) =
            let cutoff = LMP.LP . map (second (% 1))
            in LMP.quotRemLP (recip factor * cutoff monomials) (cutoff denominator)


type Poly2 = LMP.LaurentMPoly Int


monomial2 :: Int -> String -> Ratio Integer -> Poly2
monomial2 = monomialPoly


invert2 :: String -> Poly2 -> Poly2
invert2  = invertPoly


normalizeBy2 :: Poly -> Poly -> Poly
normalizeBy2 = normalizePoly


type Poly = LMP.LaurentMPoly Int


monomial :: Int -> String -> Ratio Integer -> Poly
monomial = monomialPoly


invert :: String -> Poly -> Poly
invert = invertPoly


normalizeBy :: Poly -> Poly -> Poly
normalizeBy = normalizePoly


kauffmanXToJones :: Poly -> Poly
kauffmanXToJones (LMP.LP monomials) =
    sum $ do
        (LMP.LM m, coeff) <- monomials
        let next = case M.assocs m of
                [("a", p)] | odd (B.numeratorQ p) -> (LMP.LM $ M.fromList [("t", p / (-4))], coeff)
                           | otherwise            -> (LMP.LM $ M.fromList [("t", p / (-4))], coeff)
                _                                 -> (LMP.LM m, coeff)
        return $ LMP.LP [next]
