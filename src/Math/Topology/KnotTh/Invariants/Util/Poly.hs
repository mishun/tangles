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
    , dubrovnikToKauffmanF
    ) where

import Control.Arrow (second)
import Control.Exception (assert)
import qualified Data.Map as Map
import Data.Ratio (Ratio, (%), numerator)
import Text.Printf
import qualified Math.Algebra.Field.Base as B
import qualified Math.Projects.KnotTheory.LaurentMPoly as LMP


monomialPoly :: a -> String -> Ratio Integer -> LMP.LaurentMPoly a
monomialPoly a var d = LMP.LP [(LMP.LM $ Map.fromList [(var, B.Q d)], a)]


invertPoly :: (Integral a) => String -> LMP.LaurentMPoly a -> LMP.LaurentMPoly a
invertPoly var (LMP.LP monomials) = sum $ do
    (LMP.LM vars, coeff) <- monomials
    let modify x d | x == var   = -d
                   | otherwise  = d
    return $! LMP.LP [(LMP.LM $ Map.mapWithKey modify vars, coeff)]


normalizePoly :: (Integral a, Show a) => LMP.LaurentMPoly a -> LMP.LaurentMPoly a -> LMP.LaurentMPoly a
normalizePoly (LMP.LP denominator) (LMP.LP monomials)
    | remainder /= 0  = error $ printf "normalizeBy: non-divisible %s by %s" (show $ LMP.LP denominator) (show $ LMP.LP monomials)
    | otherwise       =
        let (LMP.LP q') = factor * quotient
        in LMP.LP $ map (second numerator) q'
    where
        factor =
            -- let mono = Map.map (min 0) $ foldl (\ a (LMP.LM b, _) -> Map.unionWith min a b) Map.empty monomials
            let mono = Map.map (min 0) $ Map.unionsWith min $ map (\ (LMP.LM b, _) -> b) monomials
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
        let next = case Map.assocs m of
                [("a", p)] | odd (B.numeratorQ p) -> (LMP.LM $ Map.singleton "t" (p / (-4)), coeff)
                           | otherwise            -> (LMP.LM $ Map.singleton "t" (p / (-4)), coeff)
                _                                 -> (LMP.LM m, coeff)
        return $ LMP.LP [next]


dubrovnikToKauffmanF :: Int -> Int -> Poly2 -> Poly2
dubrovnikToKauffmanF links comps (LMP.LP monomials) =
     sum $ do
        (LMP.LM m, coeff) <- monomials
        let an = B.numeratorQ $ Map.findWithDefault 0 "a" m
            zn = B.numeratorQ $ Map.findWithDefault 0 "z" m

            powerI = 3 * (an - fromIntegral links) + zn

            signs = assert (even powerI) $ fromIntegral comps + div powerI 2

        return $ assert (even $ an + zn) $ LMP.LP [(LMP.LM m, if odd signs then -coeff else coeff)]
