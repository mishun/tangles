module Math.KnotTh.Invariants.JonesPolynomial
    ( jonesPolynomial
    , kauffmanXPolynomial
    , normalizedJonesPolynomialOfLink
    , minimalJonesPolynomialOfLink
    , minimalKauffmanXPolynomialOfLink
    , minimalJonesPolynomialOfTangle
    ) where

import Data.List (sort)
import Data.Array.Base ((!))
import Data.Array.ST (runSTUArray, newArray_, writeArray)
import Control.Monad (forM_)
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T
import Math.KnotTh.Invariants.Skein.Applied
import Math.KnotTh.Invariants.Util.Poly


data BracketLikeRelation a = BracketLikeRelation a a


instance (Ord a, Num a, Show a) => SkeinRelation (BracketLikeRelation a) a where
    circleFactor (BracketLikeRelation a b) = -(a * a + b * b)

    initialLplus (BracketLikeRelation a b) = [(Lzero, a), (Linfty, b)]

    twistPFactor = undefined
    twistNFactor = undefined

    smoothLplusFactor  = undefined
    smoothLzeroFactor  = undefined
    smoothLinftyFactor = undefined

    finalNormalization (BracketLikeRelation a b) knot =
        let factor =
                let w = selfWrithe knot
                in (if w <= 0 then -a else -b) ^ abs (3 * w)
        in (factor *)


jonesVar, kauffmanXVar :: String
jonesVar = "t"
kauffmanXVar = "A"


jonesPolynomial :: (SkeinStructure k c d) => k ArbitraryCrossing -> SkeinResult k Poly
jonesPolynomial = evaluateSkeinRelation $ BracketLikeRelation (monomial 1 jonesVar (-1 / 4)) (monomial 1 jonesVar (1 / 4))


normalizedJonesPolynomialOfLink :: L.NonAlternatingLink -> Poly
normalizedJonesPolynomialOfLink link
    | (numberOfFreeLoops link == 0) && (numberOfCrossings link == 0)  =
        error "jonesPolynomialOfLink: empty link provided"
    | otherwise                                                       =
        normalizeBy (1 + monomial 1 jonesVar 1) (monomial (-1) jonesVar (1 / 2) * jonesPolynomial link)


kauffmanXPolynomial :: (SkeinStructure k c d) => k ArbitraryCrossing -> SkeinResult k Poly
kauffmanXPolynomial = evaluateSkeinRelation $ BracketLikeRelation (monomial 1 kauffmanXVar 1) (monomial 1 kauffmanXVar (-1))


minimalJonesPolynomialOfLink :: L.NonAlternatingLink -> Poly
minimalJonesPolynomialOfLink link =
    let jp = jonesPolynomial link
    in min jp (invert jonesVar jp)


minimalKauffmanXPolynomialOfLink :: L.NonAlternatingLink -> Poly
minimalKauffmanXPolynomialOfLink link =
    let kp = kauffmanXPolynomial link
    in min kp (invert kauffmanXVar kp)


minimalJonesPolynomialOfTangle :: T.NonAlternatingTangle -> StateSum Poly
minimalJonesPolynomialOfTangle tangle
    | l == 0     = min jp (map (fmap $ invert jonesVar) jp)
    | otherwise  = minimum $ do
        rot <- [0 .. l - 1]

        let mapSum fx fm s = sort $ flip map s $ \ (StateSummand x m) ->
                flip StateSummand (fm m) $ runSTUArray $ do
                    x' <- newArray_ (0, l - 1)
                    forM_ [0 .. l - 1] $ \ i ->
                        writeArray x' (fx i `mod` l) (fx (x ! i) `mod` l)
                    return $! x'

        f <- [ mapSum (+ rot) id
             , mapSum (+ rot) (invert jonesVar)
             , mapSum (\ i -> rot - i) id
             , mapSum (\ i -> rot - i) (invert jonesVar)
             ]

        return $! f jp
    where
        jp = jonesPolynomial tangle
        l = T.numberOfLegs tangle
