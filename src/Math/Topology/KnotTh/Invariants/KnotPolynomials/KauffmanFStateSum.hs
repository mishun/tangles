{-# LANGUAGE MultiParamTypeClasses #-}
module Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanFStateSum
    ( KauffmanFArg(..)
    , ChordDiagramsSum
    ) where

import Control.DeepSeq
import qualified Data.Array as A
import Data.Function (on)
import Data.List (intercalate, foldl')
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Text.Printf
import Math.Topology.KnotTh.Algebra
import Math.Topology.KnotTh.Knotted.Threads
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Moves.AdHoc
import Math.Topology.KnotTh.Moves.ModifyDSL
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.LinkingNumbers
import Math.Topology.KnotTh.Invariants.Util.Poly


class (Eq a, Ord a, Num a) => KauffmanFArg a where
    twistFactor  :: Int -> a
    smoothFactor :: a
    loopFactor   :: a
    swapTwists   :: a -> a


instance KauffmanFArg Poly2 where
    twistFactor p = monomial2 1 "a" (fromIntegral p / 1)
    smoothFactor  = monomial2 1 "z" 1
    loopFactor    = (twistFactor 1 + twistFactor (-1)) * monomial2 1 "z" (-1) - 1
    swapTwists    = invert2 "a"


data ChordDiagram a = ChordDiagram !(UV.Vector Int) !a
    deriving (Eq, Ord)


instance Functor ChordDiagram where
    fmap f (ChordDiagram p x) = ChordDiagram p $ f x


instance (NFData a) => NFData (ChordDiagram a) where
    rnf (ChordDiagram p x) = p `seq` rnf x


instance (Show a) => Show (ChordDiagram a) where
    show (ChordDiagram a x) =
        printf "(%s)%s" (show x) (show $ UV.toList a)


data ChordDiagramsSum a = ChordDiagramsSum {-# UNPACK #-} !Int ![ChordDiagram a]
    deriving (Eq, Ord)


instance Functor ChordDiagramsSum where
    fmap f (ChordDiagramsSum order list) = ChordDiagramsSum order $ map (fmap f) list


instance (NFData a) => NFData (ChordDiagramsSum a) where
    rnf (ChordDiagramsSum _ list) = rnf list


instance (Show a) => Show (ChordDiagramsSum a) where
    show (ChordDiagramsSum _ list) =
        case list of
            [] -> "0"
            _  -> intercalate "+" $ map show list


singletonStateSum :: ChordDiagram a -> ChordDiagramsSum a
singletonStateSum summand@(ChordDiagram a _) =
    ChordDiagramsSum (UV.length a) [summand]


concatStateSums :: (Eq a, Num a) => [ChordDiagramsSum a] -> ChordDiagramsSum a
concatStateSums [] = error $ printf "concatStateSum: empty"
concatStateSums list @ (ChordDiagramsSum order _ : _) =
    let s = map (\ (!k, !v) -> ChordDiagram k v) $
            filter ((/= 0) . snd) $ M.toList $
                foldl' (\ !m (ChordDiagram !k !v) -> M.insertWith' (+) k v m) M.empty $
                    concatMap (\ (ChordDiagramsSum order' list') ->
                            if order' == order
                                then list'
                                else error $ printf "concatStateSums: order conflict with %i and %i" order order'
                        ) list
    in ChordDiagramsSum order s


mapStateSum :: (Eq a, Num a) => (ChordDiagram a -> ChordDiagramsSum a) -> ChordDiagramsSum a -> ChordDiagramsSum a
mapStateSum _ (ChordDiagramsSum order []) = ChordDiagramsSum order []
mapStateSum f (ChordDiagramsSum _ list) = concatStateSums $ map f list


{-# INLINE haveIntersection #-}
haveIntersection :: (Int, Int) -> (Int, Int) -> Bool
haveIntersection (!a', !b') (!c', !d') =
    let a = min a' b' ; b = max a' b'
        c = min c' d' ; d = max c' d'
    in (a < c && b > c && b < d) || (a > c && a < d && b > d)


{-# INLINE canonicalOver #-}
canonicalOver :: Int -> (Int, Int) -> (Int, Int) -> Bool
canonicalOver _ (!a, !b) (!c, !d) =
    min a b < min c d
--    let sa = min (abs $ a - b) $ n - abs (a - b)
--        sb = min (abs $ c - d) $ n - abs (c - d)
--    in (sa, min a b) < (sb, min c d)


restoreBasicTangle :: UV.Vector Int -> TangleDiagram
restoreBasicTangle !chordDiagram =
    let cdl = UV.length chordDiagram

        restore :: UV.Vector Int -> V.Vector (Int, Int) -> [Int] -> TangleDiagram
        restore _ _ [] = error "restoreBasicTangle: impossible happened"
        restore a h (i : rest) =
            if | l == 0                           -> planarEmpty
               | l == 2                           -> planarPropagator 1
               | i' == j                          ->
                   let tangle = restore (UV.generate (l - 2) (\ x -> ((a UV.! ((i + 2 + x) `mod` l)) - i - 2) `mod` l))
                                        (V.generate (l - 2) $ \ x -> h V.! ((i + 2 + x) `mod` l))
                                        [0 .. l - 3]
                   in rotateBy i $ horizontalComposition 0 (planarPropagator 1, 0) (tangle, 0)
               | haveIntersection (i, i') (j, j') ->
                   let tangle = restore (a UV.// [(i, j'), (j, i'), (i', j), (j', i)]) (h V.// [(i, h V.! j), (j, h V.! i)]) [0 .. l - 1]
                   in rotateBy i $ vertexOwner $ glueToBorder 2 (tangle, j) $
                       overCrossingIf $ canonicalOver cdl (h V.! i) (h V.! j)
               | otherwise                        -> restore a h rest
            where
                l = UV.length a
                i' = a UV.! i
                j = (i + 1) `mod` l
                j' = a UV.! j

    in if | cdl == 0  -> planarEmpty
          | otherwise -> restore chordDiagram (V.generate cdl $ \ i -> (i, chordDiagram UV.! i)) [0 .. cdl - 1]


data ThreadTag = BorderThread {-# UNPACK #-} !(Int, Int) {-# UNPACK #-} !Int
               | InternalThread {-# UNPACK #-} !Int {-# UNPACK #-} !Int


irregularCrossings :: TangleDiagram -> [TangleDiagramVertex]
irregularCrossings tangle =
    let (_, _, threads) = allThreadsWithMarks tangle

        expectedPassOver =
            let tags :: A.Array TangleDiagramDart ThreadTag
                tags = A.array (dartsRange tangle) $ do
                    (tid, thread) <- threads
                    let make = case thread of
                            []                     -> error "internal error"
                            (h, _) : _ | isDart h  -> InternalThread $ numberOfLegs tangle + tid
                                       | otherwise -> let a = legPlace h
                                                          b = legPlace $ snd $ last thread
                                                      in BorderThread (min a b, max a b)
                    (ord, (a, b)) <- [0 ..] `zip` thread
                    [(a, make $ 2 * ord), (b, make $ 2 * ord + 1)]

                tagPassOver _ (InternalThread a ai) (InternalThread b bi) = (a, ai) < (b, bi)
                tagPassOver _ (InternalThread _ _) (BorderThread _ _) = False
                tagPassOver _ (BorderThread _ _) (InternalThread _ _) = True
                tagPassOver n (BorderThread a ai) (BorderThread b bi)
                    | a == b                = ai < bi
                    | haveIntersection a b  = canonicalOver n a b
                    | otherwise             = a < b

            in \ d -> on (tagPassOver $ numberOfLegs tangle) (tags A.!) d (nextCCW d)

    in filter (\ c ->
            let d0 = nthOutcomingDart c 0
            in isPassingOver d0 /= expectedPassOver d0
       ) $ allVertices tangle


decomposeTangle :: (KauffmanFArg a) => [(Int, [(Int, Int)], [([(Int, Int)], DiagramCrossing)])] -> a -> TangleDiagram -> ChordDiagramsSum a
decomposeTangle path !initialFactor !tangle' =
    let splices [] toInvert factor inter =
            let tangle = modifyKnot tangle' $ modifyC False transposeIt toInvert

                (n, _, threads) = allThreadsWithMarks tangle

                a = (UV.replicate (numberOfLegs tangle ) 0 UV.//) $ do
                        (_, thread) <- threads
                        case thread of
                            (h, _) : _ | isLeg h ->
                                let i = legPlace $ fst $ head thread
                                    j = legPlace $ snd $ last thread
                                in [(i, j), (j, i)]
                            _                    -> []

            in {- (if length path >= 10 then trace (show $ explode tangle' : path) else id) $ -}
                    (: map (uncurry $ decomposeTangle (explode tangle' : path)) inter) $
                        singletonStateSum $ ChordDiagram a $
                            let w = totalSelfWrithe' tangle
                            in factor * twistFactor w * loopFactor ^ (n - numberOfLegs tangle `div` 2)

        splices (h : r) toInvert factor inter =
            let a = (factor * smoothFactor, modifyKnot tangle' $ modifyC False transposeIt toInvert >> smoothA h >> greedy [reduce2nd])
                b = (factor * smoothFactor, modifyKnot tangle' $ modifyC False transposeIt toInvert >> smoothB h >> greedy [reduce2nd])
            in splices r (h : toInvert) (-factor) (a : b : inter)

    in concatStateSums $ splices (irregularCrossings tangle') [] initialFactor []


instance (KauffmanFArg a) => RotationAction (ChordDiagramsSum a) where
    rotationOrder (ChordDiagramsSum d _) = d

    rotateByUnchecked !rot =
        mapStateSum $ \ (ChordDiagram a factor) ->
            decomposeTangle [] factor $ rotateBy rot $ restoreBasicTangle a

instance (KauffmanFArg a) => MirrorAction (ChordDiagramsSum a) where
    mirrorIt =
        mapStateSum $ \ (ChordDiagram a factor) ->
            decomposeTangle [] factor $ mirrorIt $ restoreBasicTangle a

instance (KauffmanFArg a) => TensorProduct (ChordDiagramsSum a) where
    a ⊗ b = horizontalComposition 0 (a, 0) (b, 0)

instance (KauffmanFArg a) => PlanarAlgebra (ChordDiagramsSum a) where
    planarDegree (ChordDiagramsSum d _) = d

    planarEmpty = ChordDiagramsSum 0 [ChordDiagram UV.empty 1]

    planarLoop n = ChordDiagramsSum 0 [ChordDiagram UV.empty (loopFactor ^ n)]

    planarPropagator n | n < 0      = error $ printf "planarPropagator: parameter must be non-negative, but %i passed" n
                       | otherwise  = ChordDiagramsSum (2 * n) [ChordDiagram (UV.generate (2 * n) $ \ i -> 2 * n - 1 - i) 1]

    horizontalCompositionUnchecked gl (sumA, !posA) (!sumB, !posB) =
        flip mapStateSum sumA $ \ (ChordDiagram xa ka) ->
            let ta = restoreBasicTangle xa
            in flip mapStateSum sumB $ \ (ChordDiagram xb kb) ->
                let tb = restoreBasicTangle xb
                in decomposeTangle [explode ta, explode tb] (ka * kb) $
                    horizontalComposition gl (ta, posA) (tb, posB)

instance (KauffmanFArg a) => TransposeAction (ChordDiagramsSum a) where
    transposeIt = fmap swapTwists

instance (KauffmanFArg a) => SkeinRelation ChordDiagramsSum a where
    skeinLPlus =
        singletonStateSum $ ChordDiagram (UV.fromList [2, 3, 0, 1]) 1

    skeinLMinus =
        concatStateSums $ map singletonStateSum
            [ ChordDiagram (UV.fromList [2, 3, 0, 1]) (-1)
            , ChordDiagram (UV.fromList [3, 2, 1, 0]) smoothFactor
            , ChordDiagram (UV.fromList [1, 0, 3, 2]) smoothFactor
            ]

    takeAsScalar s =
        case s of
            ChordDiagramsSum 0 []                 -> 0
            ChordDiagramsSum 0 [ChordDiagram _ x] -> x
            _                                     -> error "scalarStateSum: constant expected"
