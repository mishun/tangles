{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
module Math.Topology.KnotTh.Invariants.KhovanovHomology
    ( module Math.Topology.KnotTh.Algebra.Cobordism.DottedCobordism
    , KhovanovComplex(..)
    , testComplexBorders
    , khovanovComplex
    , khovanovHomologyBetti
    ) where

import Control.Arrow ((&&&))
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Text.Printf
import Math.Topology.KnotTh.Algebra.Homology
import qualified Math.Topology.KnotTh.Algebra.Cobordism.CobordismMatrix as CM
import Math.Topology.KnotTh.Algebra.Cobordism.DottedCobordism
import Math.Topology.KnotTh.Algebra.PlanarAlgebra.Reduction
import Math.Topology.KnotTh.Invariants.LinkingNumbers
import Math.Topology.KnotTh.Tangle


data BoundedChain c = Chain !(V.Vector (CM.CobordismMatrix c))
                    | Singl !(CobordismBorder (CM.CobordismMatrix c))

deriving instance (KhovanovCobordism c) => Show (BoundedChain c)


objectChain :: (KhovanovCobordism c) => CobordismBorder c -> BoundedChain c
objectChain = Singl . CM.singletonVector


morphismChain :: (KhovanovCobordism c) => c -> BoundedChain c
morphismChain = Chain . V.singleton . CM.singleton


testBorders :: (KhovanovCobordism c) => String -> V.Vector (CM.CobordismMatrix c) -> V.Vector (CM.CobordismMatrix c) -> V.Vector (CM.CobordismMatrix c)
testBorders msg pre b =
    let indent off = (replicate off ' ' ++)

        dumpChain off chain =
            unlines $ indent off "Chain:" : V.toList (V.imap (\ i m -> indent (off + 2) $ printf "Border %i:\n%s" i (dumpMatrix (off + 4) m)) chain)

        dumpMatrix off m =
            unlines $ ((indent off $ printf "Mat %ix%i" (CM.numberOfRows m) (CM.numberOfCols m)) :) $ do
                row <- [0 .. CM.numberOfRows m - 1]
                col <- [0 .. CM.numberOfCols m - 1]
                return $ indent (off + 4) $ show $ m CM.! (row, col)

    in case V.findIndex (not . isZeroCobordism) $ V.zipWith (∘) (V.tail b) b of
        Nothing -> b
        Just i  -> error $ printf "border square failed (%i ∘ %i) %s on\n%s\n\n\n\n%s\n\n\n\n%s\n\n"
                                (i + 1) i msg (dumpChain 2 pre) (dumpChain 2 b) (dumpMatrix 2 $ (b V.! (i + 1)) ∘ (b V.! i))


simplifyChain :: (KhovanovCobordism c) => BoundedChain c -> BoundedChain c
simplifyChain (Singl b) =
    Singl $ CM.fromVector $ V.concatMap (\ x ->
            let (delooped, factors) = delooping x
            in V.replicate (V.length factors) delooped
        ) $ CM.toVector b
simplifyChain (Chain borders) = Chain $ goL 0 $ goE 0 borders
    where
        goE d kh | d >= V.length kh  = kh
                 | otherwise         =
                     case CM.findIndex isIsomorphism $ kh V.! d of
                         Nothing         -> goE (d + 1) kh
                         Just (row, col) ->
                            let msg = printf "elimination at d = %i pos = %s" d (show (row, col))
                            in goE d (testBorders msg kh $ eliminateAt d row col kh)

        goL d kh | d > V.length kh  = goE 0 kh
                 | otherwise        =
                     let level | d == 0     = CM.toVector $ cobordismBorder0 $ kh V.! d
                               | otherwise  = CM.toVector $ cobordismBorder1 $ kh V.! (d - 1)
                     in case V.findIndex ((> 0) . numberOfLoops) level of
                            Nothing      -> goL (d + 1) kh
                            Just loopPos ->
                                let msg = printf "delooping d = %i pos = %i" d loopPos
                                in goL d (testBorders msg kh $ deloopAt d loopPos kh)

        eliminateAt d row col kh =
            let m = (kh V.! d)
                (eps, delta, gamma) = CM.minor (row, col) m
            in (kh V.//) $ [(d, eps - gamma ∘ CM.singleton (m CM.! (row, col)) ∘ delta)]
                                ++ [(d - 1, CM.removeRow col (kh V.! (d - 1))) | d > 0]
                                ++ [(d + 1, CM.removeCol row (kh V.! (d + 1))) | d < V.length kh - 1]

        deloopAt d loopPos kh =
            let (delooped, pairs) = delooping $
                    if | d == 0    -> CM.toVector (cobordismBorder0 $ kh V.! d) V.! loopPos
                       | otherwise -> CM.toVector (cobordismBorder1 $ kh V.! (d - 1)) V.! loopPos

                newLines = V.length pairs

                postDeloop m =
                    let cols = CM.toVector $ cobordismBorder0 m
                        rows = let v = CM.toVector $ cobordismBorder1 m
                               in V.take loopPos v V.++ V.replicate newLines delooped V.++ V.drop (loopPos + 1) v
                    in CM.matrix cols rows $ \ row col ->
                        if | row < loopPos             -> m CM.! (row, col)
                           | row >= loopPos + newLines -> m CM.! (row - newLines + 1, col)
                           | otherwise                 -> fst (pairs V.! (row - loopPos)) ∘ (m CM.! (loopPos, col))

                preDeloop m =
                    let rows = CM.toVector $ cobordismBorder1 m
                        cols = let v = CM.toVector $ cobordismBorder0 m
                               in V.take loopPos v V.++ V.replicate newLines delooped V.++ V.drop (loopPos + 1) v
                    in CM.matrix cols rows $ \ row col ->
                        if | col < loopPos             -> m CM.! (row, col)
                           | col >= loopPos + newLines -> m CM.! (row, col - newLines + 1)
                           | otherwise                 -> (m CM.! (row, loopPos)) ∘ snd (pairs V.! (col - loopPos))

            in (kh V.//) $ [(d - 1, postDeloop $ kh V.! (d - 1)) | d > 0]
                        ++ [(d, preDeloop $ kh V.! d) | d < V.length kh]


data KhovanovComplex c = Kh !Int !Int !(BoundedChain c)


deriving instance (KhovanovCobordism c) => Show (KhovanovComplex c)

instance (KhovanovCobordism c) => RotationAction (KhovanovComplex c) where
    rotationOrder (Kh legs _ _) = legs

    rotateByUnchecked rot (Kh legs shift chain) =
        Kh legs shift $
            case chain of
                Singl b -> Singl $ rotateBy rot b
                Chain c -> Chain $ V.map (rotateBy rot) c

instance (KhovanovCobordism c) => TensorProduct (KhovanovComplex c) where
    a ⊗ b = horizontalComposition 0 (a, 0) (b, 0)

instance (KhovanovCobordism c) => PlanarAlgebra (KhovanovComplex c) where
    planarDegree (Kh legs _ _) = legs

    planarLoop n = stripComplex $ Kh 0 0 (simplifyChain $ objectChain (planarLoop n))

    planarPropagator n = Kh (2 * n) 0 (objectChain $ planarPropagator n)

    horizontalCompositionUnchecked !gl (Kh legsA shiftA chainA, !posA) (Kh legsB shiftB chainB, !posB) =
        let composeO ao bo = horizontalComposition gl (ao, posA) (bo, posB)

            composeM am bm | CM.isEmpty am  = am
                           | CM.isEmpty bm  = bm
                           | otherwise      = horizontalComposition gl (am, posA) (bm, posB)

            glueChains (Singl a)  (Singl b)  = Singl $ composeO a b
            glueChains (Singl a)  (Chain bc) = Chain $ V.map (composeM (identityCobordism a)) bc
            glueChains (Chain ac) (Singl b)  = Chain $ V.map (`composeM` identityCobordism b) ac
            glueChains (Chain a)  (Chain b)  =
                let spanA = V.length a
                    spanB = V.length b

                    width d = max 0 $ min d spanA - max 0 (d - spanB) + 1

                    coords d w =
                        let pa = w + max 0 (d - spanB)
                        in (pa, d - pa)

                    layerA d | d == spanA  = cobordismBorder1 $ a V.! (d - 1)
                             | otherwise   = cobordismBorder0 $ a V.! d

                    layerB d | d == spanB  = cobordismBorder1 $ b V.! (d - 1)
                             | otherwise   = cobordismBorder0 $ b V.! d

                in Chain $ V.generate (spanA + spanB) $ \ d ->
                    let cols = V.generate (width d) $ \ w ->
                                let (pa, pb) = coords d w
                                in composeO (layerA pa) (layerB pb)

                        rows = V.generate (width $ d + 1) $ \ w ->
                                let (pa, pb) = coords (d + 1) w
                                in composeO (layerA pa) (layerB pb)

                    in CM.flatten $ CM.matrix cols rows $ \ row col ->
                        let (pa0, pb0) = coords d col
                            (pa1, pb1) = coords (d + 1) row
                        in if | pb1 == pb0             -> composeM (a V.! pa0) (identityCobordism $ layerB pb0)
                              | pa1 == pa0 && even pa0 -> composeM (identityCobordism $ layerA pa0) (b V.! pb0)
                              | pa1 == pa0 && odd pa0  -> negate $ composeM (identityCobordism $ layerA pa0) (b V.! pb0)
                              | otherwise              -> zeroCobordism (cols V.! col) (rows V.! row)

        in stripComplex $ Kh (legsA + legsB - 2 * gl) (shiftA + shiftB) (simplifyChain $ glueChains chainA chainB)


stripComplex :: (PreadditiveCobordism c) => KhovanovComplex c -> KhovanovComplex c
stripComplex (Kh legs offset (Chain chain)) =
    let zeroL = (== 0) . CM.vectorLength . cobordismBorder0 . (chain V.!)
        zeroR = (== 0) . CM.vectorLength . cobordismBorder1 . (chain V.!)

        go !l !r | l > r      = let brd | r >= 0     = cobordismBorder1 $ chain V.! r
                                        | otherwise  = cobordismBorder0 $ chain V.! l
                                in Kh legs (offset + l) (Singl brd)
                 | zeroL l    = go (l + 1) r
                 | zeroR r    = go l (r - 1)
                 | otherwise  = Kh legs (offset + l) (Chain $ V.force $ V.slice l (r - l + 1) chain)

    in go 0 (V.length chain - 1)
stripComplex kh = kh


testComplexBorders :: (PreadditiveCobordism c) => KhovanovComplex c -> Bool
testComplexBorders (Kh _ _ (Chain b)) = V.all isZeroCobordism (V.zipWith (∘) (V.tail b) b)
testComplexBorders _ = True


crossingComplex :: DiagramCrossing -> KhovanovComplex (DottedCobordism' Integer)
crossingComplex OverCrossing  = Kh 4 0 (morphismChain saddleCobordism)
crossingComplex UnderCrossing = Kh 4 0 (morphismChain saddleCobordism')


khovanovComplex :: TangleDiagram -> KhovanovComplex (DottedCobordism' Integer)
khovanovComplex tangle | ls == 0    = kh
                       | otherwise  = kh ⊗ planarLoop ls
    where ls = numberOfFreeLoops tangle
          kh = reduceWithDefaultStrategy $ fmap crossingComplex tangle


khovanovHomologyBetti :: TangleDiagram -> [(Int, Int)]
khovanovHomologyBetti tangle =
    let Kh _ shift0 chain = khovanovComplex tangle

        shift =
            let oriented = arbitraryOrientation tangle

                nminus = length $ filter (< 0) $ map selfWrithe $ allVertices oriented

                lns = V.sum $ V.imap (\ !i -> UV.sum . UV.imap (\ j w -> if i < j then abs w else 0)) $
                        linkingNumbersTable oriented

                crs = length $ filter (not . isSelfIntersection) $ allVertices oriented

            in shift0 - nminus - (crs - lns) `div` 2

    in filter ((/= 0) . snd) $
        case chain of
            Singl space  -> [(shift, V.sum $ V.map tqftBorderDim $ CM.toVector space)]
            Chain border ->
                let dim = V.length border

                    borderTQFT m =
                        let go xl xr yl yr | xl == xr && yl == yr  = tqft $ m CM.! (yl, xl)
                                           | xl == xr              = go xl xr yl ym M.<-> go xl xr (ym + 1) yr
                                           | yl == yr              = go xl xm yl yr M.<|> go (xm + 1) xr yl yr
                                           | otherwise             = M.joinBlocks ( go xl xm yl ym      , go (xm + 1) xr yl ym
                                                                                  , go xl xm (ym + 1) yr, go (xm + 1) xr (ym + 1) yr
                                                                                  )
                                where xm = (xl + xr) `div` 2
                                      ym = (yl + yr) `div` 2

                            rows = CM.numberOfRows m
                            cols = CM.numberOfCols m
                        in if rows == 0 || cols == 0
                            then M.zero (V.sum $ V.map tqftBorderDim $ CM.toVector $ cobordismBorder1 m)
                                        (V.sum $ V.map tqftBorderDim $ CM.toVector $ cobordismBorder0 m)
                            else go 0 (cols - 1) 0 (rows - 1)

                    bettiVector = cohomologyBettiNumbers $ V.map borderTQFT border

                in map ((+ shift) &&& (bettiVector UV.!)) [0 .. dim]
