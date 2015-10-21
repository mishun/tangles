{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
module Math.Topology.KnotTh.Invariants.KhovanovHomology
    ( module Math.Topology.KnotTh.Algebra.Cobordism.DottedCobordism
    , KhovanovComplex(..)
    , testComplexBorders
    , khovanovComplex
    , khovanovHomologyBetti
    ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Text.Printf
import Math.Topology.KnotTh.Algebra.Homology
import qualified Math.Topology.KnotTh.Algebra.Cobordism.CobordismMatrix as CM
import Math.Topology.KnotTh.Algebra.Cobordism.DottedCobordism
import Math.Topology.KnotTh.Algebra.PlanarAlgebra.Reduction
import Math.Topology.KnotTh.Tangle


data BoundedChain c = Chain !(V.Vector (CM.CobordismMatrix c))
                    | Singl !(CobordismBorder (CM.CobordismMatrix c))

deriving instance (DottedCobordism c) => Show (BoundedChain c)


glueChains :: (DottedCobordism c) => Int -> (BoundedChain c, Int) -> (BoundedChain c, Int) -> BoundedChain c
glueChains gl (Singl a, posA)  (Singl b, posB)  = Singl $ horizontalComposition gl (a, posA) (b, posB)
glueChains gl (Singl a, posA)  (Chain bc, posB) = Chain $ V.map (\ b -> horizontalComposition gl (identityCobordism a, posA) (b, posB)) bc
glueChains gl (Chain ac, posA) (Singl b, posB)  = Chain $ V.map (\ a -> horizontalComposition gl (a, posA) (identityCobordism b, posB)) ac
glueChains gl (Chain a, posA)  (Chain b, posB)  =
    let spanA = V.length a
        spanB = V.length b

        width d = max 0 $ (min d spanA) - (max 0 $ d - spanB) + 1

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
                    in horizontalComposition gl (layerA pa, posA) (layerB pb, posB)

            rows = V.generate (width $ d + 1) $ \ w ->
                    let (pa, pb) = coords (d + 1) w
                    in horizontalComposition gl (layerA pa, posA) (layerB pb, posB)

        in CM.flatten $ CM.matrix cols rows $ \ row col ->
            let (pa0, pb0) = coords d col
                (pa1, pb1) = coords (d + 1) row
            in if | pb1 == pb0             -> horizontalComposition gl (a V.! pa0, posA) (identityCobordism $ layerB pb0, posB)
                  | pa1 == pa0 && even pa0 -> horizontalComposition gl (identityCobordism $ layerA pa0, posA) (b V.! pb0, posB)
                  | pa1 == pa0 && odd pa0  -> negate $ horizontalComposition gl (identityCobordism $ layerA pa0, posA) (b V.! pb0, posB)
                  | otherwise              -> zeroCobordism (cols V.! col) (rows V.! row)


simplifyChain :: (DottedCobordism c) => BoundedChain c -> BoundedChain c
simplifyChain (Singl b) =
    Singl $ CM.fromVector $ V.concatMap (\ x ->
            let (delooped, factors) = delooping x
            in V.replicate (V.length factors) delooped
        ) $ CM.toVector b
simplifyChain (Chain borders) = Chain $ goL 0 $ goE 0 borders
    where
        test msg pre b =
            case V.findIndex (not . isZeroCobordism) $ V.zipWith (∘) (V.tail b) b of
                Nothing -> b
                Just i  -> error $ printf "border square failed (%i ∘ %i) %s on\n%s\n\n%s\n\n"
                                        (i + 1) i msg (show pre) (show b)

        goE d kh | d >= V.length kh  = kh
                 | otherwise          =
                     case CM.findIndex isIsomorphism $ kh V.! d of
                         Nothing         -> goE (d + 1) kh
                         Just (row, col) ->
                            let msg = printf "elimination at d = %i pos = %s" d (show (row, col)) :: String
                            in goE d (test msg kh $ eliminateAt d row col kh)

        goL d kh | d > V.length kh  = goE 0 kh
                 | otherwise        =
                     let level | d == 0     = CM.toVector $ cobordismBorder0 $ kh V.! d
                               | otherwise  = CM.toVector $ cobordismBorder1 $ kh V.! (d - 1)
                     in case V.findIndex ((> 0) . numberOfLoops) level of
                            Nothing      -> goL (d + 1) kh
                            Just loopPos ->
                                let msg = printf "delooping d = %i pos = %i" d loopPos :: String
                                in goL d (test msg kh $ deloopAt d loopPos kh)

        eliminateAt d row col kh =
            let m = (kh V.! d)
                (eps, delta, gamma) = CM.minor (row, col) m
            in (kh V.//) $ [(d, eps - gamma ∘ (CM.singleton $ m CM.! (row, col)) ∘ delta)]
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


stripChain :: (PreadditiveCobordism c) => BoundedChain c -> (Int, BoundedChain c)
stripChain (Singl x) = (0, Singl x)
stripChain (Chain chain) =
    let zeroL = (== 0) . CM.vectorLength . cobordismBorder0 . (chain V.!)
        zeroR = (== 0) . CM.vectorLength . cobordismBorder1 . (chain V.!)

        go !l !r | l > r      = let brd | r >= 0     = cobordismBorder1 $ chain V.! r
                                        | otherwise  = cobordismBorder0 $ chain V.! l
                                in (l, Singl brd)
                 | zeroL l    = go (l + 1) r
                 | zeroR r    = go l (r - 1)
                 | otherwise  = (l, Chain $ V.force $ V.slice l (r - l + 1) chain)

    in go 0 (V.length chain - 1)


data KhovanovComplex c =
    KhovanovComplex
        { legsN        :: !Int
        , chainOffset  :: !Int
        , complexChain :: !(BoundedChain c)
        }

deriving instance (DottedCobordism c) => Show (KhovanovComplex c)

instance (DottedCobordism c) => RotationAction (KhovanovComplex c) where
    rotationOrder = legsN

    rotateByUnchecked rot kh =
        kh { complexChain =
                case complexChain kh of
                    Singl b -> Singl $ rotateBy rot b
                    Chain c -> Chain $ V.map (rotateBy rot) c
           }

instance (DottedCobordism c) => PlanarAlgebra (KhovanovComplex c) where
    planarDegree = legsN

    planarPropagator n =
        KhovanovComplex
            { legsN        = 2 * n
            , chainOffset  = 0
            , complexChain = Singl $ CM.singletonVector $ planarPropagator n
            }

    horizontalCompositionUnchecked !gl (!a, !posA) (!b, !posB) =
        let (delta, chain) = stripChain $ simplifyChain $ glueChains gl (complexChain a, posA) (complexChain b, posB)
        in KhovanovComplex
            { legsN        = legsN a + legsN b - 2 * gl
            , chainOffset  = chainOffset a + chainOffset b + delta
            , complexChain = chain
            }


testComplexBorders :: (PreadditiveCobordism c) => KhovanovComplex c -> Bool
testComplexBorders (KhovanovComplex { complexChain = Singl _ }) = True
testComplexBorders (KhovanovComplex { complexChain = Chain b }) = V.all isZeroCobordism (V.zipWith (∘) (V.tail b) b)


crossingComplex :: DiagramCrossing -> KhovanovComplex (DottedCobordism' Integer)
crossingComplex OverCrossing =
    KhovanovComplex
        { legsN        = 4
        , chainOffset  = 0
        , complexChain = Chain $ V.singleton $ CM.singleton saddleCobordism
        }
crossingComplex UnderCrossing =
    KhovanovComplex
        { legsN        = 4
        , chainOffset  = 0
        , complexChain = Chain $ V.singleton $ CM.singleton saddleCobordism'
        }


khovanovComplex :: TangleDiagram -> KhovanovComplex (DottedCobordism' Integer)
khovanovComplex = reduceWithDefaultStrategy . fmap crossingComplex


khovanovHomologyBetti :: TangleDiagram -> [(Int, Int)]
khovanovHomologyBetti tangle =
    let writhe = selfWrithe tangle
        nminus = length $ filter (< 0) $ map writhe $ allVertices tangle
        kh = khovanovComplex tangle
    in case complexChain kh of
        Singl brd   -> [(chainOffset kh - nminus, V.sum $ V.map tqftBorderDim $ CM.toVector brd)]
        Chain chain ->
            let dim = V.length chain

                (tqftDim, tqft) = prepareTQFT $ numberOfLegs tangle

                borderTQFT m =
                    let rows = tqftDim * CM.numberOfRows m
                        cols = tqftDim * CM.numberOfCols m
                    in M.matrix rows cols $ \ (!row, !col) ->
                        let (extRow, intRow) = (row - 1) `divMod` tqftDim
                            (extCol, intCol) = (col - 1) `divMod` tqftDim
                        in (tqft $ m CM.! (extRow, extCol)) M.! (intRow + 1, intCol + 1)

                bettiVector = cohomologyBettiNumbers $ V.map borderTQFT chain
            in do
                d <- [0 .. dim]
                let betti = bettiVector UV.! d
                [(d + chainOffset kh - nminus, betti) | betti > 0]
