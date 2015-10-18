{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
module Math.Topology.KnotTh.Invariants.KhovanovHomology
    ( module Math.Topology.KnotTh.Cobordism.DottedCobordism
    , KhovanovComplex(..)
    , testComplexBorders
    , khovanovComplex
    , khovanovHomologyBetti
    ) where

import Control.Monad (guard)
import qualified Data.Array.Unboxed as A
import qualified Data.Vector as V
import Text.Printf
import Math.Topology.KnotTh.Algebra.Homology
import qualified Math.Topology.KnotTh.Cobordism.CobordismMatrix as M
import Math.Topology.KnotTh.Cobordism.DottedCobordism
import Math.Topology.KnotTh.PlanarAlgebra.Reduction
import Math.Topology.KnotTh.Tangle


data BoundedChain c = Chain !(V.Vector (M.CobordismMatrix c))
                    | Singl !(CobordismBorder c)

deriving instance (DottedCobordism c) => Show (BoundedChain c)


glueChains :: (DottedCobordism c) => Int -> (BoundedChain c, Int) -> (BoundedChain c, Int) -> BoundedChain c
glueChains gl (Singl a, posA) (Singl b, posB) = Singl $ horizontalComposition gl (a, posA) (b, posB)
glueChains gl (Singl a, posA) (Chain bc, posB) =
    let arrow = M.singleton $ identityCobordism a
    in Chain $ V.map (\ b -> horizontalComposition gl (arrow, posA) (b, posB)) bc
glueChains gl (Chain ac, posA) (Singl b, posB) =
    let arrow = M.singleton $ identityCobordism b
    in Chain $ V.map (\ a -> horizontalComposition gl (a, posA) (arrow, posB)) ac
glueChains gl (Chain a, posA) (Chain b, posB) =
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

        in M.flatten $ M.matrix cols rows $ \ row col ->
            let (pa0, pb0) = coords d col
                (pa1, pb1) = coords (d + 1) row
            in if | pb1 == pb0             -> horizontalComposition gl (a V.! pa0, posA) (identityCobordism $ layerB pb0, posB)
                  | pa1 == pa0 && even pa0 -> horizontalComposition gl (identityCobordism $ layerA pa0, posA) (b V.! pb0, posB)
                  | pa1 == pa0 && odd pa0  -> negate $ horizontalComposition gl (identityCobordism $ layerA pa0, posA) (b V.! pb0, posB)
                  | otherwise              -> zeroCobordism (cols V.! col) (rows V.! row)


simplifyChain :: (DottedCobordism c) => BoundedChain c -> BoundedChain c
simplifyChain (Singl b) = Singl b
simplifyChain (Chain borders) = Chain $ goL 0 $ goE 0 borders
    where
        test msg pre b =
            case V.findIndex (not . isZeroCobordism) $ V.zipWith (∘) (V.tail b) b of
                Nothing -> b
                Just i  -> error $ printf "border square failed (%i ∘ %i) %s on\n%s\n\n%s\n\n"
                                        (i + 1) i msg (show pre) (show b)

        goE d kh | d >= V.length kh  = kh
                 | otherwise          =
                     case M.findIndex isIsomorphism $ kh V.! d of
                         Nothing         -> goE (d + 1) kh
                         Just (row, col) ->
                            let msg = printf "elimination at d = %i pos = %s" d (show (row, col)) :: String
                            in goE d (test msg kh $ eliminateAt d row col kh)

        goL d kh | d > V.length kh  = goE 0 kh
                 | otherwise        =
                     let level | d == 0     = M.toVector $ cobordismBorder0 $ kh V.! d
                               | otherwise  = M.toVector $ cobordismBorder1 $ kh V.! (d - 1)
                     in case V.findIndex ((> 0) . numberOfLoops) level of
                            Nothing      -> goL (d + 1) kh
                            Just loopPos ->
                                let msg = printf "delooping d = %i pos = %i" d loopPos :: String
                                in goL d (test msg kh $ deloopAt d loopPos kh)

        eliminateAt d row col kh =
            let m = (kh V.! d)
                (eps, delta, gamma) = M.minor (row, col) m
            in (kh V.//) $ [(d, eps - gamma ∘ (M.singleton $ m M.! (row, col)) ∘ delta)]
                                ++ [(d - 1, M.removeRow col (kh V.! (d - 1))) | d > 0]
                                ++ [(d + 1, M.removeCol row (kh V.! (d + 1))) | d < V.length kh - 1]

        deloopAt d loopPos kh =
            let (delooped, pairs) = delooping $
                    if | d == 0    -> M.toVector (cobordismBorder0 $ kh V.! d) V.! loopPos
                       | otherwise -> M.toVector (cobordismBorder1 $ kh V.! (d - 1)) V.! loopPos

                newLines = V.length pairs

                postDeloop m =
                    let cols = M.toVector $ cobordismBorder0 m
                        rows = let v = M.toVector $ cobordismBorder1 m
                               in V.take loopPos v V.++ V.replicate newLines delooped V.++ V.drop (loopPos + 1) v
                    in M.matrix cols rows $ \ row col ->
                        if | row < loopPos             -> m M.! (row, col)
                           | row >= loopPos + newLines -> m M.! (row - newLines + 1, col)
                           | otherwise                 -> fst (pairs V.! (row - loopPos)) ∘ (m M.! (loopPos, col))

                preDeloop m =
                    let rows = M.toVector $ cobordismBorder1 m
                        cols = let v = M.toVector $ cobordismBorder0 m
                               in V.take loopPos v V.++ V.replicate newLines delooped V.++ V.drop (loopPos + 1) v
                    in M.matrix cols rows $ \ row col ->
                        if | col < loopPos             -> m M.! (row, col)
                           | col >= loopPos + newLines -> m M.! (row, col - newLines + 1)
                           | otherwise                 -> (m M.! (row, loopPos)) ∘ snd (pairs V.! (col - loopPos))

            in (kh V.//) $ [(d - 1, postDeloop $ kh V.! (d - 1)) | d > 0]
                        ++ [(d, preDeloop $ kh V.! d) | d < V.length kh]


stripChain :: (PreadditiveCobordism c) => BoundedChain c -> (Int, BoundedChain c)
stripChain (Singl x) = (0, Singl x)
stripChain (Chain chain) =
    let zeroL = (== M.emptyVector) . cobordismBorder0 . (chain V.!)
        zeroR = (== M.emptyVector) . cobordismBorder1 . (chain V.!)

        go !l !r | l > r      = let brd = M.toVector $ cobordismBorder1 $ chain V.! r
                                in if V.length brd == 1
                                    then (l, Singl $ V.head brd)
                                    else error "zero chain bad case"
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
            , complexChain = Singl $ planarPropagator n
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


overCrossingComplex, underCrossingComplex :: KhovanovComplex (DottedCobordism' Integer)
overCrossingComplex =
    KhovanovComplex
        { legsN        = 4
        , chainOffset  = 0
        , complexChain = Chain $ V.singleton $ M.singleton saddleCobordism
        }
underCrossingComplex =
    KhovanovComplex
        { legsN        = 4
        , chainOffset  = 0
        , complexChain = Chain $ V.singleton $ M.singleton saddleCobordism'
        }


khovanovComplex :: TangleDiagram -> KhovanovComplex (DottedCobordism' Integer)
khovanovComplex =
    reduceWithDefaultStrategy
        (\ v -> let d = nthOutcomingDart v 0
                in if passOver d
                    then overCrossingComplex
                    else underCrossingComplex
        )


khovanovHomologyBetti :: TangleDiagram -> [(Int, Int)]
khovanovHomologyBetti tangle =
    let w = selfWritheArray tangle
        nminus = length $ filter (< 0) $ map (w A.!) $ allVertices tangle
        kh = khovanovComplex tangle
    in case complexChain kh of
        Singl _     -> [(1, chainOffset kh)]
        Chain chain ->
            let dim = V.length chain

                borderTQFT m =
                    let rows = M.numberOfRows m
                        cols = M.numberOfCols m
                    in V.generate rows $ \ row ->
                        V.generate cols $ \ col ->
                            applyTQFT $ m M.! (row, col)

                smith = V.map (smithNormalForm . borderTQFT) chain

                kerDim d | d == dim   = M.numberOfRows $ chain V.! (d - 1)
                         | otherwise  = M.numberOfCols (chain V.! d) - V.length (smith V.! d)

                imDim 0 = 0
                imDim d = V.length $ smith V.! (d - 1)

            in do
                d <- [0 .. dim]
                let betti = kerDim d - imDim d
                guard $ betti > 0
                return $! (d + chainOffset kh - nminus, betti)
