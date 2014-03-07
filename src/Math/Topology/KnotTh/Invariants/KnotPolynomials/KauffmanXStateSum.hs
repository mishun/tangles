module Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
    ( KauffmanXArg(..)
    , PlanarChordDiagram(..)
    , KauffmanXStateSum(..)
    ) where

import Data.List (foldl', intercalate)
import Data.Monoid (Monoid(..))
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Array.IArray ((!), bounds)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Control.Monad.ST (runST)
import Control.Monad (forM_, when, foldM_)
import Text.Printf
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.Util.Poly


class (Eq a, Ord a, Num a) => KauffmanXArg a where
    aFactor, bFactor, circleFactor :: a
    swapFactors                    :: a -> a

    circleFactor = -(aFactor * aFactor + bFactor * bFactor)


instance KauffmanXArg Poly where
    aFactor = monomial 1 "a" 1
    bFactor = monomial 1 "a" (-1)
    swapFactors = invert "a"


data PlanarChordDiagram a = PlanarChordDiagram !(UV.Vector Int) !a deriving (Eq, Ord)


instance Functor PlanarChordDiagram where
    fmap f (PlanarChordDiagram a x) =
        PlanarChordDiagram a (f x)


instance (Show a) => Show (PlanarChordDiagram a) where
    show (PlanarChordDiagram a x) =
        printf "(%s)%s" (show x) (show $ UV.toList a)


data KauffmanXStateSum a = KauffmanXStateSum !Int ![PlanarChordDiagram a] deriving (Eq, Ord)


instance Functor KauffmanXStateSum where
    fmap f (KauffmanXStateSum n l) =
        KauffmanXStateSum n (map (fmap f) l)

instance (Show a) => Show (KauffmanXStateSum a) where
    show (KauffmanXStateSum _ list) =
        case list of
            [] -> "0"
            _  -> intercalate "+" $ map show list


singletonStateSum :: PlanarChordDiagram a -> KauffmanXStateSum a
singletonStateSum summand @ (PlanarChordDiagram a _) =
    KauffmanXStateSum (UV.length a) [summand]


concatStateSums :: (Eq a, Num a) => [KauffmanXStateSum a] -> KauffmanXStateSum a
concatStateSums [] = error $ printf "concatStateSum: empty"
concatStateSums list @ (KauffmanXStateSum order _ : _) =
    KauffmanXStateSum order $ map (\ (!k, !v) -> PlanarChordDiagram k v) $
        filter ((/= 0) . snd) $ M.toList $
            foldl' (\ !m (PlanarChordDiagram !k !v) -> M.insertWith' (+) k v m) M.empty $
                concatMap (\ (KauffmanXStateSum order' list') ->
                        if order' == order
                            then list'
                            else error $ printf "concatStateSums: order conflict with %i and %i" order order'
                    ) list


mapStateSum :: (Eq a, Num a) => (PlanarChordDiagram a -> KauffmanXStateSum a) -> KauffmanXStateSum a -> KauffmanXStateSum a
mapStateSum _ (KauffmanXStateSum order []) = KauffmanXStateSum order []
mapStateSum f (KauffmanXStateSum _ list) = concatStateSums $ map f list


forAllSummands :: (Monad m) => KauffmanXStateSum a -> (PlanarChordDiagram a -> m ()) -> m ()
forAllSummands (KauffmanXStateSum _ list) = forM_ list


instance (KauffmanXArg a) => Monoid (KauffmanXStateSum a) where
    mempty = KauffmanXStateSum 0 [PlanarChordDiagram UV.empty 1]

    mappend a b =
       let c = takeAsScalar a * takeAsScalar b
       in KauffmanXStateSum 0 $ if c == 0
           then []
           else [PlanarChordDiagram UV.empty c]


instance (KauffmanXArg a) => PlanarStateSum (KauffmanXStateSum a) where
    stateDegree (KauffmanXStateSum d _) = d

    loopState _ (preSum@(KauffmanXStateSum !degree _), !p) =
        let !p' = (p + 1) `mod` degree

            !subst = UV.create $ do
                a <- UMV.replicate degree (-1)
                foldM_ (\ !k !i ->
                    if i == p' || i == p
                        then return $! k
                        else UMV.write a i k >> (return $! k + 1)
                    ) 0 [0 .. degree - 1]
                return a

            !result = flip mapStateSum preSum $ \ (PlanarChordDiagram x k) ->
                let x' = UV.create $ do
                        xm <- UMV.new (degree - 2)
                        forM_ [0 .. degree - 1] $ \ !i ->
                            when (i /= p' && i /= p) $ do
                                let j | (x UV.! i) == p   = x UV.! p'
                                      | (x UV.! i) == p'  = x UV.! p
                                      | otherwise         = x UV.! i
                                UMV.write xm (subst UV.! i) (subst UV.! j)
                        return xm

                    k' | x UV.! p == p'  = k * circleFactor
                       | otherwise       = k

                in singletonStateSum $ PlanarChordDiagram x' k'
        in (result, subst)

    connectStates _ (sumV@(KauffmanXStateSum !degreeV _), !p) (sumU@(KauffmanXStateSum !degreeU _), !q) =
        let !substV = UV.create $ do
                a <- UMV.replicate degreeV (-1)
                forM_ [0 .. p - 1] $ \ !i ->
                    UMV.write a i i
                forM_ [p + 1 .. degreeV - 1] $ \ !i ->
                    UMV.write a i $ i + degreeU - 2
                return a

            !substU = UV.create $ do
                a <- UMV.replicate degreeU (-1)
                forM_ [q + 1 .. degreeU - 1] $ \ !i ->
                    UMV.write a i $ i - q - 1 + p
                forM_ [0 .. q - 1] $ \ !i ->
                    UMV.write a i $ i + degreeU - q + p - 1
                return a

            !result = flip mapStateSum sumV $ \ (PlanarChordDiagram xa ka) ->
                flip mapStateSum sumU $ \ (PlanarChordDiagram xb kb) ->
                    let x = UV.create $ do
                            xm <- UMV.new (degreeV + degreeU - 2)
                            forM_ [0 .. degreeV - 1] $ \ !i -> when (i /= p) $
                                UMV.write xm (substV UV.! i) $
                                    if (xa UV.! i) == p
                                        then substU UV.! (xb UV.! q)
                                        else substV UV.! (xa UV.! i)
                            forM_ [0 .. degreeU - 1] $ \ !i -> when (i /= q) $
                                UMV.write xm (substU UV.! i) $
                                    if (xb UV.! i) == q
                                        then substV UV.! (xa UV.! p)
                                        else substU UV.! (xb UV.! i)
                            return xm
                    in singletonStateSum $ PlanarChordDiagram x (ka * kb)
        in (result, substV, substU)

    assemble border connections internals global = runST $ do
        let l = V.length border
        let (1, n) = bounds internals

        cd <- UMV.new l
        forM_ [0 .. l - 1] $ \ !i -> do
            let (v, p) = border V.! i
            when (v == 0) $ UMV.write cd i p

        result <- newSTRef []
        let substState factor [] = do
                x <- UV.freeze cd
                modifySTRef' result (PlanarChordDiagram x factor :)

            substState factor (v : rest) =
                forAllSummands (internals ! v) $ \ (PlanarChordDiagram x f) -> do
                    let k = UV.length x
                    forM_ [0 .. k - 1] $ \ !i -> do
                        let a = (connections ! v) ! i
                        let b = (connections ! v) ! (x UV.! i)
                        UMV.write cd a b
                    substState (factor * f) rest

        substState (takeAsScalar global) [1 .. n]
        (concatStateSums . map singletonStateSum) `fmap` readSTRef result

    rotateState rot
        | rot == 0   = id
        | otherwise  =
            mapStateSum $ \ (PlanarChordDiagram x f) ->
                let x' = UV.create $ do
                        let l = UV.length x
                        a <- UMV.new l
                        forM_ [0 .. l - 1] $ \ !i ->
                            UMV.write a ((i + rot) `mod` l) (((x UV.! i) + rot) `mod` l)
                        return a
                in singletonStateSum $ PlanarChordDiagram x' f

    mirrorState =
        mapStateSum $ \ (PlanarChordDiagram x f) ->
            let x' = UV.create $ do
                    let l = UV.length x
                    a <- UMV.new l
                    forM_ [0 .. l - 1] $ \ !i ->
                        UMV.write a ((-i) `mod` l) ((-(x UV.! i)) `mod` l)
                    return a
            in singletonStateSum $ PlanarChordDiagram x' f


instance (KauffmanXArg a) => SkeinRelation KauffmanXStateSum a where
    skeinLPlus =
        concatStateSums $ map singletonStateSum
            [ PlanarChordDiagram (UV.fromList [3, 2, 1, 0]) aFactor
            , PlanarChordDiagram (UV.fromList [1, 0, 3, 2]) bFactor
            ]

    skeinLMinus =
        concatStateSums $ map singletonStateSum
            [ PlanarChordDiagram (UV.fromList [3, 2, 1, 0]) bFactor
            , PlanarChordDiagram (UV.fromList [1, 0, 3, 2]) aFactor
            ]

    finalNormalization tangle =
        let factor =
                let writheFactor =
                        let w = selfWrithe tangle
                        in (if w <= 0 then -aFactor else -bFactor) ^ abs (3 * w)
                    loopsFactor = circleFactor ^ numberOfFreeLoops tangle
                in writheFactor * loopsFactor
        in fmap (* factor)

    invertCrossingsAction = fmap swapFactors

    takeAsScalar s =
        case s of
            KauffmanXStateSum 0 []                       -> 0
            KauffmanXStateSum 0 [PlanarChordDiagram _ x] -> x
            _                                            -> undefined
