module Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
    ( KauffmanXArg(..)
    , PlanarChordDiagram(..)
    , KauffmanXStateSum(..)
    ) where

import Data.List (foldl', intercalate)
import Data.Monoid (Monoid(..))
import qualified Data.Map as M
import Data.Array.IArray ((!), bounds, elems, listArray)
import Data.Array.MArray (newArray, newArray_, writeArray, freeze)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Control.Monad.ST (ST, runST)
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


data PlanarChordDiagram a = PlanarChordDiagram !(UArray Int Int) !a deriving (Eq, Ord)


instance Functor PlanarChordDiagram where
    fmap f (PlanarChordDiagram a x) =
        PlanarChordDiagram a (f x)


instance (Show a) => Show (PlanarChordDiagram a) where
    show (PlanarChordDiagram a x) =
        printf "(%s)%s" (show x) (show $ elems a)


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
    KauffmanXStateSum (1 + snd (bounds a)) [summand]


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
    mempty = KauffmanXStateSum 0 [PlanarChordDiagram (listArray (0, -1) []) 1]

    mappend a b =
       let c = takeAsScalar a * takeAsScalar b
       in KauffmanXStateSum 0 $ if c == 0
           then []
           else [PlanarChordDiagram (listArray (0, -1) []) c]


instance (KauffmanXArg a) => PlanarStateSum (KauffmanXStateSum a) where
    stateDegree (KauffmanXStateSum d _) = d

    loopState _ (preSum@(KauffmanXStateSum !degree _), !p) =
        let !p' = (p + 1) `mod` degree

            !subst = runSTUArray $ do
                a <- newArray (0, degree - 1) (-1) :: ST s (STUArray s Int Int)
                foldM_ (\ !k !i ->
                    if i == p' || i == p
                        then return $! k
                        else writeArray a i k >> (return $! k + 1)
                    ) 0 [0 .. degree - 1]
                return $! a

            !result = flip mapStateSum preSum $ \ (PlanarChordDiagram x k) ->
                let x' = runSTUArray $ do
                        xm <- newArray_ (0, degree - 3) :: ST s (STUArray s Int Int)
                        forM_ [0 .. degree - 1] $ \ !i ->
                            when (i /= p' && i /= p) $ do
                                let j | (x ! i) == p   = x ! p'
                                      | (x ! i) == p'  = x ! p
                                      | otherwise      = x ! i
                                writeArray xm (subst ! i) (subst ! j)
                        return $! xm

                    k' | x ! p == p'  = k * circleFactor
                       | otherwise    = k

                in singletonStateSum $ PlanarChordDiagram x' k'
        in (result, subst)

    connectStates _ (sumV@(KauffmanXStateSum !degreeV _), !p) (sumU@(KauffmanXStateSum !degreeU _), !q) =
        let !substV = runSTUArray $ do
                a <- newArray (0, degreeV - 1) (-1) :: ST s (STUArray s Int Int)
                forM_ [0 .. p - 1] $ \ !i ->
                    writeArray a i i
                forM_ [p + 1 .. degreeV - 1] $ \ !i ->
                    writeArray a i $ i + degreeU - 2
                return $! a

            !substU = runSTUArray $ do
                a <- newArray (0, degreeU - 1) (-1) :: ST s (STUArray s Int Int)
                forM_ [q + 1 .. degreeU - 1] $ \ !i ->
                    writeArray a i $ i - q - 1 + p
                forM_ [0 .. q - 1] $ \ !i ->
                    writeArray a i $ i + degreeU - q + p - 1
                return $! a

            !result = flip mapStateSum sumV $ \ (PlanarChordDiagram xa ka) ->
                flip mapStateSum sumU $ \ (PlanarChordDiagram xb kb) ->
                    let x = runSTUArray $ do
                            xm <- newArray_ (0, degreeV + degreeU - 3) :: ST s (STUArray s Int Int)
                            forM_ [0 .. degreeV - 1] $ \ !i -> when (i /= p) $
                                writeArray xm (substV ! i) $
                                    if (xa ! i) == p
                                        then substU ! (xb ! q)
                                        else substV ! (xa ! i)
                            forM_ [0 .. degreeU - 1] $ \ !i -> when (i /= q) $
                                writeArray xm (substU ! i) $
                                    if (xb ! i) == q
                                        then substV ! (xa ! p)
                                        else substU ! (xb ! i)
                            return $! xm
                    in singletonStateSum $ PlanarChordDiagram x (ka * kb)
        in (result, substV, substU)

    assemble border connections internals global = runST $ do
        let (0, l) = bounds border
        let (1, n) = bounds internals

        cd <- newArray_ (0, l) :: ST s (STUArray s Int Int)
        forM_ [0 .. l] $ \ !i -> do
            let (v, p) = border ! i
            when (v == 0) $ writeArray cd i p

        result <- newSTRef []
        let substState factor [] = do
                x <- freeze cd
                modifySTRef' result (PlanarChordDiagram x factor :)

            substState factor (v : rest) =
                forAllSummands (internals ! v) $ \ (PlanarChordDiagram x f) -> do
                    let (0, k) = bounds x
                    forM_ [0 .. k] $ \ !i -> do
                        let a = (connections ! v) ! i
                        let b = (connections ! v) ! (x ! i)
                        writeArray cd a b
                    substState (factor * f) rest

        substState (takeAsScalar global) [1 .. n]
        (concatStateSums . map singletonStateSum) `fmap` readSTRef result

    rotateState rot
        | rot == 0   = id
        | otherwise  =
            mapStateSum $ \ (PlanarChordDiagram x f) ->
                let x' = runSTUArray $ do
                        let (0, l) = bounds x
                        a <- newArray_ (0, l)
                        forM_ [0 .. l] $ \ !i ->
                            writeArray a ((i + rot) `mod` (l + 1)) (((x ! i) + rot) `mod` (l + 1))
                        return $! a
                in singletonStateSum $ PlanarChordDiagram x' f

    mirrorState =
        mapStateSum $ \ (PlanarChordDiagram x f) ->
            let x' = runSTUArray $ do
                    let (0, l) = bounds x
                    a <- newArray_ (0, l)
                    forM_ [0 .. l] $ \ !i ->
                        writeArray a ((-i) `mod` (l + 1)) ((-(x ! i)) `mod` (l + 1))
                    return $! a
            in singletonStateSum $ PlanarChordDiagram x' f


instance (KauffmanXArg a) => SkeinRelation KauffmanXStateSum a where
    skeinLPlus =
        concatStateSums $ map singletonStateSum
            [ PlanarChordDiagram (listArray (0, 3) [3, 2, 1, 0]) aFactor
            , PlanarChordDiagram (listArray (0, 3) [1, 0, 3, 2]) bFactor
            ]

    skeinLMinus =
        concatStateSums $ map singletonStateSum
            [ PlanarChordDiagram (listArray (0, 3) [3, 2, 1, 0]) bFactor
            , PlanarChordDiagram (listArray (0, 3) [1, 0, 3, 2]) aFactor
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
