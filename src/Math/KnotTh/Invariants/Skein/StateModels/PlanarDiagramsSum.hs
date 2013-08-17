module Math.KnotTh.Invariants.Skein.StateModels.PlanarDiagramsSum
    ( PlanarDiagramsSum
    ) where

import Data.List (foldl', intercalate)
import qualified Data.Map as M
import Data.Array.Base ((!), bounds, elems, listArray, newArray, newArray_, writeArray, freeze)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when, foldM_)
import Control.DeepSeq
import Text.Printf
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Tangle


data PlanarDiagram a = PlanarDiagram !(UArray Int Int) !a deriving (Eq, Ord)


instance Functor PlanarDiagram where
    fmap f (PlanarDiagram p x) = PlanarDiagram p $! f x


instance (NFData a) => NFData (PlanarDiagram a) where
    rnf (PlanarDiagram p x) = p `seq` rnf x


instance (Show a) => Show (PlanarDiagram a) where
    show (PlanarDiagram a x) =
        printf "(%s)%s" (show x) (show $ elems a)


data PlanarDiagramsSum a = PlanarDiagramsSum !Int ![PlanarDiagram a] deriving (Eq, Ord)


instance Functor PlanarDiagramsSum where
    fmap f (PlanarDiagramsSum order list) = PlanarDiagramsSum order $ map (fmap f) list


instance (NFData a) => NFData (PlanarDiagramsSum a) where
    rnf (PlanarDiagramsSum _ list) = rnf list


instance (Show a) => Show (PlanarDiagramsSum a) where
    show (PlanarDiagramsSum _ list) =
        case list of
            [] -> "0"
            _  -> intercalate "+" $ map show list


singletonStateSum :: PlanarDiagram a -> PlanarDiagramsSum a
singletonStateSum summand @ (PlanarDiagram a _) =
    PlanarDiagramsSum (1 + snd (bounds a)) [summand]


concatStateSums :: (Eq a, Num a) => [PlanarDiagramsSum a] -> PlanarDiagramsSum a
concatStateSums [] = error $ printf "concatStateSum: empty"
concatStateSums list @ (PlanarDiagramsSum order _ : _) =
    PlanarDiagramsSum order $ map (\ (!k, !v) -> PlanarDiagram k v) $
        filter ((/= 0) . snd) $ M.toList $
            foldl' (\ !m (PlanarDiagram !k !v) -> M.insertWith' (+) k v m) M.empty $
                concatMap (\ (PlanarDiagramsSum order' list') ->
                        if order' == order
                            then list'
                            else error $ printf "concatStateSums: order conflict with %i and %i" order order'
                    ) list


mapStateSum :: (Eq a, Num a) => (PlanarDiagram a -> PlanarDiagramsSum a) -> PlanarDiagramsSum a -> PlanarDiagramsSum a
mapStateSum _ (PlanarDiagramsSum order []) = PlanarDiagramsSum order []
mapStateSum f (PlanarDiagramsSum _ list) = concatStateSums $ map f list


forAllSummands :: (Monad m) => PlanarDiagramsSum a -> (PlanarDiagram a -> m ()) -> m ()
forAllSummands (PlanarDiagramsSum _ list) = forM_ list


instance StateModel PlanarDiagramsSum where
    complexityRank (PlanarDiagramsSum _ list) = length list

    projection (PlanarDiagramsSum _ list) = do
        PlanarDiagram cd x <- list
        return (implode (0, map ((,) 0) $ elems cd, []), x)

    initialize _ =
        concatStateSums . map (\ (skein, factor) ->
                let a = listArray (0, 3) $
                        case skein of
                            Lplus  -> error "initialize: planar diagram"
                            Lzero  -> [3, 2, 1, 0]
                            Linfty -> [1, 0, 3, 2]
                in singletonStateSum $ PlanarDiagram a factor
            )

    asConst _ (PlanarDiagramsSum _ []) = 0
    asConst _ (PlanarDiagramsSum _ [PlanarDiagram _ x]) = x
    asConst _ _ = error "takeAsConst: constant expected"

    glueHandle relation !p preSum @ (PlanarDiagramsSum !degree _) =
        let !p' = (p + 1) `mod` degree

            !subst = runSTUArray $ do
                a <- newArray (0, degree - 1) (-1) :: ST s (STUArray s Int Int)
                foldM_ (\ !k !i ->
                    if i == p' || i == p
                        then return $! k
                        else writeArray a i k >> (return $! k + 1)
                    ) 0 [0 .. degree - 1]
                return $! a

            !result = flip mapStateSum preSum $ \ (PlanarDiagram x k) ->
                let x' = runSTUArray $ do
                        xm <- newArray_ (0, degree - 3) :: ST s (STUArray s Int Int)
                        forM_ [0 .. degree - 1] $ \ !i ->
                            when (i /= p' && i /= p) $ do
                                let j | (x ! i) == p   = x ! p'
                                      | (x ! i) == p'  = x ! p
                                      | otherwise      = x ! i
                                writeArray xm (subst ! i) (subst ! j)
                        return $! xm

                    k' | x ! p == p'  = k * circleFactor relation
                       | otherwise    = k

                in singletonStateSum $ PlanarDiagram x' k'
        in (subst, result)

    connect _ (!p, sumV @ (PlanarDiagramsSum !degreeV _)) (!q, sumU @ (PlanarDiagramsSum !degreeU _)) =
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

            !result = flip mapStateSum sumV $ \ (PlanarDiagram xa ka) ->
                flip mapStateSum sumU $ \ (PlanarDiagram xb kb) ->
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
                    in singletonStateSum $ PlanarDiagram x (ka * kb)
        in (substV, substU, result)

    assemble _ border connections internals global = runST $ do
        let (0, l) = bounds border
        let (1, n) = bounds internals

        cd <- newArray_ (0, l) :: ST s (STUArray s Int Int)
        forM_ [0 .. l] $ \ !i -> do
            let (v, p) = border ! i
            when (v == 0) $ writeArray cd i p

        result <- newSTRef []
        let substState factor [] = do
                x <- freeze cd
                modifySTRef' result (PlanarDiagram x factor :)

            substState factor (v : rest) =
                forAllSummands (internals ! v) $ \ (PlanarDiagram x f) -> do
                    let (0, k) = bounds x
                    forM_ [0 .. k] $ \ !i -> do
                        let a = (connections ! v) ! i
                        let b = (connections ! v) ! (x ! i)
                        writeArray cd a b
                    substState (factor * f) rest

        substState global [1 .. n]
        (concatStateSums . map singletonStateSum) `fmap` readSTRef result

    rotate _ rot
        | rot == 0   = id
        | otherwise  =
            mapStateSum $ \ (PlanarDiagram x f) ->
                let x' = runSTUArray $ do
                        let (0, l) = bounds x
                        a <- newArray_ (0, l)
                        forM_ [0 .. l] $ \ !i ->
                            writeArray a ((i + rot) `mod` (l + 1)) (((x ! i) + rot) `mod` (l + 1))
                        return $! a
                in singletonStateSum $ PlanarDiagram x' f

    mirror _ =
        mapStateSum $ \ (PlanarDiagram x f) ->
            let x' = runSTUArray $ do
                    let (0, l) = bounds x
                    a <- newArray_ (0, l)
                    forM_ [0 .. l] $ \ !i ->
                        writeArray a ((-i) `mod` (l + 1)) ((-(x ! i)) `mod` (l + 1))
                    return $! a
            in singletonStateSum $ PlanarDiagram x' f
