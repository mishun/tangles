module Math.Topology.KnotTh.Algebra.Homology
    ( smithNormalForm
    , cohomologyBettiNumbers
    ) where

import Control.Monad (when)
import qualified Control.Monad.State.Strict as S
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Math.Topology.KnotTh.Algebra


smithNormalForm :: (Integral a) => V.Vector (V.Vector a) -> V.Vector a
smithNormalForm a0 =
    let rows = V.length a0
        cols = V.length $ V.head a0

        get row col = S.gets ((V.! col) . (V.! row))

        negateRow r = do
            m <- S.get
            S.put $ m V.// [(r, V.map negate $ m V.! r)]

        transformRows !a00 !a01 !a10 !a11 !r0 !r1 = do
            m <- S.get
            let row0 = V.zipWith (\ x0 x1 -> a00 * x0 + a01 * x1) (m V.! r0) (m V.! r1)
                row1 = V.zipWith (\ x0 x1 -> a10 * x0 + a11 * x1) (m V.! r0) (m V.! r1)
            S.put $! m V.// [(r0, row0), (r1, row1)]

        transformCols !a00 !a01 !a10 !a11 !c0 !c1 =
            S.modify $ V.map $ \ row ->
                let x0 = row V.! c0
                    x1 = row V.! c1
                in row V.// [(c0, a00 * x0 + a01 * x1), (c1, a10 * x0 + a11 * x1)]

        swapRows a b =
            when (a /= b) $ do
                !m <- S.get
                S.put $! m V.// [(a, m V.! b), (b, m V.! a)]

        swapCols a b =
            when (a /= b) $
                S.modify $ V.map $ \ row ->
                    row V.// [(a, row V.! b), (b, row V.! a)]

        findPivot !t !row !col | col >= cols  = return False
                               | row >= rows  = findPivot t t (col + 1)
                               | otherwise    = do
            pivot <- get row col
            if pivot == 0
                then findPivot t (row + 1) col
                else do
                    swapRows t row
                    swapCols t col
                    when (pivot < 0) $ negateRow t
                    return True

        eliminate !t = do
            ok <- findPivot t t t
            if not ok
                then V.generateM t $ \ !i -> get i i
                else do
                    get t t >>= clearCorner t
                    eliminate $ t + 1

        clearCorner !t !pivot = do
            pivot' <- clearRow t (t + 1) pivot >>= clearCol t (t + 1)
            when (pivot' /= pivot) $
                clearCorner t pivot'

        clearRow !t !i !pivot =
            if i >= rows
                then return pivot
                else do
                    x <- get i t
                    case divMod x pivot of
                        (quotient, 0) -> do
                            transformRows 1 0 (-quotient) 1 t i
                            clearRow t (i + 1) pivot

                        _             -> do
                            let (g, sigma, tau) = extendedGCD pivot x
                            transformRows sigma tau (-x `div` g) (pivot `div` g) t i
                            clearRow t (i + 1) g

        clearCol !t !i !pivot =
            if i >= cols
                then return pivot
                else do
                    x <- get t i
                    case divMod x pivot of
                        (quotient, 0) -> do
                            transformCols 1 0 (-quotient) 1 t i
                            clearCol t (i + 1) pivot

                        _             -> do
                            let (g, sigma, tau) = extendedGCD pivot x
                            transformCols sigma tau (-x `div` g) (pivot `div` g) t i
                            clearCol t (i + 1) g

    in if | rows == 0 -> V.empty
          | cols == 0 -> V.empty
          | otherwise -> S.evalState (eliminate 0) a0


cohomologyBettiNumbers :: (Integral a) => V.Vector (M.Matrix a) -> UV.Vector Int
cohomologyBettiNumbers chain =
    let dim = V.length chain
    in if dim == 0
        then error "cohomologyBettiNumbers: expected non-empty chain"
        else let smith = V.map (\ m -> smithNormalForm $ V.generate (M.nrows m) (\ row -> M.getRow (row + 1) m)) chain
             in UV.generate (dim + 1) $ \ d ->
                    let kerDim  | d == dim   = M.nrows $ chain V.! (d - 1)
                                | otherwise  = M.ncols (chain V.! d) - V.length (smith V.! d)

                        imDim | d == 0     = 0
                              | otherwise  = V.length $ smith V.! (d - 1)
                    in kerDim - imDim
