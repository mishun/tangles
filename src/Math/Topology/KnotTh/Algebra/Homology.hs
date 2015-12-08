module Math.Topology.KnotTh.Algebra.Homology
    ( gradedCohomology
    ) where

import Control.Monad (forM_, liftM2, when)
import qualified Control.Monad.ST as ST
import qualified Data.Map.Strict as M
import qualified Data.Matrix as Matrix
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Math.Topology.KnotTh.Algebra


smithNormalForm :: (Integral a, Eq g) => V.Vector g -> V.Vector g -> Matrix.Matrix a -> (V.Vector (g, a), V.Vector g)
smithNormalForm colGrading rowGrading matrix = ST.runST $ do
    let rows = Matrix.nrows matrix
        cols = Matrix.ncols matrix
        idx i j = i * cols + j

    when (V.length colGrading /= cols) $ error "bad colGrading length"
    when (V.length rowGrading /= rows) $ error "bad rowGrading length"

    rg <- V.thaw rowGrading
    cg <- V.thaw colGrading
    v <- V.thaw $ Matrix.getMatrixAsVector matrix

    let get row col = do
            x <- MV.read v (idx row col)
            when (x /= 0) $ do
                g <- MV.read rg row
                g' <- MV.read cg col
                when (g /= g') $ error "non-zero element between different gradings"
            return x

        negateRow row =
            forM_ [0 .. cols - 1] $ \ !k ->
                MV.modify v negate (idx row k)

        transformRows !a00 !a01 !a10 !a11 !row0 !row1 = do
            do
                g0 <- MV.read rg row0
                g1 <- MV.read rg row1
                when (g0 /= g1) $ error "different row gradings"

            forM_ [0 .. cols - 1] $ \ !k -> do
                x0 <- MV.read v (idx row0 k)
                x1 <- MV.read v (idx row1 k)
                MV.write v (idx row0 k) $! a00 * x0 + a01 * x1
                MV.write v (idx row1 k) $! a10 * x0 + a11 * x1

        transformCols !a00 !a01 !a10 !a11 !col0 !col1 = do
            do
                g0 <- MV.read cg col0
                g1 <- MV.read cg col1
                when (g0 /= g1) $ error "different col gradings"

            forM_ [0 .. rows - 1] $ \ !k -> do
                x0 <- MV.read v (idx k col0)
                x1 <- MV.read v (idx k col1)
                MV.write v (idx k col0) $! a00 * x0 + a01 * x1
                MV.write v (idx k col1) $! a10 * x0 + a11 * x1

        swapRows rowA rowB =
            when (rowA /= rowB) $ do
                MV.swap rg rowA rowB
                forM_ [0 .. cols - 1] $ \ k ->
                    MV.swap v (idx rowA k) (idx rowB k)

        swapCols colA colB =
            when (colA /= colB) $ do
                MV.swap cg colA colB
                forM_ [0 .. rows - 1] $ \ k ->
                    MV.swap v (idx k colA) (idx k colB)

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
            if ok
                then do
                    get t t >>= clearCorner t
                    eliminate $ t + 1

                else liftM2 (,) (V.generateM t $ \ i -> liftM2 (,) (MV.read rg i) (get i i))
                                (V.generateM (cols - t) $ \ i -> MV.read cg (t + i))

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
                            when (quotient /= 0) $
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
                            when (quotient /= 0) $
                                transformCols 1 0 (-quotient) 1 t i
                            clearCol t (i + 1) pivot

                        _             -> do
                            let (g, sigma, tau) = extendedGCD pivot x
                            transformCols sigma tau (-x `div` g) (pivot `div` g) t i
                            clearCol t (i + 1) g

    eliminate 0


gradedCohomology :: (Integral a, Ord g) => V.Vector (V.Vector g) -> V.Vector (Matrix.Matrix a) -> V.Vector [(g, Int)]
gradedCohomology grad chain | V.length grad /= 1 + V.length chain  = error "size conflict"
                            | otherwise                            =
    let dim = V.length chain
        smith = V.imap (\ d -> smithNormalForm (grad V.! d) (grad V.! (d + 1))) chain
    in V.generate (V.length grad) $ \ !d ->
        let kernel | d == dim   = V.toList (grad V.! d) `zip` repeat 1
                   | otherwise  = V.toList (snd $ smith V.! d) `zip` repeat 1

            image | d == 0     = []
                  | otherwise  = map (\ (g, _) -> (g, -1)) $ V.toList (fst $ smith V.! (d - 1))

        in M.toList $ M.fromListWith (+) $ kernel ++ image
