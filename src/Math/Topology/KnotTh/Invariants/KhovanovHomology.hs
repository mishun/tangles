module Math.Topology.KnotTh.Invariants.KhovanovHomology
    ( PlanarAlgebra'(..)
    , Smoothing
    , CannedCobordism(..)
    , DottedCobordism
    ) where

import Control.Monad (foldM, forM_, when)
import qualified Control.Monad.ST as ST
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV


class PlanarAlgebra' a where
    numberOfLegs  :: a -> Int
    numberOfLoops :: a -> Int
    propagator    :: a
    rotate        :: Int -> a -> a
    glue          :: Int -> (a, Int) -> (a, Int) -> (a, UV.Vector Int, UV.Vector Int)


data Smoothing = Smoothing { _loops :: {-# UNPACK #-} !Int, connections :: !(UV.Vector Int) }
    deriving (Eq, Ord)


instance PlanarAlgebra' Smoothing where
    numberOfLegs (Smoothing _ a) = UV.length a

    numberOfLoops (Smoothing ls _) = ls

    propagator = Smoothing 0 $ UV.fromList [1, 0]

    rotate _ _ = undefined

    glue !gl (Smoothing loopsA a, !posA) (Smoothing loopsB b, !posB) =
        let nA = UV.length a
            substA = UV.create $ do
                s <- UMV.replicate nA (-1)
                forM_ [0 .. nA - gl - 1] $ \ !i ->
                    UMV.write s ((posA + gl + i) `mod` nA) i
                forM_ [0 .. gl - 1] $ \ !i ->
                    UMV.write s ((posA + gl - 1 - i) `mod` nA) $! -1 - ((posB + i) `mod` nB)
                return s

            nB = UV.length b
            substB = UV.create $ do
                s <- UMV.replicate nB (-1)
                forM_ [0 .. nB - gl - 1] $ \ !i ->
                    UMV.write s ((posB + gl + i) `mod` nB) (nA - gl + i)
                forM_ [0 .. gl - 1] $ \ !i ->
                    UMV.write s ((posB + gl - i - 1) `mod` nB) $! -1 - ((posA + i) `mod` nA)
                return s

            res = UV.create $ do
                let mateA x | ys >= 0    = ys
                            | otherwise  = mateB $! -1 - ys
                        where ys = substA UV.! (a UV.! x)

                    mateB x | ys >= 0    = ys
                            | otherwise  = mateA $! -1 - ys
                        where ys = substB UV.! (b UV.! x)

                r <- UMV.new (nA - gl + nB - gl)
                forM_ [0 .. nA - gl - 1] $ \ !i ->
                    UMV.write r i $! mateA $! (posA + gl + i) `mod` nA
                forM_ [0 .. nB - gl - 1] $ \ !i ->
                    UMV.write r (nA - gl + i) $! mateB $! (posB + gl + i) `mod` nB
                return r

            loops = ST.runST $ do
                vis <- UMV.replicate nA False

                let goUpA target p | p == target  = return True
                                   | otherwise    = do
                        visited <- UMV.read vis p
                        if visited
                            then return False
                            else UMV.write vis p True >> (goDownA target $! a UV.! p)

                    goDownA target p | p' >= 0    = return False
                                     | otherwise  = UMV.write vis p True >> (goUpB target $! -1 - p')
                        where p' = substA UV.! p

                    goUpB target p = goDownB target $! b UV.! p

                    goDownB target p | p' >= 0    = return False
                                     | otherwise  = goUpA target $! -1 - p'
                        where p' = substB UV.! p

                foldM (\ !loopsCount !i -> do
                        let p = (posA + i) `mod` nA
                        visited <- UMV.read vis p
                        if visited
                            then return loopsCount
                            else do
                                UMV.write vis p True
                                isLoop <- goDownA p
                                 $! a UV.! p
                                return $! if isLoop then loopsCount + 1
                                                    else loopsCount
                    )
                    (loopsA + loopsB)
                    [0 .. gl - 1]

        in (Smoothing loops res, substA, substB)




data Films =
    Films
        { numberOfFilms :: !Int
        , sideFilm      :: !(UV.Vector Int)
        }


primitiveFilms :: UV.Vector Int -> UV.Vector Int -> Films
primitiveFilms !u !d =
    ST.runST $ do
        let n = UV.length u

        f <- UMV.replicate n (-1)
        let mark !comp !i = do
                fi <- UMV.read f i
                when (fi < 0) $ do
                    let j = u UV.! i
                    UMV.write f i comp
                    UMV.write f j comp
                    mark comp (d UV.! j)

        nf <- foldM
            (\ !comp !i -> do
                fi <- UMV.read f i
                if fi >= 0
                    then return $! comp
                    else do
                        mark comp i
                        return $! comp + 1
            )
            0
            [0 .. n - 1]

        f' <- UV.unsafeFreeze f
        return $!
            Films
                { numberOfFilms = nf
                , sideFilm      = f'
                }



class (PlanarAlgebra' c) => CannedCobordism c where
    (∘) :: c -> c -> c


data DottedCobordism a =
    DottedCobordism
        { border0 :: !Smoothing
        , border1 :: !Smoothing
        , films   :: !Films
        , dots    :: !(M.Map Dots a)
        }

data Dots =
    Dots
        { disk0Dots :: !(UV.Vector Bool)
        , disk1Dots :: !(UV.Vector Bool)
        , filmDots  :: !(UV.Vector Bool)
        }
    deriving (Eq, Ord)


instance (Eq a, Num a) => CannedCobordism (DottedCobordism a) where
    (∘) u d | border1 d /= border0 u  = error "incomposable cobordisms"
            | otherwise               =
        DottedCobordism
            { border0 = border0 d
            , border1 = border1 u
            , films   = primitiveFilms (connections $ border1 u) (connections $ border0 d)
            , dots    = M.filter (/= 0) $ M.fromListWith (+) $ do
                (ddots, dfactor) <- M.toList $ dots d
                (udots, ufactor) <- M.toList $ dots u
                --lr <- blackGlue alr blr
                return $! (undefined, ufactor * dfactor)
            }


instance (Eq a, Num a) => PlanarAlgebra' (DottedCobordism a) where
    numberOfLegs = numberOfLegs . border0

    rotate _ x = x

    glue !gl (a, !posA) (b, !posB) =
        undefined
