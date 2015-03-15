module Math.Topology.KnotTh.Invariants.KhovanovHomology.DottedCobordism
    ( DottedCobordism
    , (∘)
    ) where

import Control.Monad (foldM, when)
import Control.Monad.ST (runST)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Math.Topology.KnotTh.Invariants.KhovanovHomology.PlanarAlgebra
import Math.Topology.KnotTh.Invariants.KhovanovHomology.PlanarSmoothing


data Films =
    Films
        { numberOfFilms :: !Int
        , sideFilm      :: !(UV.Vector Int)
        }


primitiveFilms :: UV.Vector Int -> UV.Vector Int -> Films
primitiveFilms !u !d =
    runST $ do
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



data DottedCobordism a =
    DottedCobordism
        { border0 :: !PlanarSmoothing
        , border1 :: !PlanarSmoothing
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


(∘) :: (Eq a, Num a) => DottedCobordism a -> DottedCobordism a -> DottedCobordism a
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


instance PlanarAlgebra' (DottedCobordism a) where
    numberOfLegs = numberOfLegs . border0

    rotate _ x = x

    glue !gl (a, !posA) (b, !posB) =
        undefined
