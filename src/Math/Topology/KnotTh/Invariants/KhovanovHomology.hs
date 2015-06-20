{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KhovanovHomology
    ( PlanarAlgebra'(..)
    , CannedCobordism(..)
    , numberOfLoops
    , DottedCobordism
    ) where

import Control.Monad (foldM, forM_, when)
import qualified Control.Monad.ST as ST
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf


class PlanarAlgebra' a where
    numberOfLegs     :: a -> Int
    planarPropagator :: a
    glue             :: Int -> (a, Int) -> (a, Int) -> (a, UV.Vector Int, UV.Vector Int)
    --rotate         :: Int -> a -> a



makeWallMap :: Int -> UV.Vector Int -> UV.Vector Int -> (Int, UV.Vector Int)
makeWallMap !n !bot !top = ST.runST $ do
    hid <- UMV.replicate n (-1)

    let mark !free !s0 !s1 !i = do
            UMV.write hid i free
            let j = s0 UV.! i
            vj <- UMV.read hid j
            when (vj < 0) $
                mark free s1 s0 j

    hn <- foldM (\ !free !i -> do
            vi <- UMV.read hid i
            if vi < 0
                then mark free bot top i >> return (free + 1)
                else return free
        ) 0 [0 .. n - 1]

    hid' <- UV.unsafeFreeze hid
    return $! (hn, hid')



class (Eq g, Ord g) => CobordismGuts g where
    capGuts      :: g
    cupGuts      :: g
    identityGuts :: Int -> Int -> g
    composeGuts  :: g -> g -> g

class (CobordismGuts g) => ModuleCobordismGuts g where
    isNormalGuts  :: g -> Bool
    normalizeGuts :: (Eq a, Num a) => g -> [(g, a)]


newtype ModuleGuts g a = MG (M.Map g a)
    deriving (Eq, Ord, Show)

singletonModuleGuts :: (ModuleCobordismGuts g, Eq a, Num a) => g -> ModuleGuts g a
singletonModuleGuts = MG . M.fromList . normalizeGuts

instance (ModuleCobordismGuts g, Eq a, Ord a, Num a) => CobordismGuts (ModuleGuts g a) where
    capGuts = singletonModuleGuts capGuts

    cupGuts = singletonModuleGuts cupGuts

    identityGuts wallHoles endHoles =
        singletonModuleGuts (identityGuts wallHoles endHoles)

    composeGuts (MG map1) (MG map0) =
        MG $ M.filter (/= 0) $ M.fromListWith (+) $ do
            (g0, factor0) <- M.toList map0
            (g1, factor1) <- M.toList map1
            (g, factorNorm) <- normalizeGuts $ composeGuts g1 g0
            return $! (g, factor0 * factor1 * factorNorm)



data DottedGuts =
    DottedGuts
        { wallSurfs  :: !(UV.Vector Int)
        , loopSurfs0 :: !(UV.Vector Int)
        , loopSurfs1 :: !(UV.Vector Int)
        , surfsData  :: !(V.Vector (Int, Int, Bool))
        }
    deriving (Eq, Ord, Show)

instance CobordismGuts DottedGuts where
    capGuts =
        DottedGuts
            { wallSurfs  = UV.empty
            , loopSurfs0 = UV.singleton 0
            , loopSurfs1 = UV.empty
            , surfsData  = V.singleton (1, 0, False)
            }

    cupGuts =
        DottedGuts
            { wallSurfs  = UV.empty
            , loopSurfs0 = UV.empty
            , loopSurfs1 = UV.singleton 0
            , surfsData  = V.singleton (1, 0, False)
            }

    identityGuts wallHoles endHoles =
        let ls = UV.generate endHoles (+ wallHoles)
        in DottedGuts
            { wallSurfs  = UV.generate wallHoles id
            , loopSurfs0 = ls
            , loopSurfs1 = ls
            , surfsData  = V.replicate wallHoles (1, 0, False)
                            V.++ V.replicate endHoles (2, 0, False)
            }

    composeGuts _ _ = error "not implemented"

instance ModuleCobordismGuts DottedGuts where
    isNormalGuts = V.all (\ (borders, handles, _) -> borders == 1 && handles == 0) . surfsData

    normalizeGuts g | isNormalGuts g  = [(g, 1)]
                    | otherwise       = error "normalize not implemented"


data CobordismWalls =
    Walls
        { legsN      :: {-# UNPACK #-} !Int
        , loops0     :: {-# UNPACK #-} !Int
        , loops1     :: {-# UNPACK #-} !Int
        , border0    :: !(UV.Vector Int)
        , border1    :: !(UV.Vector Int)
        , wallHolesN :: {-# UNPACK #-} !Int
        , wallMap    :: !(UV.Vector Int)
        }
    deriving (Eq, Show)

data Cobordism g = Cob !CobordismWalls !g
    deriving (Eq, Show)


class (PlanarAlgebra' c, PlanarAlgebra' (CobordismBorder c)) => CannedCobordism c where
    data CobordismBorder c :: *

    cobordismBorder0  :: c -> CobordismBorder c
    cobordismBorder1  :: c -> CobordismBorder c
    capCobordism      :: c
    cupCobordism      :: c
    identityCobordism :: CobordismBorder c -> c
    (∘)               :: c -> c -> c


instance (CobordismGuts g) => CannedCobordism (Cobordism g) where
    data CobordismBorder (Cobordism g) = Brd {-# UNPACK #-} !Int !(UV.Vector Int)
        deriving (Eq, Ord, Show)

    cobordismBorder0 (Cob c _) = Brd (loops0 c) (border0 c)
    cobordismBorder1 (Cob c _) = Brd (loops1 c) (border1 c)

    capCobordism =
        Cob
            Walls { legsN      = 0
                  , loops0     = 1
                  , loops1     = 0
                  , border0    = UV.empty
                  , border1    = UV.empty
                  , wallHolesN = 0
                  , wallMap    = UV.empty
                  }
            capGuts

    cupCobordism =
        Cob
            Walls { legsN      = 0
                  , loops0     = 0
                  , loops1     = 1
                  , border0    = UV.empty
                  , border1    = UV.empty
                  , wallHolesN = 0
                  , wallMap    = UV.empty
                  }
            cupGuts

    identityCobordism (Brd loops border) =
        let l = UV.length border
            (whN, wm) = makeWallMap l border border
        in Cob
            Walls { legsN      = l
                  , loops0     = loops
                  , loops1     = loops
                  , border0    = border
                  , border1    = border
                  , wallHolesN = whN
                  , wallMap    = wm
                  }
            (identityGuts whN loops)

    (Cob w1 g1) ∘ (Cob w0 g0) | legsN w0   /= legsN w1    = error $ printf "(∘): different leg numbers <%i> and <%i>" (legsN w1) (legsN w0)
                              | loops1 w0  /= loops0 w1   = error $ printf "(∘): different border loops numbers <%i> and <%i>" (loops0 w1) (loops1 w0)
                              | border1 w0 /= border0 w1  = error "(∘): different borders"
                              | otherwise                 =
        let l = legsN w0
            (whN, wm) = makeWallMap l (border0 w0) (border1 w1)

            w = Walls { legsN      = legsN w0
                      , loops0     = loops0 w0
                      , loops1     = loops1 w1
                      , border0    = border0 w0
                      , border1    = border1 w1
                      , wallHolesN = whN
                      , wallMap    = wm
                      }
        in Cob w (composeGuts g1 g0)


instance (CobordismGuts g) => PlanarAlgebra' (Cobordism g) where
    numberOfLegs (Cob w _) = legsN w

    planarPropagator = identityCobordism planarPropagator

    --glue !gl (!a, !posA) (!b, !posB) =
    --    undefined

instance (CobordismGuts g) => PlanarAlgebra' (CobordismBorder (Cobordism g)) where
    numberOfLegs (Brd _ a) = UV.length a

    planarPropagator = Brd 0 (UV.fromList [1, 0])

    glue !gl (Brd loopsA a, !posA) (Brd loopsB b, !posB) =
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

        in (Brd loops res, substA, substB)

numberOfLoops :: CobordismBorder (Cobordism g) -> Int
numberOfLoops (Brd ls _) = ls


type DottedCobordism a = Cobordism (ModuleGuts DottedGuts a)
