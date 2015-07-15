{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Cobordism.DottedCobordism
    ( module Math.Topology.KnotTh.Cobordism
    , module Math.Topology.KnotTh.PlanarAlgebra
    , DottedCobordism
    ) where

import Control.Monad (foldM, forM_, liftM2, when)
import Control.Monad.IfElse (unlessM, whenM)
import qualified Control.Monad.ST as ST
import qualified Data.Map.Strict as M
import Data.Ratio (Ratio)
import qualified Data.STRef as STRef
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf
import Math.Topology.KnotTh.ChordDiagram
import Math.Topology.KnotTh.Cobordism
import Math.Topology.KnotTh.PlanarAlgebra


data CobordismHeader =
    CobordismHeader
        { legsN      :: {-# UNPACK #-} !Int
        , loops0     :: {-# UNPACK #-} !Int
        , loops1     :: {-# UNPACK #-} !Int
        , arcs0      :: !(UV.Vector Int)
        , arcs1      :: !(UV.Vector Int)
        , wallHolesN :: {-# UNPACK #-} !Int
        , wallMap    :: !(UV.Vector Int)
        }
    deriving (Eq, Show)

emptyHeader :: Int -> Int -> CobordismHeader
emptyHeader l0 l1 =
    CobordismHeader
        { legsN      = 0
        , loops0     = l0
        , loops1     = l1
        , arcs0      = UV.empty
        , arcs1      = UV.empty
        , wallHolesN = 0
        , wallMap    = UV.empty
        }

{-# INLINE makeHeader #-}
makeHeader :: Int -> (UV.Vector Int, Int) -> (UV.Vector Int, Int) -> CobordismHeader
makeHeader !legs (!bot, !botLoops) (!top, !topLoops) =
    ST.runST $ do
        hid <- UMV.replicate legs (-1)

        let mark !color !s0 !s1 !i = do
                UMV.write hid i color
                let j = s0 UV.! i
                whenM ((< 0) `fmap` UMV.read hid j) $
                    mark color s1 s0 j

        hn <- foldM (\ !freeColor !i -> do
                vi <- UMV.read hid i
                if vi < 0
                    then mark freeColor bot top i >> (return $! freeColor + 1)
                    else return freeColor
            ) 0 [0 .. legs - 1]

        hid' <- UV.unsafeFreeze hid
        return $!
            CobordismHeader
                { legsN      = legs
                , loops0     = botLoops
                , loops1     = topLoops
                , arcs0      = bot
                , arcs1      = top
                , wallHolesN = hn
                , wallMap    = hid'
                }


class (Eq g, Ord g) => CobordismGuts g where
    emptyGuts      :: g
    surfGuts       :: Int -> g
    capGuts        :: Int -> g
    swapGuts       :: g
    pantsGuts      :: g
    saddleGuts     :: g
    identityGuts   :: Int -> Int -> g
    flipGuts       :: g -> g
    rotateGuts     :: UV.Vector Int -> g -> g
    verComposeGuts :: CobordismHeader -> (CobordismHeader, g) -> (CobordismHeader, g) -> g
    horComposeGuts :: (CobordismHeader, Int, UV.Vector Int, UV.Vector Int) -> (g, CobordismHeader, Int) -> (g, CobordismHeader, Int) -> g

class (CobordismGuts g) => ModuleCobordismGuts g where
    normalizeGuts :: (Integral a) => g -> [(g, Ratio a)]


newtype ModuleGuts g a = MG (M.Map g (Ratio a))
    deriving (Eq, Ord, Show)

singletonModuleGuts :: (ModuleCobordismGuts g, Integral a) => g -> ModuleGuts g a
singletonModuleGuts = MG . M.fromList . normalizeGuts

instance (ModuleCobordismGuts g, Integral a) => CobordismGuts (ModuleGuts g a) where
    emptyGuts = MG M.empty

    surfGuts   = singletonModuleGuts . surfGuts
    capGuts    = singletonModuleGuts . capGuts
    swapGuts   = singletonModuleGuts swapGuts
    pantsGuts  = singletonModuleGuts pantsGuts
    saddleGuts = singletonModuleGuts saddleGuts

    identityGuts wallHoles endHoles =
        singletonModuleGuts (identityGuts wallHoles endHoles)

    flipGuts (MG m) =
        MG $ M.filter (/= 0) $ M.fromListWith (+) $ do
            (g, factor) <- M.toList m
            (g', factorNorm) <- normalizeGuts $ flipGuts g
            return $! (g', factor * factorNorm)

    rotateGuts rot (MG m) =
        MG $ M.filter (/= 0) $ M.fromListWith (+) $ do
            (g, factor) <- M.toList m
            (g', factorNorm) <- normalizeGuts $ rotateGuts rot g
            return $! (g', factor * factorNorm)

    verComposeGuts h (h1, MG map1) (h0, MG map0) =
        MG $ M.filter (/= 0) $ M.fromListWith (+) $ do
            (g0, factor0) <- M.toList map0
            (g1, factor1) <- M.toList map1
            (g, factorNorm) <- normalizeGuts $ verComposeGuts h (h1, g1) (h0, g0)
            return $! (g, factor0 * factor1 * factorNorm)

    horComposeGuts tmp (MG mapA, hA, posA) (MG mapB, hB, posB) =
        MG $ M.filter (/= 0) $ M.fromListWith (+) $ do
            (gA, factorA) <- M.toList mapA
            (gB, factorB) <- M.toList mapB
            (g, factorNorm) <- normalizeGuts $ horComposeGuts tmp (gA, hA, posA) (gB, hB, posB)
            return $! (g, factorA * factorB * factorNorm)


data DottedGuts =
    DottedGuts
        { wallSurfs    :: !(UV.Vector Int)
        , loopSurfs0   :: !(UV.Vector Int)
        , loopSurfs1   :: !(UV.Vector Int)
        , surfHolesN   :: !(UV.Vector Int)
        , surfHandlesN :: !(UV.Vector Int)
        }
    deriving (Eq, Ord, Show)

instance CobordismGuts DottedGuts where
    emptyGuts =
        DottedGuts
            { wallSurfs    = UV.empty
            , loopSurfs0   = UV.empty
            , loopSurfs1   = UV.empty
            , surfHolesN   = UV.empty
            , surfHandlesN = UV.empty
            }

    surfGuts genus =
        DottedGuts
            { wallSurfs    = UV.empty
            , loopSurfs0   = UV.empty
            , loopSurfs1   = UV.empty
            , surfHolesN   = UV.singleton 0
            , surfHandlesN = UV.singleton genus
            }

    capGuts genus =
        DottedGuts
            { wallSurfs    = UV.empty
            , loopSurfs0   = UV.singleton 0
            , loopSurfs1   = UV.empty
            , surfHolesN   = UV.singleton 1
            , surfHandlesN = UV.singleton genus
            }

    swapGuts =
        DottedGuts
            { wallSurfs    = UV.empty
            , loopSurfs0   = UV.fromList [0, 1]
            , loopSurfs1   = UV.fromList [1, 0]
            , surfHolesN   = UV.replicate 2 2
            , surfHandlesN = UV.replicate 2 0
            }

    pantsGuts =
        DottedGuts
            { wallSurfs    = UV.empty
            , loopSurfs0   = UV.replicate 2 0
            , loopSurfs1   = UV.singleton 0
            , surfHolesN   = UV.singleton 3
            , surfHandlesN = UV.singleton 0
            }

    saddleGuts =
        DottedGuts
            { wallSurfs    = UV.singleton 0
            , loopSurfs0   = UV.empty
            , loopSurfs1   = UV.empty
            , surfHolesN   = UV.singleton 1
            , surfHandlesN = UV.singleton 0
            }

    identityGuts wallHoles endHoles =
        let ls = UV.enumFromN wallHoles endHoles
        in DottedGuts
            { wallSurfs    = UV.enumFromN 0 wallHoles
            , loopSurfs0   = ls
            , loopSurfs1   = ls
            , surfHolesN   = UV.replicate wallHoles 1 UV.++ UV.replicate endHoles 2
            , surfHandlesN = UV.replicate (wallHoles + endHoles) 0
            }

    flipGuts g =
        g { loopSurfs0 = loopSurfs1 g
          , loopSurfs1 = loopSurfs0 g
          }

    rotateGuts perm g =
        g { wallSurfs = UV.backpermute (wallSurfs g) perm }

    verComposeGuts h (h1, g1) (h0, g0) =
        ST.runST $ do
            let legs = legsN h
                segmsToGlue = UV.generate legs $ \ leg ->
                    (wallSurfs g0 UV.! (wallMap h0 UV.! leg), wallSurfs g1 UV.! (wallMap h1 UV.! leg))
                loopsToGlue = UV.zip (loopSurfs1 g0) (loopSurfs0 g1)

            (newS0, newS1, surfN) <- do
                newS0 <- UMV.replicate (UV.length $ surfHolesN g0) (-1)
                newS1 <- UMV.replicate (UV.length $ surfHolesN g1) (-1)

                let mark0 !color !surf = do
                        whenM ((< 0) `fmap` UMV.read newS0 surf) $ do
                            UMV.write newS0 surf color
                            UV.forM_ segmsToGlue $ \ (s0, s1) -> when (surf == s0) $ mark1 color s1
                            UV.forM_ loopsToGlue $ \ (s0, s1) -> when (surf == s0) $ mark1 color s1

                    mark1 !color !surf =
                        whenM ((< 0) `fmap` UMV.read newS1 surf) $ do
                            UMV.write newS1 surf color
                            UV.forM_ segmsToGlue $ \ (s0, s1) -> when (surf == s1) $ mark0 color s0
                            UV.forM_ loopsToGlue $ \ (s0, s1) -> when (surf == s1) $ mark0 color s0

                freeColor <- STRef.newSTRef 0
                let tryMark0 !s =
                        whenM ((< 0) `fmap` UMV.read newS0 s) $ do
                            color <- STRef.readSTRef freeColor
                            STRef.writeSTRef freeColor $! color + 1
                            mark0 color s

                    tryMark1 !s =
                        whenM ((< 0) `fmap` UMV.read newS1 s) $ do
                            color <- STRef.readSTRef freeColor
                            STRef.writeSTRef freeColor $! color + 1
                            mark1 color s

                -- the order is important!
                -- wall adjacent surfaces:
                UV.forM_ (wallMap h0) $ tryMark0 . (wallSurfs g0 UV.!)
                -- bottom loops:
                UV.forM_ (loopSurfs0 g0) tryMark0
                -- top loops:
                UV.forM_ (loopSurfs1 g1) tryMark1
                -- closed surfaces:
                UV.forM_ (loopSurfs1 g0) tryMark0
                forM_ [0 .. UV.length (surfHolesN g0) - 1] tryMark0
                forM_ [0 .. UV.length (surfHolesN g1) - 1] tryMark1

                newS0' <- UV.unsafeFreeze newS0
                newS1' <- UV.unsafeFreeze newS1
                surfN <- STRef.readSTRef freeColor
                return $! (newS0', newS1', surfN)

            let wallS = UV.create $ do
                    ws <- UMV.new (wallHolesN h)
                    forM_ [0 .. legs - 1] $ \ !leg ->
                        UMV.write ws (wallMap h UV.! leg) $ (newS0 UV.!) $ wallMap h0 UV.! leg
                    return ws

                loopS0 = UV.map (newS0 UV.!) $ loopSurfs0 g0
                loopS1 = UV.map (newS1 UV.!) $ loopSurfs1 g1

            return $!
                DottedGuts
                    { wallSurfs    = wallS
                    , loopSurfs0   = loopS0
                    , loopSurfs1   = loopS1
                    , surfHandlesN = UV.create $ do
                        -- TODO: process new handles formation
                        handles <- UMV.replicate surfN 0
                        forM_ [0 .. UV.length (surfHandlesN g0) - 1] $ \ s ->
                            let s' = newS0 UV.! s
                                dh = surfHandlesN g0 UV.! s
                            in UMV.read handles s' >>= (UMV.write handles s' . (+ dh))
                        forM_ [0 .. UV.length (surfHandlesN g1) - 1] $ \ s ->
                            let s' = newS1 UV.! s
                                dh = surfHandlesN g1 UV.! s
                            in UMV.read handles s' >>= UMV.write handles s' . (+ dh)
                        return handles
                    , surfHolesN   = UV.create $ do
                        holes <- UMV.replicate surfN 0
                        UV.forM_ (wallS UV.++ loopS0 UV.++ loopS1) $ \ s ->
                            UMV.read holes s >>= UMV.write holes s . (+ 1)
                        return holes
                    }

    horComposeGuts (h, !gl, !exLpReps0, !exLpReps1) (!gA, !hA, !posA) (!gB, !hB, !posB) =
        ST.runST $ do
            let legsA = legsN hA
                legsB = legsN hB

                segmsToGlue = UV.generate gl $ \ i ->
                    let legA = (posA + i) `mod` legsA
                        legB = (posB + gl - 1 - i) `mod` legsB
                    in (wallSurfs gA UV.! (wallMap hA UV.! legA), wallSurfs gB UV.! (wallMap hB UV.! legB))

            (newSA, newSB, surfN) <- do
                newSA <- UMV.replicate (UV.length $ surfHolesN gA) (-1)
                newSB <- UMV.replicate (UV.length $ surfHolesN gB) (-1)

                let markA !color !surf = do
                        whenM ((< 0) `fmap` UMV.read newSA surf) $ do
                            UMV.write newSA surf color
                            UV.forM_ segmsToGlue $ \ (sA, sB) -> when (surf == sA) $ markB color sB

                    markB !color !surf =
                        whenM ((< 0) `fmap` UMV.read newSB surf) $ do
                            UMV.write newSB surf color
                            UV.forM_ segmsToGlue $ \ (sA, sB) -> when (surf == sB) $ markA color sA

                freeColor <- STRef.newSTRef 0
                let tryMarkA !s =
                        whenM ((< 0) `fmap` UMV.read newSA s) $ do
                            color <- STRef.readSTRef freeColor
                            STRef.writeSTRef freeColor $! color + 1
                            markA color s

                    tryMarkB !s =
                        whenM ((< 0) `fmap` UMV.read newSB s) $ do
                            color <- STRef.readSTRef freeColor
                            STRef.writeSTRef freeColor $! color + 1
                            markB color s

                -- the order is important!
                -- wall adjacent surfaces:
                forM_ [0 .. legsA - gl - 1] $ \ !i -> tryMarkA $ wallMap hA UV.! ((posA + gl + i) `mod` legsA)
                forM_ [0 .. legsB - gl - 1] $ \ !i -> tryMarkB $ wallMap hB UV.! ((posB + gl + i) `mod` legsB)
                -- bottom loops:
                forM_ [0 .. gl - 1] $ \ !i -> tryMarkA $ wallMap hA UV.! ((posA + i) `mod` legsA)
                UV.forM_ (loopSurfs0 gA) tryMarkA
                UV.forM_ (loopSurfs0 gB) tryMarkB
                -- top loops:
                UV.forM_ (loopSurfs1 gA) tryMarkA
                UV.forM_ (loopSurfs1 gB) tryMarkB
                -- closed surfaces:
                forM_ [0 .. UV.length (surfHolesN gA) - 1] tryMarkA
                forM_ [0 .. UV.length (surfHolesN gB) - 1] tryMarkB

                newSA' <- UV.unsafeFreeze newSA
                newSB' <- UV.unsafeFreeze newSB
                surfN <- STRef.readSTRef freeColor
                return $! (newSA', newSB', surfN)

            let wallS = UV.create $ do
                    ws <- UMV.new (wallHolesN h)
                    forM_ [0 .. legsA - gl - 1] $ \ !i ->
                        let leg = i
                            legA = (posA + gl + i) `mod` legsA
                        in UMV.write ws (wallMap h UV.! leg) $ (newSA UV.!) $ wallMap hA UV.! legA
                    forM_ [0 .. legsB - gl - 1] $ \ !i ->
                        let leg = legsA - gl + i
                            legB = (posB + gl + i) `mod` legsB
                        in UMV.write ws (wallMap h UV.! leg) $ (newSB UV.!) $ wallMap hB UV.! legB
                    return ws

                loopS0 =
                    UV.concat [ UV.map (\ !i -> (newSA UV.!) $ (wallMap hA UV.!) $ (posA + i) `mod` legsA) exLpReps0
                              , UV.map (newSA UV.!) $ loopSurfs0 gA
                              , UV.map (newSB UV.!) $ loopSurfs0 gB
                              ]

                loopS1 =
                    UV.concat [ UV.map (\ !i -> (newSA UV.!) $ (wallMap hA UV.!) $ (posA + i) `mod` legsA) exLpReps1
                              , UV.map (newSA UV.!) $ loopSurfs1 gA
                              , UV.map (newSB UV.!) $ loopSurfs1 gB
                              ]

            return $!
                DottedGuts
                    { wallSurfs    = wallS
                    , loopSurfs0   = loopS0
                    , loopSurfs1   = loopS1
                    , surfHandlesN = UV.create $ do
                        handles <- UMV.replicate surfN 0
                        forM_ [0 .. UV.length (surfHandlesN gA) - 1] $ \ s ->
                            let s' = newSA UV.! s
                                dh = surfHandlesN gA UV.! s
                            in UMV.read handles s' >>= UMV.write handles s' . (+ dh)
                        forM_ [0 .. UV.length (surfHandlesN gB) - 1] $ \ s ->
                            let s' = newSB UV.! s
                                dh = surfHandlesN gB UV.! s
                            in UMV.read handles s' >>= UMV.write handles s' . (+ dh)
                        return handles
                    , surfHolesN   = UV.create $ do
                        holes <- UMV.replicate surfN 0
                        UV.forM_ (wallS UV.++ loopS0 UV.++ loopS1) $ \ s ->
                            UMV.read holes s >>= UMV.write holes s . (+ 1)
                        return holes
                    }

instance ModuleCobordismGuts DottedGuts where
    normalizeGuts =
        let removeClosed !factor g =
                let (holes, handles) = UV.unzip $ UV.filter ((> 0) . fst) $ UV.zip (surfHolesN g) (surfHandlesN g)
                    removedN = UV.length (surfHolesN g) - UV.length holes
                in cutNecks (factor * 2 ^ removedN) $ g { surfHolesN = holes, surfHandlesN = handles }

            cutNecks !factor g =
                case UV.findIndex (> 1) (surfHolesN g) of
                    Nothing   -> [(renumerateSurfaces g, factor)]
                    Just surf -> do
                        let genus = surfHandlesN g UV.! surf
                            dst = UV.length $ surfHandlesN g
                            rew | Just i <- UV.findIndex (== surf) (wallSurfs g)   = g { wallSurfs  = wallSurfs g  UV.// [(i, dst)] }
                                | Just i <- UV.findIndex (== surf) (loopSurfs0 g)  = g { loopSurfs0 = loopSurfs0 g UV.// [(i, dst)] }
                                | Just i <- UV.findIndex (== surf) (loopSurfs1 g)  = g { loopSurfs1 = loopSurfs1 g UV.// [(i, dst)] }
                                | otherwise                                        = g
                        handles <- UV.snoc (surfHandlesN g) 1 : [ UV.snoc (surfHandlesN g) 0 UV.// [(surf, 1)] | genus == 0]
                        cutNecks (factor / 2) $
                            rew { surfHolesN   = UV.snoc (surfHolesN g) 1 UV.// [(surf, (surfHolesN g UV.! surf) - 1)]
                                , surfHandlesN = handles
                                }

            renumerateSurfaces g =
                ST.runST $ do
                    newIndex <- UMV.replicate (UV.length $ surfHolesN g) (-1)
                    freeIndex <- STRef.newSTRef 0

                    let faceId !i = do
                            tmp <- UMV.read newIndex i
                            if tmp >= 0 then return tmp
                                        else do
                                j <- STRef.readSTRef freeIndex
                                STRef.writeSTRef freeIndex $! j + 1
                                UMV.write newIndex i j
                                return j

                    wallS <- UV.mapM faceId $ wallSurfs g
                    loopS0 <- UV.mapM faceId $ loopSurfs0 g
                    loopS1 <- UV.mapM faceId $ loopSurfs1 g
                    idx <- UV.unsafeFreeze newIndex
                    return $!
                        DottedGuts
                            { wallSurfs    = wallS
                            , loopSurfs0   = loopS0
                            , loopSurfs1   = loopS1
                            , surfHandlesN = UV.backpermute (surfHandlesN g) idx
                            , surfHolesN   = UV.backpermute (surfHolesN g) idx
                            }
        in \ g ->
            if | UV.any (> 1) (surfHandlesN g)                               -> []
               | UV.any (== (0, 0)) (UV.zip (surfHolesN g) (surfHandlesN g)) -> []
               | otherwise                                                   -> removeClosed 1 g


{-# INLINE glueArcs #-}
glueArcs :: Int -> (UV.Vector Int, Int) -> (UV.Vector Int, Int) -> (UV.Vector Int, [Int])
glueArcs !gl (!a, !posA) (!b, !posB) =
    ST.runST $ do
        let legsA = UV.length a
            legsB = UV.length b

        visited <- UMV.replicate gl False

        !arcs <-
            let mateA !x | y >= gl    = return $! y - gl
                         | otherwise  = do
                             UMV.write visited y True
                             mateB $ (posB + gl - 1 - y) `mod` legsB
                    where y = ((a UV.! x) - posA) `mod` legsA

                mateB !x | y >= gl    = return $! legsA + y - 2 * gl
                         | otherwise  = mateA $ (posA + gl - 1 - y) `mod` legsA
                    where y = ((b UV.! x) - posB) `mod` legsB

            in liftM2 (UV.++) (UV.generateM (legsA - gl) (\ !i -> mateA $ (posA + gl + i) `mod` legsA))
                              (UV.generateM (legsB - gl) (\ !i -> mateB $ (posB + gl + i) `mod` legsB))

        loops <-
            let markA !x = do
                    unlessM (UMV.read visited x) $ do
                        UMV.write visited x True
                        markB $ (`mod` legsA) $ (+ negate posA) $ (a UV.!) $ (posA + x) `mod` legsA

                markB !x = do
                    unlessM (UMV.read visited x) $ do
                        UMV.write visited x True
                        markA $ (`mod` legsB) $ (\ p -> posB - p + gl - 1) $ (b UV.!) $ (posB + gl - 1 - x) `mod` legsB

            in foldM (\ !lst !i -> do
                    v <- UMV.read visited i
                    if v then return $! lst
                         else do
                             markA i
                             return $! i : lst
                ) [] [0 .. gl - 1]

        return $! (arcs, loops)


{-# INLINE rotateArcs #-}
rotateArcs :: Int -> UV.Vector Int -> UV.Vector Int
rotateArcs rot a =
    UV.create $ do
        let l = UV.length a
            t i = (i + rot) `mod` l
        a' <- UMV.new l
        forM_ [0 .. l - 1] $ \ !i ->
            UMV.write a' (t i) (t $ a UV.! i)
        return a'


data Cobordism' g = Cob {-# UNPACK #-} !CobordismHeader !g
    deriving (Eq, Show)

instance (CobordismGuts g) => Composition (Cobordism' g) where
    Cob h1 g1 ∘ Cob h0 g0 | legsN h0  /= legsN h1   = error $ printf "(∘): different leg numbers <%i> and <%i>" (legsN h1) (legsN h0)
                          | loops1 h0 /= loops0 h1  = error $ printf "(∘): different border loops numbers <%i> and <%i>" (loops0 h1) (loops1 h0)
                          | arcs1 h0  /= arcs0 h1   = error "(∘): different border arcs"
                          | otherwise               =
        let h = makeHeader (legsN h0) (arcs0 h0, loops0 h0) (arcs1 h1, loops1 h1)
        in Cob h (verComposeGuts h (h1, g1) (h0, g0))

instance (CobordismGuts g) => TensorProduct (CobordismBorder (Cobordism' g)) where
    a ⊗ b = horizontalComposition 0 (a, 0) (b, 0)

instance (CobordismGuts g) => TensorProduct (Cobordism' g) where
    a ⊗ b = horizontalComposition 0 (a, 0) (b, 0)

instance (CobordismGuts g) => Cobordism (Cobordism' g) where
    data CobordismBorder (Cobordism' g) = Brd {-# UNPACK #-} !Int !(UV.Vector Int)
        deriving (Eq, Ord, Show)

    cobordismBorder0 (Cob h _) = Brd (loops0 h) (arcs0 h)
    cobordismBorder1 (Cob h _) = Brd (loops1 h) (arcs1 h)

    identityCobordism (Brd loops arcs) =
        let h = makeHeader (UV.length arcs) (arcs, loops) (arcs, loops)
        in Cob h (identityGuts (wallHolesN h) loops)

instance (CobordismGuts g) => Cobordism3 (Cobordism' g) where
    transposeCobordism (Cob h g) =
        Cob (h { arcs0 = arcs1 h, arcs1 = arcs0 h, loops0 = loops1 h, loops1 = loops0 h })
            (flipGuts g)

    numberOfLoops (Brd ls _) = ls

    surfOfGenusCobordism genus | genus < 0  = error $ printf "surfOfGenusCobordism: genus must be non-negative, but %i passed" genus
                               | otherwise  = Cob (emptyHeader 0 0) (surfGuts genus)

    capOfGenusCobordism genus | genus < 0  = error $ printf "capOfGenusCobordism: genus must be non-negative, but %i passed" genus
                              | otherwise  = Cob (emptyHeader 1 0) (capGuts genus)

    tubeCobordism = planarLoop

    swapCobordism = Cob (emptyHeader 2 2) swapGuts

    pantsCobordism = Cob (emptyHeader 2 1) pantsGuts

instance (CobordismGuts g) => CannedCobordism (Cobordism' g) where
    saddleCobordism =
        Cob (makeHeader 4 (UV.fromList [3, 2, 1, 0], 0)
                          (UV.fromList [1, 0, 3, 2], 0)
            ) saddleGuts

instance (CobordismGuts g) => RotationAction (Cobordism' g) where
    rotationOrder (Cob h _) = legsN h

    rotateByUnchecked !rot (Cob h g) =
        let legs = legsN h
            h' = makeHeader legs (rotateArcs rot (arcs0 h), loops0 h)
                                 (rotateArcs rot (arcs1 h), loops1 h)
            subst = UV.create $ do
                s <- UMV.new (wallHolesN h)
                forM_ [0 .. legsN h - 1] $ \ !i ->
                    let newH = wallMap h' UV.! i
                        oldH = wallMap h UV.! ((i - rot) `mod` legs)
                    in UMV.write s newH oldH
                return s
        in Cob h' (rotateGuts subst g)

instance (CobordismGuts g) => PlanarAlgebra (Cobordism' g) where
    planarDegree (Cob h _) = legsN h

    planarEmpty = identityCobordism planarEmpty

    planarLoop = identityCobordism planarLoop

    planarPropagator = identityCobordism . planarPropagator

    horizontalCompositionUnchecked !gl (Cob hA gA, !posA) (Cob hB gB, !posB) =
        let (resArcs0, extraLoops0) = glueArcs gl (arcs0 hA, posA) (arcs0 hB, posB)
            (resArcs1, extraLoops1) = glueArcs gl (arcs1 hA, posA) (arcs1 hB, posB)
            h = makeHeader (legsN hA + legsN hB - 2 * gl) (resArcs0, length extraLoops0 + loops0 hA + loops0 hB)
                                                          (resArcs1, length extraLoops1 + loops1 hA + loops1 hB)
        in Cob h $ horComposeGuts (h, gl, UV.fromList extraLoops0, UV.fromList extraLoops1) (gA, hA, posA) (gB, hB, posB)

instance (CobordismGuts g) => RotationAction (CobordismBorder (Cobordism' g)) where
    rotationOrder (Brd _ a) = UV.length a

    rotateByUnchecked rot (Brd loops a) = Brd loops (rotateArcs rot a)

instance (CobordismGuts g) => MirrorAction (CobordismBorder (Cobordism' g)) where
    mirrorIt = error "mirror is not implemeted"

instance (CobordismGuts g) => PlanarAlgebra (CobordismBorder (Cobordism' g)) where
    planarDegree (Brd _ a) = UV.length a

    planarEmpty = Brd 0 UV.empty

    planarLoop = Brd 1 UV.empty

    planarPropagator n | n < 0      = error $ printf "planarPropagator: parameter must be non-negative, but %i passed" n
                       | otherwise  = Brd 0 $ UV.generate (2 * n) (\ i -> 2 * n - 1 - i)

    horizontalCompositionUnchecked !gl (Brd loopsA a, !posA) (Brd loopsB b, !posB) =
        let (arcs, extraLoops) = glueArcs gl (a, posA) (b, posB)
        in Brd (length extraLoops + loopsA + loopsB) arcs

instance (CobordismGuts g) => ChordDiagram (CobordismBorder (Cobordism' g)) where
    numberOfChordEnds (Brd _ a) = UV.length a

    chordMate (Brd _ a) x = a UV.! x
    chordMateArray (Brd _ a) = a

instance (ModuleCobordismGuts g, Integral a) => Num (Cobordism' (ModuleGuts g a)) where
    Cob h0 (MG m0) + Cob h1 (MG m1) | h0 /= h1   = error "(+): can not sum"
                                    | otherwise  =
        Cob h0 $ MG $ M.filter (/= 0) $ M.unionWith (+) m0 m1

    negate (Cob h (MG m)) =
        Cob h $ MG $ M.map negate m

    (*) = (∘)

    fromInteger 0 = Cob (emptyHeader 0 0) $ MG M.empty
    fromInteger n = Cob (emptyHeader 0 0) $ MG $ M.singleton emptyGuts (fromIntegral n)

    abs = id
    signum x = identityCobordism (cobordismBorder0 x)

instance (ModuleCobordismGuts g, Integral a) => PreadditiveCobordism (Cobordism' (ModuleGuts g a)) where
    zeroCobordism (Brd l0 a0) (Brd l1 a1) | UV.length a0 /= UV.length a1  = error "zeroCobordism: different number of legs"
                                          | otherwise                     = Cob (makeHeader (UV.length a0) (a0, l0) (a1, l1)) (MG M.empty)

    isZeroCobordism (Cob _ (MG m)) = M.null m


type DottedCobordism a = Cobordism' (ModuleGuts DottedGuts a)
