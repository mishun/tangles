{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Algebra.Cobordism.DottedCobordism
    ( module Math.Topology.KnotTh.Algebra
    , module Math.Topology.KnotTh.Algebra.Cobordism
    , module Math.Topology.KnotTh.Algebra.PlanarAlgebra
    , KhovanovCobordism(..)
    , DottedCobordism'
    ) where

import Control.Arrow ((***))
import Control.Exception (assert)
import Control.Monad (foldM, forM_, liftM2, when)
import Control.Monad.IfElse (unlessM, whenM)
import qualified Control.Monad.ST as ST
import Data.Bits (testBit, popCount)
import qualified Data.Map.Strict as Map
import qualified Data.Matrix as M
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf
import Math.Topology.KnotTh.Algebra
import Math.Topology.KnotTh.Algebra.Cobordism
import Math.Topology.KnotTh.Algebra.Dihedral
import Math.Topology.KnotTh.Algebra.PlanarAlgebra
import Math.Topology.KnotTh.ChordDiagram
import Math.Topology.KnotTh.Knotted.Crossings.Diagram


data CobordismHeader =
    CobordismHeader
        { legsN      :: {-# UNPACK #-} !Int
        , loops0     :: {-# UNPACK #-} !Int
        , loops1     :: {-# UNPACK #-} !Int
        , arcs0      :: !(UV.Vector Int)
        , arcs1      :: !(UV.Vector Int)
        , deg0       :: !Int
        , deg1       :: !Int
        , wallHolesN :: {-# UNPACK #-} !Int
        , wallMap    :: !(UV.Vector Int)
        }
    deriving (Eq, Show)

instance TransposeAction CobordismHeader where
    transposeIt h =
        h { arcs0  = arcs1 h
          , loops0 = loops1 h
          , arcs1  = arcs0 h
          , loops1 = loops0 h
          }

emptyHeader :: (Int, Int) -> (Int, Int) -> CobordismHeader
emptyHeader (!l0, !d0) (!l1, !d1) =
    CobordismHeader
        { legsN      = 0
        , loops0     = l0
        , loops1     = l1
        , arcs0      = UV.empty
        , arcs1      = UV.empty
        , deg0       = d0
        , deg1       = d1
        , wallHolesN = 0
        , wallMap    = UV.empty
        }

{-# INLINE makeHeader #-}
makeHeader :: Int -> (UV.Vector Int, Int, Int) -> (UV.Vector Int, Int, Int) -> CobordismHeader
makeHeader !legs (!bot, !botLoops, !botDeg) (!top, !topLoops, !topDeg) =
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
                , deg0       = botDeg
                , deg1       = topDeg
                , wallHolesN = hn
                , wallMap    = hid'
                }


data Guts =
    Guts
        { wallSurfs   :: !(UV.Vector Int)
        , loopSurfs0  :: !(UV.Vector Int)
        , loopSurfs1  :: !(UV.Vector Int)
        , surfacesN   :: !Int
        , surfHoles   :: !(UV.Vector Int)
        , surfHandles :: !(UV.Vector Int)
        }
    deriving (Eq, Ord, Show)

instance TransposeAction Guts where
    transposeIt g =
        g { loopSurfs0 = loopSurfs1 g
          , loopSurfs1 = loopSurfs0 g
          }


emptyGuts :: Guts
emptyGuts =
    Guts
        { wallSurfs   = UV.empty
        , loopSurfs0  = UV.empty
        , loopSurfs1  = UV.empty
        , surfacesN   = 0
        , surfHoles   = UV.empty
        , surfHandles = UV.empty
        }


surfGuts :: Int -> Guts
surfGuts genus =
    Guts
        { wallSurfs   = UV.empty
        , loopSurfs0  = UV.empty
        , loopSurfs1  = UV.empty
        , surfacesN   = 1
        , surfHoles   = UV.singleton 0
        , surfHandles = UV.singleton genus
        }


capGuts :: Int -> Guts
capGuts genus =
    Guts
        { wallSurfs   = UV.empty
        , loopSurfs0  = UV.singleton 0
        , loopSurfs1  = UV.empty
        , surfacesN   = 1
        , surfHoles   = UV.singleton 1
        , surfHandles = UV.singleton genus
        }


swapGuts :: Guts
swapGuts =
    Guts
        { wallSurfs   = UV.empty
        , loopSurfs0  = UV.fromList [0, 1]
        , loopSurfs1  = UV.fromList [1, 0]
        , surfacesN   = 2
        , surfHoles   = UV.replicate 2 2
        , surfHandles = UV.replicate 2 0
        }


pantsGuts :: Guts
pantsGuts =
    Guts
        { wallSurfs   = UV.empty
        , loopSurfs0  = UV.replicate 2 0
        , loopSurfs1  = UV.singleton 0
        , surfacesN   = 1
        , surfHoles   = UV.singleton 3
        , surfHandles = UV.singleton 0
        }


saddleGuts :: Guts
saddleGuts =
    Guts
        { wallSurfs   = UV.singleton 0
        , loopSurfs0  = UV.empty
        , loopSurfs1  = UV.empty
        , surfacesN   = 1
        , surfHoles   = UV.singleton 1
        , surfHandles = UV.singleton 0
        }


identityGuts :: Int -> Int -> Guts
identityGuts wallHoles endHoles =
    let ls = UV.enumFromN wallHoles endHoles
    in Guts
        { wallSurfs   = UV.enumFromN 0 wallHoles
        , loopSurfs0  = ls
        , loopSurfs1  = ls
        , surfacesN   = wallHoles + endHoles
        , surfHoles   = UV.replicate wallHoles 1 UV.++ UV.replicate endHoles 2
        , surfHandles = UV.replicate (wallHoles + endHoles) 0
        }


genusOfGuts :: Guts -> Int
genusOfGuts = UV.sum . surfHandles


rotateGuts :: UV.Vector Int -> Guts -> Guts
rotateGuts perm g =
    g { wallSurfs = UV.backpermute (wallSurfs g) perm }


renumerateSurfaces :: Guts -> Guts
renumerateSurfaces g = assert (surfacesN g == UV.length (surfHoles g)) $
    ST.runST $ do
        let n = surfacesN g

        newIndex <- UMV.replicate n (-1)
        newHandles <- UMV.new n
        newHoles <- UMV.new n
        freeIndex <- newSTRef 0

        let faceId !i = do
                tmp <- UMV.read newIndex i
                if tmp >= 0 then return tmp
                            else do
                    j <- readSTRef freeIndex
                    writeSTRef freeIndex $! j + 1
                    UMV.write newIndex i j
                    UMV.write newHandles j (surfHandles g UV.! i)
                    UMV.write newHoles j (surfHoles g UV.! i)
                    return j

        wallS <- UV.mapM faceId $ wallSurfs g
        loopS0 <- UV.mapM faceId $ loopSurfs0 g
        loopS1 <- UV.mapM faceId $ loopSurfs1 g

        handles' <- UV.unsafeFreeze newHandles
        holes' <- UV.unsafeFreeze newHoles

        return $!
            Guts { wallSurfs   = wallS
                 , loopSurfs0  = loopS0
                 , loopSurfs1  = loopS1
                 , surfacesN   = surfacesN g
                 , surfHandles = handles'
                 , surfHoles   = holes'
                 }


verComposeGuts :: Int -> UV.Vector (Int, Int) -> UV.Vector Int -> UV.Vector Int -> Guts -> Guts -> Guts
verComposeGuts !newBorderHolesN !segmsToGlueHoles !cornerHoles0 !newCornerHoles !g1 !g0 =
    ST.runST $ do
        let segmsToGlueSurf = UV.map ((wallSurfs g0 UV.!) *** (wallSurfs g1 UV.!)) segmsToGlueHoles

            loopsToGlueSurf = UV.zip (loopSurfs1 g0) (loopSurfs0 g1)

        (newS0, newS1, surfN) <- do
            newS0 <- UMV.replicate (surfacesN g0) (-1)
            newS1 <- UMV.replicate (surfacesN g1) (-1)

            let mark0 !color !surf =
                    whenM ((< 0) `fmap` UMV.read newS0 surf) $ do
                        UMV.write newS0 surf color
                        UV.forM_ segmsToGlueSurf $ \ (s0, s1) -> when (surf == s0) $ mark1 color s1
                        UV.forM_ loopsToGlueSurf $ \ (s0, s1) -> when (surf == s0) $ mark1 color s1

                mark1 !color !surf =
                    whenM ((< 0) `fmap` UMV.read newS1 surf) $ do
                        UMV.write newS1 surf color
                        UV.forM_ segmsToGlueSurf $ \ (s0, s1) -> when (surf == s1) $ mark0 color s0
                        UV.forM_ loopsToGlueSurf $ \ (s0, s1) -> when (surf == s1) $ mark0 color s0

            freeColor <- newSTRef 0
            let tryMark0 !s =
                    whenM ((< 0) `fmap` UMV.read newS0 s) $ do
                        color <- readSTRef freeColor
                        writeSTRef freeColor $! color + 1
                        mark0 color s

                tryMark1 !s =
                    whenM ((< 0) `fmap` UMV.read newS1 s) $ do
                        color <- readSTRef freeColor
                        writeSTRef freeColor $! color + 1
                        mark1 color s

            -- the order is important!
            -- wall adjacent surfaces:
            UV.mapM_ (tryMark0 . (wallSurfs g0 UV.!)) cornerHoles0
            -- bottom and top loops:
            UV.mapM_ tryMark0 $ loopSurfs0 g0
            UV.mapM_ tryMark1 $ loopSurfs1 g1
            -- closed surfaces:
            UV.mapM_ tryMark0 $ loopSurfs1 g0
            forM_ [0 .. surfacesN g0 - 1] tryMark0
            forM_ [0 .. surfacesN g1 - 1] tryMark1

            newS0' <- UV.unsafeFreeze newS0
            newS1' <- UV.unsafeFreeze newS1
            surfN <- readSTRef freeColor
            return (newS0', newS1', surfN)

        let wallS = UV.create $ do
                ws <- UMV.new newBorderHolesN
                UV.zipWithM_ (UMV.write ws) newCornerHoles (UV.backpermute newS0 cornerHoles0)
                return ws

            loopS0 = UV.backpermute newS0 (loopSurfs0 g0)
            loopS1 = UV.backpermute newS1 (loopSurfs1 g1)

            holes = UV.create $ do
                hs <- UMV.replicate surfN 0
                UV.mapM_ (UMV.modify hs (+ 1)) $
                    wallS UV.++ loopS0 UV.++ loopS1
                return hs

            newHandles = UV.map (\ g2 -> assert (even g2) (g2 `div` 2)) $ UV.create $ do
                g2 <- UMV.replicate surfN 0
                UV.imapM_ (\ s hi -> UMV.modify g2 (+ (2 - hi)) s) holes
                UV.zipWithM_ (\ hi -> UMV.modify g2 (+ (hi - 2))) (surfHoles g0) newS0
                UV.zipWithM_ (\ hi -> UMV.modify g2 (+ (hi - 2))) (surfHoles g1) newS1
                UV.mapM_ (\ (s0, _) -> UMV.modify g2 (+ 1) (newS0 UV.! s0)) segmsToGlueSurf
                return g2

        return $!
            Guts
                { wallSurfs   = wallS
                , loopSurfs0  = loopS0
                , loopSurfs1  = loopS1
                , surfacesN   = surfN
                , surfHoles   = holes
                , surfHandles =
                    UV.modify (\ handles -> do
                            UV.zipWithM_ (\ dh -> UMV.modify handles (+ dh)) (surfHandles g0) newS0
                            UV.zipWithM_ (\ dh -> UMV.modify handles (+ dh)) (surfHandles g1) newS1
                        ) newHandles
                }


horComposeGuts :: Int -> UV.Vector (Int, Int) -> (UV.Vector Int, UV.Vector Int) -> (UV.Vector Int, UV.Vector Int, Guts) -> (UV.Vector Int, UV.Vector Int, Guts) -> Guts
horComposeGuts !newBorderHolesN !segmsToGlueHoles (!exLpRepsA0, !exLpRepsA1) (!cornerHolesA, newCornerHolesA, !gA) (!cornerHolesB, newCornerHolesB, !gB) =
    ST.runST $ do
        let segmsToGlueSurf = UV.map ((wallSurfs gA UV.!) *** (wallSurfs gB UV.!)) segmsToGlueHoles

        (newSA, newSB, surfN) <- do
            newSA <- UMV.replicate (surfacesN gA) (-1)
            newSB <- UMV.replicate (surfacesN gB) (-1)

            let markA !color !surf =
                    whenM ((< 0) `fmap` UMV.read newSA surf) $ do
                        UMV.write newSA surf color
                        UV.forM_ segmsToGlueSurf $ \ (sA, sB) -> when (surf == sA) $ markB color sB

                markB !color !surf =
                    whenM ((< 0) `fmap` UMV.read newSB surf) $ do
                        UMV.write newSB surf color
                        UV.forM_ segmsToGlueSurf $ \ (sA, sB) -> when (surf == sB) $ markA color sA

            freeColor <- newSTRef 0
            let tryMarkA !s =
                    whenM ((< 0) `fmap` UMV.read newSA s) $ do
                        color <- readSTRef freeColor
                        writeSTRef freeColor $! color + 1
                        markA color s

                tryMarkB !s =
                    whenM ((< 0) `fmap` UMV.read newSB s) $ do
                        color <- readSTRef freeColor
                        writeSTRef freeColor $! color + 1
                        markB color s

            -- the order is important!
            -- wall adjacent surfaces:
            UV.mapM_ (tryMarkA . (wallSurfs gA UV.!)) cornerHolesA
            UV.mapM_ (tryMarkB . (wallSurfs gB UV.!)) cornerHolesB
            -- bottom loops:
            UV.mapM_ (tryMarkA . fst) segmsToGlueSurf
            UV.mapM_ tryMarkA $ loopSurfs0 gA
            UV.mapM_ tryMarkB $ loopSurfs0 gB
            -- top loops:
            UV.mapM_ tryMarkA $ loopSurfs1 gA
            UV.mapM_ tryMarkB $ loopSurfs1 gB
            -- closed surfaces:
            forM_ [0 .. surfacesN gA - 1] tryMarkA
            forM_ [0 .. surfacesN gB - 1] tryMarkB

            newSA' <- UV.unsafeFreeze newSA
            newSB' <- UV.unsafeFreeze newSB
            surfN <- readSTRef freeColor
            return (newSA', newSB', surfN)

        let wallS = UV.create $ do
                ws <- UMV.new newBorderHolesN
                UV.zipWithM_ (UMV.write ws) newCornerHolesA (UV.backpermute newSA cornerHolesA)
                UV.zipWithM_ (UMV.write ws) newCornerHolesB (UV.backpermute newSB cornerHolesB)
                return ws

            loopS0 =
                UV.concat [ UV.backpermute newSA exLpRepsA0
                          , UV.backpermute newSA (loopSurfs0 gA)
                          , UV.backpermute newSB (loopSurfs0 gB)
                          ]

            loopS1 =
                UV.concat [ UV.backpermute newSA exLpRepsA1
                          , UV.backpermute newSA (loopSurfs1 gA)
                          , UV.backpermute newSB (loopSurfs1 gB)
                          ]

            holes = UV.create $ do
                hs <- UMV.replicate surfN 0
                UV.mapM_ (UMV.modify hs (+ 1)) $
                    wallS UV.++ loopS0 UV.++ loopS1
                return hs

            newHandles = UV.map (\ g2 -> assert (even g2) (g2 `div` 2)) $ UV.create $ do
                g2 <- UMV.replicate surfN 0
                UV.imapM_ (\ s hs -> UMV.modify g2 (+ (2 - hs)) s) holes
                UV.zipWithM_ (\ hs -> UMV.modify g2 (+ (hs - 2))) (surfHoles gA) newSA
                UV.zipWithM_ (\ hs -> UMV.modify g2 (+ (hs - 2))) (surfHoles gB) newSB
                UV.mapM_ (\ (sA, _) -> UMV.modify g2 (+ 1) (newSA UV.! sA)) segmsToGlueSurf
                return g2

        return $!
            Guts
                { wallSurfs   = wallS
                , loopSurfs0  = loopS0
                , loopSurfs1  = loopS1
                , surfacesN   = surfN
                , surfHoles   = holes
                , surfHandles =
                    UV.modify (\ handles -> do
                            UV.zipWithM_ (\ dh -> UMV.modify handles (+ dh)) (surfHandles gA) newSA
                            UV.zipWithM_ (\ dh -> UMV.modify handles (+ dh)) (surfHandles gB) newSB
                        ) newHandles
                }


{-# INLINE glueArcs #-}
glueArcs :: Int -> (UV.Vector Int, Int) -> (UV.Vector Int, Int) -> (UV.Vector Int, UV.Vector Int)
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
            let markA !i =
                    unlessM (UMV.read visited i) $ do
                        UMV.write visited i True
                        let x = (posA + i) `mod` legsA
                        markB $ ((a UV.! x) - posA) `mod` legsA

                markB !i =
                    unlessM (UMV.read visited i) $ do
                        UMV.write visited i True
                        let x = (posB + gl - 1 - i) `mod` legsB
                        markA $ (posB + gl - 1 - (b UV.! x)) `mod` legsB

            in foldM (\ !lst !i -> do
                    v <- UMV.read visited i
                    if v then return $! lst
                         else do
                             markA i
                             return $! ((posA + i) `mod` legsA) : lst
                ) [] [0 .. gl - 1]

        return (arcs, UV.fromList loops)


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


data DottedCobordism' a = Cob !CobordismHeader !(Map.Map Guts a)
    deriving (Eq, Show)


normalizeDottedGuts :: (Num a) => Guts -> [(Guts, a)]
normalizeDottedGuts =
    let removeClosed g =
            let (holes, handles) = UV.unzip $ UV.filter ((> 0) . fst) $ UV.zip (surfHoles g) (surfHandles g)
            in cutNecks $ g { surfacesN   = UV.length holes
                            , surfHoles   = holes
                            , surfHandles = handles
                            }

        cutNecks g =
            case UV.findIndex (> 1) (surfHoles g) of
                Nothing   -> [(renumerateSurfaces g, 1)]
                Just surf -> do
                    let genus = surfHandles g UV.! surf
                        dst = UV.length $ surfHandles g
                        rew | Just i <- UV.findIndex (== surf) (wallSurfs g)   = g { wallSurfs  = wallSurfs g  UV.// [(i, dst)] }
                            | Just i <- UV.findIndex (== surf) (loopSurfs0 g)  = g { loopSurfs0 = loopSurfs0 g UV.// [(i, dst)] }
                            | Just i <- UV.findIndex (== surf) (loopSurfs1 g)  = g { loopSurfs1 = loopSurfs1 g UV.// [(i, dst)] }
                            | otherwise                                        = g
                    handles <- UV.snoc (surfHandles g) 1 : [ UV.snoc (surfHandles g) 0 UV.// [(surf, 1)] | genus == 0]
                    cutNecks $
                        rew { surfacesN   = UV.length handles
                            , surfHoles   = UV.snoc (surfHoles g) 1 UV.// [(surf, (surfHoles g UV.! surf) - 1)]
                            , surfHandles = handles
                            }

    in \ g ->
        if | UV.any (> 1) (surfHandles g)                              -> []
           | UV.any (== (0, 0)) (UV.zip (surfHoles g) (surfHandles g)) -> []
           | otherwise                                                 -> removeClosed g


normalizeDottedCobordism :: (Eq a, Num a) => DottedCobordism' a -> DottedCobordism' a
normalizeDottedCobordism (Cob h m) =
    Cob h $ Map.filter (/= 0) $ Map.fromListWith (+) $ do
        (g, f) <- Map.toList m
        (g', f') <- normalizeDottedGuts g
        return (g', f * f')


instance (Eq a, Num a) => Composition (DottedCobordism' a) where
    Cob h1 map1 ∘ Cob h0 map0 | legsN h0  /= legsN h1   = error $ printf "(∘): different leg numbers <%i> and <%i>" (legsN h1) (legsN h0)
                              | loops1 h0 /= loops0 h1  = error $ printf "(∘): different border loops numbers <%i> and <%i>" (loops0 h1) (loops1 h0)
                              | deg1 h0   /= deg0 h1    = error "(∘): different degrees"
                              | arcs1 h0  /= arcs0 h1   = error "(∘): different border arcs"
                              | otherwise               =
        let h = makeHeader (legsN h0) (arcs0 h0, loops0 h0, deg0 h0)
                                      (arcs1 h1, loops1 h1, deg1 h1)

            segmsToGlueHoles =
                let mate = (arcs1 h0 UV.!)
                in UV.ifilter (\ i _ -> i < mate i) $ UV.zip (wallMap h0) (wallMap h1)

        in Cob h $ Map.filter (/= 0) $ Map.fromListWith (+) $ do
            (g0, f0) <- Map.toList map0
            (g1, f1) <- Map.toList map1
            let f = f0 * f1 * 2 ^ (genusOfGuts g - genusOfGuts g0 - genusOfGuts g1)
                g = verComposeGuts (wallHolesN h) segmsToGlueHoles (wallMap h0) (wallMap h) g1 g0
            (g', f') <- normalizeDottedGuts g
            return (g', f * f')

instance TensorProduct (CobordismBorder (DottedCobordism' a)) where
    a ⊗ b = horizontalComposition 0 (a, 0) (b, 0)

instance (Eq a, Num a) => TensorProduct (DottedCobordism' a) where
    a ⊗ b = horizontalComposition 0 (a, 0) (b, 0)

instance (Eq a, Num a) => Cobordism (DottedCobordism' a) where
    data CobordismBorder (DottedCobordism' a) = Brd {-# UNPACK #-} !Int !(UV.Vector Int) {-# UNPACK #-} !Int
        deriving (Eq, Ord, Show)

    cobordismBorder0 (Cob h _) = Brd (loops0 h) (arcs0 h) (deg0 h)
    cobordismBorder1 (Cob h _) = Brd (loops1 h) (arcs1 h) (deg1 h)

    identityCobordism (Brd loops arcs deg) =
        normalizeDottedCobordism $
            let h = makeHeader (UV.length arcs) (arcs, loops, deg) (arcs, loops, deg)
            in Cob h $ Map.singleton (identityGuts (wallHolesN h) loops) 1

instance (Eq a, Num a) => TransposeAction (DottedCobordism' a) where
    transposeIt (Cob h m) =
        Cob (transposeIt h) $
            Map.filter (/= 0) $ Map.fromListWith (+) $ do
                (g, factor) <- Map.toList m
                (g', factorNorm) <- normalizeDottedGuts $ transposeIt g
                return (g', factor * factorNorm)

instance (Eq a, Num a) => Cobordism3 (DottedCobordism' a) where
    numberOfLoops (Brd ls _ _) = ls

    surfOfGenusCobordism g =
        normalizeDottedCobordism $
            Cob (emptyHeader (0, 0) (0, -2 + 2 * g)) $ Map.singleton (surfGuts g) 2

    capOfGenusCobordism 0 = Cob (emptyHeader (1, 0) (0, -1)) $ Map.singleton (capGuts 0) 1
    capOfGenusCobordism 1 = Cob (emptyHeader (1, 0) (0,  1)) $ Map.singleton (capGuts 1) 2
    capOfGenusCobordism g = Cob (emptyHeader (1, 0) (0, -1 + 2 * g)) Map.empty

    cupOfGenusCobordism 0 = Cob (emptyHeader (0,  1) (1, 0)) $ Map.singleton (transposeIt $ capGuts 0) 1
    cupOfGenusCobordism 1 = Cob (emptyHeader (0, -1) (1, 0)) $ Map.singleton (transposeIt $ capGuts 1) 2
    cupOfGenusCobordism g = Cob (emptyHeader (0, 1 - 2 * g) (1, 0)) Map.empty

    tubeCobordism = planarLoop 1

    swapCobordism =
        normalizeDottedCobordism $
            Cob (emptyHeader (2, 0) (2, 0)) $ Map.singleton swapGuts 1

    pantsCobordism =
        normalizeDottedCobordism $
            Cob (emptyHeader (2, 0) (1, 1)) $ Map.singleton pantsGuts 1

    pantsCobordism' =
        normalizeDottedCobordism $
            Cob (emptyHeader (1, 0) (2, 1)) $ Map.singleton (transposeIt pantsGuts) 1

instance (Eq a, Num a) => CannedCobordism (DottedCobordism' a) where
    saddleCobordism =
        let bot = UV.fromList [3, 2, 1, 0]
            top = UV.fromList [1, 0, 3, 2]
        in Cob (makeHeader 4 (bot, 0, 0) (top, 0, 1)) (Map.singleton saddleGuts 1)

    saddleCobordism' =
        let bot = UV.fromList [1, 0, 3, 2]
            top = UV.fromList [3, 2, 1, 0]
        in Cob (makeHeader 4 (bot, 0, 0) (top, 0, 1)) (Map.singleton saddleGuts 1)

instance (Eq a, Num a) => RotationAction (DottedCobordism' a) where
    rotationOrder (Cob h _) = legsN h

    rotateByUnchecked !rot (Cob h m) =
        let legs = legsN h
            h' = makeHeader legs (rotateArcs rot (arcs0 h), loops0 h, deg0 h)
                                 (rotateArcs rot (arcs1 h), loops1 h, deg1 h)
            subst = UV.create $ do
                s <- UMV.new (wallHolesN h)
                forM_ [0 .. legsN h - 1] $ \ !i ->
                    let newH = wallMap h' UV.! i
                        oldH = wallMap h UV.! ((i - rot) `mod` legs)
                    in UMV.write s newH oldH
                return s

        in Cob h' $ Map.filter (/= 0) $ Map.fromListWith (+) $ do
            (g, factor) <- Map.toList m
            (g', factorNorm) <- normalizeDottedGuts $ rotateGuts subst g
            return (g', factor * factorNorm)

instance (Eq a, Num a) => PlanarAlgebra (DottedCobordism' a) where
    planarDegree (Cob h _) = legsN h

    planarEmpty = identityCobordism planarEmpty

    planarLoop = identityCobordism . planarLoop

    planarPropagator = identityCobordism . planarPropagator

    horizontalCompositionUnchecked !gl (Cob hA mapA, !posA) (Cob hB mapB, !posB) =
        let legsA = legsN hA
            legsB = legsN hB

            (resArcs0, extraLoops0) = glueArcs gl (arcs0 hA, posA) (arcs0 hB, posB)
            (resArcs1, extraLoops1) = glueArcs gl (arcs1 hA, posA) (arcs1 hB, posB)

            cornerHolesA = UV.backpermute (wallMap hA) $ UV.generate (legsA - gl) (\ i -> (posA + gl + i) `mod` legsA)
            cornerHolesB = UV.backpermute (wallMap hB) $ UV.generate (legsB - gl) (\ i -> (posB + gl + i) `mod` legsB)

            segmsToGlueHoles = UV.generate gl $ \ i ->
                let legA = (posA + i) `mod` legsN hA
                    legB = (posB + gl - 1 - i) `mod` legsN hB
                in (wallMap hA UV.! legA, wallMap hB UV.! legB)

            h = makeHeader (legsA + legsB - 2 * gl)
                           (resArcs0, UV.length extraLoops0 + loops0 hA + loops0 hB, deg0 hA + deg0 hB)
                           (resArcs1, UV.length extraLoops1 + loops1 hA + loops1 hB, deg1 hA + deg1 hB)

            (newCornerHolesA, newCornerHolesB) = UV.splitAt (legsA - gl) (wallMap h)

            tmp = (UV.backpermute (wallMap hA) extraLoops0, UV.backpermute (wallMap hA) extraLoops1)

        in Cob h $ Map.filter (/= 0) $ Map.fromListWith (+) $ do
            (gA, fA) <- Map.toList mapA
            (gB, fB) <- Map.toList mapB
            let f = fA * fB * 2 ^ (genusOfGuts g - genusOfGuts gA - genusOfGuts gB)
                g = horComposeGuts (wallHolesN h) segmsToGlueHoles tmp (cornerHolesA, newCornerHolesA, gA)
                                                                       (cornerHolesB, newCornerHolesB, gB)
            (g', f') <- normalizeDottedGuts g
            return (g', f * f')

instance RotationAction (CobordismBorder (DottedCobordism' a)) where
    rotationOrder (Brd _ a _) = UV.length a

    rotateByUnchecked rot (Brd loops a deg) = Brd loops (rotateArcs rot a) deg

instance MirrorAction (CobordismBorder (DottedCobordism' a)) where
    mirrorIt = error "mirror is not implemeted"

instance PlanarAlgebra (CobordismBorder (DottedCobordism' a)) where
    planarDegree (Brd _ a _) = UV.length a

    planarEmpty = Brd 0 UV.empty 0

    planarLoop n | n >= 0     = Brd n UV.empty 0
                 | otherwise  = error $ printf "planarLoop: number of loops %i is negative" n

    planarPropagator n | n < 0      = error $ printf "planarPropagator: parameter must be non-negative, but %i passed" n
                       | otherwise  = Brd 0 (UV.generate (2 * n) $ \ i -> 2 * n - 1 - i) 0

    horizontalCompositionUnchecked !gl (Brd loopsA a degA, !posA) (Brd loopsB b degB, !posB) =
        let (arcs, extraLoops) = glueArcs gl (a, posA) (b, posB)
        in Brd (UV.length extraLoops + loopsA + loopsB) arcs (degA + degB)

instance ChordDiagram (CobordismBorder (DottedCobordism' a)) where
    numberOfChordEnds (Brd _ a _) = UV.length a

    chordMate (Brd _ a _) x = a UV.! x
    chordMateArray (Brd _ a _) = a

instance (Eq a, Num a) => Num (DottedCobordism' a) where
    Cob h0 m0 + Cob h1 m1 | h0 /= h1   = error "(+): can not sum"
                          | otherwise  =
        Cob h0 $ Map.filter (/= 0) $ Map.unionWith (+) m0 m1

    negate (Cob h m) =
        Cob h $ Map.map negate m

    (*) = (∘)

    fromInteger 0 = Cob (emptyHeader (0, 0) (0, 0)) Map.empty
    fromInteger n = Cob (emptyHeader (0, 0) (0, 0)) $ Map.singleton emptyGuts (fromIntegral n)

    abs = id
    signum x = identityCobordism (cobordismBorder0 x)

instance (Eq a, Num a) => PreadditiveCobordism (DottedCobordism' a) where
    zeroCobordism (Brd l0 a0 d0) (Brd l1 a1 d1) | UV.length a0 /= UV.length a1  = error "zeroCobordism: different number of legs"
                                                | otherwise                     = Cob (makeHeader (UV.length a0) (a0, l0, d0) (a1, l1, d1)) Map.empty

    isZeroCobordism (Cob _ m) = Map.null m


class (CannedCobordism c, PreadditiveCobordism c, Show c, Show (CobordismBorder c)) => KhovanovCobordism c where
    type Grading c    :: *

    crossingCobordism :: DiagramCrossing -> c
    isIsomorphism     :: c -> Bool
    delooping         :: CobordismBorder c -> V.Vector (c, c)
    tqftBorderDim     :: CobordismBorder c -> V.Vector (Grading c)
    tqft              :: c -> M.Matrix Integer


instance (Integral a, Show a) => KhovanovCobordism (DottedCobordism' a) where
    type Grading (DottedCobordism' a) = Int

    crossingCobordism OverCrossing  = saddleCobordism
    crossingCobordism UnderCrossing = saddleCobordism'

    isIsomorphism c | b0 /= b1                         = False
                    | (c ∘ c) == identityCobordism b0  = True
                    | otherwise                        = False
        where b0 = cobordismBorder0 c
              b1 = cobordismBorder1 c

    delooping (Brd loops arcs deg) =
        let cap0 = Cob (makeHeader 0 (UV.empty, 1, 0) (UV.empty, 0, -1)) $ Map.singleton (capGuts 0) 1
            cap1 = Cob (makeHeader 0 (UV.empty, 1, 0) (UV.empty, 0, 1)) $ Map.singleton (capGuts 1) 1
            cup0 = Cob (makeHeader 0 (UV.empty, 0, 1) (UV.empty, 1, 0)) $ Map.singleton (transposeIt $ capGuts 0) 1
            cup1 = Cob (makeHeader 0 (UV.empty, 0, -1) (UV.empty, 1, 0)) $ Map.singleton (transposeIt $ capGuts 1) 1

            generate 0 cobs = cobs
            generate n cobs =
                generate (n - 1) $ do
                    (a, b) <- cobs
                    [(a ⊗ cap0, b ⊗ cup1), (a ⊗ cap1, b ⊗ cup0)]

            delooped = Brd 0 arcs deg

        in V.fromList $ generate loops [(identityCobordism delooped, identityCobordism delooped)]

    tqftBorderDim b@(Brd _ _ deg) = V.generate (2 ^ numberOfChords b) (\ i -> deg - popCount i)

    tqft (Cob header m) =
        let dim = 2 ^ (legsN header `div` 2)
            segs0 = UV.backpermute (wallMap header) $ UV.ifilter (<) $ arcs0 header
            segs1 = UV.backpermute (wallMap header) $ UV.ifilter (<) $ arcs1 header

            matrix factor g =
                M.matrix dim dim $ \ (row, col) ->
                    let delta = UV.create $ do
                            d <- UMV.replicate (wallHolesN header) (-1 :: Int)
                            UV.mapM_ (UMV.modify d (+ 1)) segs0
                            UV.imapM_ (\ h s -> UMV.modify d (+ (surfHandles g UV.! s)) h) $ wallSurfs g
                            UV.imapM_ (\ i -> when (testBit (col - 1) i) . UMV.modify d (+ 1)) segs0
                            UV.imapM_ (\ i -> when (testBit (row - 1) i) . UMV.modify d (+ (-1))) segs1
                            return d
                    in if UV.all (== 0) delta
                        then factor
                        else 0

        in Map.foldlWithKey' (\ carry k v -> carry + matrix (fromIntegral v) k) (M.zero dim dim) m
