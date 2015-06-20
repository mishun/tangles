{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KhovanovHomology
    ( PlanarAlgebra'(..)
    , CannedCobordism(..)
    , numberOfLoops
    , DottedCobordism
    ) where

import Control.Exception (assert)
import Control.Monad (foldM, forM_, when)
import Control.Monad.IfElse (whenM)
import qualified Control.Monad.ST as ST
import qualified Data.Map as M
import qualified Data.STRef as STRef
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf


class PlanarAlgebra' a where
    numberOfLegs     :: a -> Int
    planarPropagator :: a
    glue             :: Int -> (a, Int) -> (a, Int) -> (a, UV.Vector Int, UV.Vector Int)
    --rotate         :: Int -> a -> a

class (PlanarAlgebra' c, PlanarAlgebra' (CobordismBorder c)) => CannedCobordism c where
    data CobordismBorder c :: *

    cobordismBorder0  :: c -> CobordismBorder c
    cobordismBorder1  :: c -> CobordismBorder c
    capCobordism      :: c
    cupCobordism      :: c
    identityCobordism :: CobordismBorder c -> c
    (∘)               :: c -> c -> c



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
                    then mark freeColor bot top i >> return (freeColor + 1)
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
    capGuts       :: g
    cupGuts       :: g
    identityGuts  :: Int -> Int -> g
    composeGuts   :: CobordismHeader -> (CobordismHeader, g) -> (CobordismHeader, g) -> g

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

    composeGuts h (h1, MG map1) (h0, MG map0) =
        MG $ M.filter (/= 0) $ M.fromListWith (+) $ do
            (g0, factor0) <- M.toList map0
            (g1, factor1) <- M.toList map1
            (g, factorNorm) <- normalizeGuts $ composeGuts h (h1, g1) (h0, g0)
            return $! assert (isNormalGuts g) (g, factor0 * factor1 * factorNorm)



data DottedGuts =
    DottedGuts
        { wallSurfs  :: !(UV.Vector Int)
        , loopSurfs0 :: !(UV.Vector Int)
        , loopSurfs1 :: !(UV.Vector Int)
        , surfsData  :: !(UV.Vector (Int, Int))
        }
    deriving (Eq, Ord, Show)

instance CobordismGuts DottedGuts where
    capGuts =
        DottedGuts
            { wallSurfs  = UV.empty
            , loopSurfs0 = UV.singleton 0
            , loopSurfs1 = UV.empty
            , surfsData  = UV.singleton (1, 0)
            }

    cupGuts =
        DottedGuts
            { wallSurfs  = UV.empty
            , loopSurfs0 = UV.empty
            , loopSurfs1 = UV.singleton 0
            , surfsData  = UV.singleton (1, 0)
            }

    identityGuts wallHoles endHoles =
        let ls = UV.generate endHoles (+ wallHoles)
        in DottedGuts
            { wallSurfs  = UV.generate wallHoles id
            , loopSurfs0 = ls
            , loopSurfs1 = ls
            , surfsData  = UV.replicate wallHoles (1, 0)
                            UV.++ UV.replicate endHoles (2, 0)
            }

    composeGuts h (h1, g1) (h0, g0) =
        ST.runST $ do
            let legs = legsN h

            (newS0, newS1, surfN) <- do
                newS0 <- UMV.replicate (UV.length $ surfsData g0) (-1 :: Int)
                newS1 <- UMV.replicate (UV.length $ surfsData g1) (-1 :: Int)

                let mark0 !color !surf = do
                        whenM ((< 0) `fmap` UMV.read newS0 surf) $ do
                            UMV.write newS0 surf color
                            forM_ [0 .. legs - 1] $ \ !leg ->
                                when (surf == wallSurfs g0 UV.! (wallMap h0 UV.! leg)) $
                                    mark1 color $ wallSurfs g1 UV.! (wallMap h1 UV.! leg)
                            forM_ [0 .. UV.length (loopSurfs1 g0) - 1] $ \ !i ->
                                when (surf == loopSurfs1 g0 UV.! i) $
                                    mark1 color $ loopSurfs0 g1 UV.! i

                    mark1 !color !surf =
                        whenM ((< 0) `fmap` UMV.read newS1 surf) $ do
                            UMV.write newS1 surf color
                            forM_ [0 .. legs - 1] $ \ !leg ->
                                when (surf == wallSurfs g1 UV.! (wallMap h1 UV.! leg)) $
                                    mark0 color $ wallSurfs g0 UV.! (wallMap h0 UV.! leg)
                            forM_ [0 .. UV.length (loopSurfs1 g0) - 1] $ \ !i ->
                                when (surf == loopSurfs0 g1 UV.! i) $
                                    mark0 color $ loopSurfs1 g0 UV.! i

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

                -- order is important!
                UV.forM_ (wallMap h0) $ tryMark0 . (wallSurfs g0 UV.!)
                UV.forM_ (loopSurfs0 g0) tryMark0
                UV.forM_ (loopSurfs1 g1) tryMark1
                UV.forM_ (loopSurfs1 g0) tryMark0
                forM_ [0 .. UV.length (surfsData g0) - 1] tryMark0
                forM_ [0 .. UV.length (surfsData g1) - 1] tryMark1

                newS0' <- UV.unsafeFreeze newS0
                newS1' <- UV.unsafeFreeze newS1
                surfN <- STRef.readSTRef freeColor
                return $! (newS0', newS1', surfN)

            sd <- do
                holes <- UMV.replicate surfN 1
                handles <- UMV.replicate surfN 0
                comps <- UMV.replicate surfN (0 :: Int)

                forM_ [0 .. UV.length newS0 - 1] $ \ !s -> do
                    let ns = newS0 UV.! s
                    fmap (+ 1) (UMV.read comps ns) >>= UMV.write comps ns

                forM_ [0 .. UV.length newS1 - 1] $ \ !s -> do
                    let ns = newS1 UV.! s
                    fmap (+ 1) (UMV.read comps ns) >>= UMV.write comps ns

                holes' <- UV.unsafeFreeze holes
                handles' <- UV.unsafeFreeze handles
                return $! UV.zip holes' handles'

            return $!
                DottedGuts
                    { wallSurfs  = UV.create $ do
                        ws <- UMV.new (wallHolesN h)
                        forM_ [0 .. legs - 1] $ \ !leg -> do
                            UMV.write ws (wallMap h UV.! leg) $ (newS0 UV.!) $ wallMap h0 UV.! leg
                        return ws
                    , loopSurfs0 = UV.map (newS0 UV.!) $ loopSurfs0 g0
                    , loopSurfs1 = UV.map (newS1 UV.!) $ loopSurfs1 g1
                    , surfsData  = sd
                    }

instance ModuleCobordismGuts DottedGuts where
    isNormalGuts = UV.all (== (1, 0)) . surfsData

    normalizeGuts g | isNormalGuts g  = [(g, 1)]
                    | otherwise       = error "normalize not implemented"



data Cobordism g = Cob !CobordismHeader !g
    deriving (Eq, Show)

instance (CobordismGuts g) => CannedCobordism (Cobordism g) where
    data CobordismBorder (Cobordism g) = Brd {-# UNPACK #-} !Int !(UV.Vector Int)
        deriving (Eq, Ord, Show)

    cobordismBorder0 (Cob h _) = Brd (loops0 h) (arcs0 h)
    cobordismBorder1 (Cob h _) = Brd (loops1 h) (arcs1 h)

    capCobordism = Cob (makeHeader 0 (UV.empty, 1) (UV.empty, 0)) capGuts
    cupCobordism = Cob (makeHeader 0 (UV.empty, 0) (UV.empty, 1)) cupGuts

    identityCobordism (Brd loops arcs) =
        let h = makeHeader (UV.length arcs) (arcs, loops) (arcs, loops)
        in Cob h (identityGuts (wallHolesN h) loops)

    (Cob h1 g1) ∘ (Cob h0 g0) | legsN h0  /= legsN h1   = error $ printf "(∘): different leg numbers <%i> and <%i>" (legsN h1) (legsN h0)
                              | loops1 h0 /= loops0 h1  = error $ printf "(∘): different border loops numbers <%i> and <%i>" (loops0 h1) (loops1 h0)
                              | arcs1 h0  /= arcs0 h1   = error "(∘): different borders"
                              | otherwise               =
        let h = makeHeader (legsN h0) (arcs0 h0, loops0 h0) (arcs1 h1, loops1 h1)
        in Cob h (composeGuts h (h1, g1) (h0, g0))


instance (CobordismGuts g) => PlanarAlgebra' (Cobordism g) where
    numberOfLegs (Cob h _) = legsN h

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
