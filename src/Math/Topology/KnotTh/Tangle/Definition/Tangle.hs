{-# LANGUAGE TemplateHaskell, TypeFamilies, UnboxedTuples #-}
module Math.Topology.KnotTh.Tangle.Definition.Tangle
    ( Tangle
    , TangleProjection
    , TangleProjectionVertex
    , TangleProjectionDart
    , TangleDiagram
    , TangleDiagramVertex
    , TangleDiagramDart
    ) where

import Data.Maybe (fromMaybe)
import Data.Bits ((.&.), complement, shiftL, shiftR)
import Data.List (nub, sort, foldl')
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Primitive.Mutable as PMV
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (runST)
import Control.Monad (void, forM_, when, unless, foldM_, foldM)
import Control.DeepSeq (NFData(..))
import Text.Printf
import qualified Math.Algebra.Group.D4 as D4
import qualified Math.Algebra.RotationDirection as R
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Projection
import Math.Topology.KnotTh.Crossings.Diagram
import Math.Topology.KnotTh.Tangle.Definition.TangleLike


data Tangle a =
    Tangle
        { loopsCount      :: {-# UNPACK #-} !Int
        , vertexCount     :: {-# UNPACK #-} !Int
        , involutionArray :: {-# UNPACK #-} !(PV.Vector Int)
        , crossingsArray  :: {-# UNPACK #-} !(V.Vector a)
        , legsCount       :: {-# UNPACK #-} !Int
        }


instance PlanarDiagram Tangle where
    numberOfVertices = vertexCount

    numberOfEdges t = PV.length (involutionArray t) `shiftR` 1

    numberOfDarts t = PV.length (involutionArray t)

    nthVertex t i | i < 1 || i > b  = error $ printf "nthVertex: index %i is out of bounds [1, %i]" i b
                  | otherwise       = Vertex t (i - 1)
        where
             b = numberOfVertices t

    nthDart t i | i < 0 || i >= b  = error $ printf "nthDart: index %i is out of bounds [0, %i)" i b
                | otherwise        = Dart t i
        where
            b = PV.length (involutionArray t)

    allVertices t = map (Vertex t) [0 .. numberOfVertices t - 1]

    allHalfEdges t = map (Dart t) [0 .. PV.length (involutionArray t) - 1]

    allEdges t =
        foldl' (\ !es !i ->
                let j = involutionArray t `PV.unsafeIndex` i
                in if i < j
                    then (Dart t i, Dart t j) : es
                    else es
            ) [] [0 .. PV.length (involutionArray t) - 1]

    data Vertex Tangle a = Vertex !(Tangle a) {-# UNPACK #-} !Int

    vertexDegree _ = 4
    vertexOwner (Vertex t _) = t
    vertexIndex (Vertex _ i) = i + 1

    nthOutcomingDart (Vertex t c) i = Dart t ((c `shiftL` 2) + (i .&. 3))

    outcomingDarts c = map (nthOutcomingDart c) [0 .. 3]

    data Dart Tangle a = Dart !(Tangle a) {-# UNPACK #-} !Int

    dartOwner (Dart t _) = t
    dartIndex (Dart _ i) = i

    opposite (Dart t d) = Dart t (involutionArray t `PV.unsafeIndex` d)

    beginVertex (Dart t d) | d >= n     = error $ printf "beginVertex: taken from %i-th leg" (d - n)
                           | otherwise  = Vertex t (d `shiftR` 2)
        where
            n = vertexCount t `shiftL` 2

    beginPlace (Dart t d) | d >= n     = error $ printf "beginPlace: taken from %i-th leg" (d - n)
                          | otherwise  = d .&. 3
        where
            n = vertexCount t `shiftL` 2

    beginPair' d | isDart d   = (vertexIndex $ beginVertex d, beginPlace d)
                 | otherwise  = (0, legPlace d)

    nextCCW (Dart t d) | d >= n     = Dart t (n + (d - n + 1) `mod` legsCount t)
                       | otherwise  = Dart t ((d .&. complement 3) + ((d + 1) .&. 3))
        where
            n = vertexCount t `shiftL` 2

    nextCW (Dart t d) | d >= n     = Dart t (n + (d - n - 1) `mod` legsCount t)
                      | otherwise  = Dart t ((d .&. complement 3) + ((d - 1) .&. 3))
        where
            n = vertexCount t `shiftL` 2

    isDart (Dart t i) = i < (vertexCount t `shiftL` 2)

    vertexIndicesRange t = (1, numberOfVertices t)

    dartIndicesRange t = (0, numberOfDarts t - 1)


instance (NFData a) => NFData (Tangle a) where
    rnf t = rnf (crossingsArray t) `seq` t `seq` ()

instance (NFData a) => NFData (Vertex Tangle a)

instance (NFData a) => NFData (Dart Tangle a)


instance Functor Tangle where
    fmap f t = t { crossingsArray = f `fmap` crossingsArray t }


instance Knotted Tangle where
    vertexCrossing (Vertex t i) = crossingsArray t `V.unsafeIndex` i

    unrootedHomeomorphismInvariant tangle
        | n > 127    = error $ printf "unrootedHomeomorphismInvariant: too many crossings (%i)" n
        | otherwise  = UV.concat $ UV.singleton (numberOfFreeLoops tangle) : border : internal
        where
            n = numberOfVertices tangle
            l = numberOfLegs tangle

            border | l == 0    = UV.empty
                   | otherwise = minimum $ do
                leg <- allLegs tangle
                dir <- R.bothDirections
                globalG <- fromMaybe [D4.i] $ globalTransformations tangle

                return $! UV.create $ do
                    index <- UMV.replicate (n + 1) 0
                    queue <- MV.new n
                    free <- newSTRef 1

                    let {-# INLINE look #-}
                        look !d | isLeg d    = return 0
                                | otherwise  = do
                            let u = beginVertex d
                            ux <- UMV.read index (vertexIndex u)
                            if ux > 0
                                then return $! ux
                                else do
                                    nf <- readSTRef free
                                    writeSTRef free $! nf + 1
                                    UMV.write index (vertexIndex u) nf
                                    MV.write queue (nf - 1) d
                                    return $! nf

                    rc <- UMV.new (l + 2 * n)
                    foldM_ (\ !d !i -> do
                            look (opposite d) >>= UMV.write rc i
                            return $! nextDir dir d
                        ) leg [0 .. l - 1]

                    let {-# INLINE lookAndAdd #-}
                        lookAndAdd !d !s = do
                            !c <- look d
                            return $! c + (s `shiftL` 7)

                        bfs !headI = do
                            tailI <- readSTRef free
                            when (headI < tailI - 1) $ do
                                input <- MV.read queue headI
                                !nb <- foldMAdjacentDartsFrom input dir lookAndAdd 0
                                case crossingCodeWithGlobal globalG dir input of
                                    (# be, le #) -> do
                                        let offset = l + headI `shiftL` 1
                                        UMV.write rc offset be
                                        UMV.write rc (offset + 1) (le + nb `shiftL` 3)
                                        bfs (headI + 1)

                    bfs 0
                    tailI <- readSTRef free
                    return $ UMV.take (l + 2 * (tailI - 1)) rc

            internal | UV.length border >= (l + 2 * n)  = []
                     | otherwise                        = sort $ map compCode comps
                where
                    comps = runST $ do
                        visited <- UMV.replicate (n + 1) (-1)
                        let dfs mark v = do
                                vis <- UMV.read visited (vertexIndex v)
                                when (vis < 0) $ do
                                    UMV.write visited (vertexIndex v) mark
                                    forMAdjacentDarts v $ \ !d ->
                                        when (isDart d) (dfs mark $ beginVertex d)

                        forM_ (allLegOpposites tangle) $ \ !d ->
                            when (isDart d) (dfs 0 $ beginVertex d)

                        c <- foldM (\ !ci !v -> do
                                vis <- UMV.read visited (vertexIndex v)
                                if vis < 0
                                    then dfs ci v >> return (ci + 1)
                                    else return ci
                            ) 1 (allVertices tangle)

                        lists <- MV.replicate c []
                        forM_ (allVertices tangle) $ \ !v -> do
                            ci <- UMV.read visited (vertexIndex v)
                            lst <- MV.read lists ci
                            MV.write lists ci (v : lst)

                        mapM (MV.read lists) [1 .. c - 1]

                    compCode comp = minimum $ do
                        start <- comp
                        root <- outcomingDarts start
                        dir <- R.bothDirections
                        globalG <- fromMaybe [D4.i] $ globalTransformations tangle

                        return $! UV.create $ do
                            index <- UMV.replicate (n + 1) 0
                            queue <- MV.new n
                            free <- newSTRef 1

                            let {-# INLINE look #-}
                                look !d = do
                                    let u = beginVertex d
                                    ux <- UMV.read index (vertexIndex u)
                                    if ux > 0
                                        then return $! ux
                                        else do
                                            nf <- readSTRef free
                                            writeSTRef free $! nf + 1
                                            UMV.write index (vertexIndex u) nf
                                            MV.write queue (nf - 1) d
                                            return $! nf

                            rc <- UMV.new (2 * n)
                            void $ look root

                            let {-# INLINE lookAndAdd #-}
                                lookAndAdd !d !s = do
                                    !c <- look d
                                    return $! c + (s `shiftL` 7)

                                bfs !headI = do
                                    tailI <- readSTRef free
                                    when (headI < tailI - 1) $ do
                                        input <- MV.read queue headI
                                        !nb <- foldMAdjacentDartsFrom input dir lookAndAdd 0
                                        case crossingCodeWithGlobal globalG dir input of
                                            (# be, le #) -> do
                                                let offset = headI `shiftL` 1
                                                UMV.write rc offset be
                                                UMV.write rc (offset + 1) (le + nb `shiftL` 3)
                                                bfs (headI + 1)

                            bfs 0
                            tailI <- readSTRef free
                            return $ UMV.take (2 * (tailI - 1)) rc

    isConnected tangle
        | numberOfEdges tangle == 0 && numberOfFreeLoops tangle <= 1  = True
        | numberOfFreeLoops tangle /= 0                               = False
        | otherwise                                                   = all (\ (a, b) -> S.member a con && S.member b con) edges
        where
            edges = allEdges tangle
            con = dfs S.empty $ fst $ head edges
            dfs vis c | S.member c vis  = vis
                      | otherwise       = foldl' dfs (S.insert c vis) neigh
                where
                    neigh | isLeg c    = [opposite c]
                          | otherwise  = [opposite c, nextCCW c, nextCW c]

    type ExplodeType Tangle a = (Int, [(Int, Int)], [([(Int, Int)], a)])

    explode tangle =
        ( numberOfFreeLoops tangle
        , map endPair' $ allLegs tangle
        , map (\ v -> (map endPair' $ outcomingDarts v, vertexCrossing v)) $ allVertices tangle
        )

    implode (loops, brd, list) = runST $ do
        when (loops < 0) $
            error $ printf "Tangle.implode: number of free loops %i is negative" loops

        let l = length brd
        when (odd l) $
            error $ printf "Tangle.implode: number of legs %i must be even" l

        let n = length list
        cr <- PMV.new (4 * n + l)
        st <- MV.new n

        let {-# INLINE write #-}
            write !a !c !p = do
                let b | c == 0 && p >= 0 && p < l  = 4 * n + p
                      | c == 0                     = error $ printf "Tangle.implode: leg index %i is out of bounds [0, %i)" p l
                      | c < 1 || c > n             = error $ printf "Tangle.implode: crossing index %i is out of bounds [1 .. %i]" c n
                      | p < 0 || p > 3             = error $ printf "Tangle.implode: place index %i is out of bounds [0 .. 3]" p
                      | otherwise                  = 4 * (c - 1) + p
                when (a == b) $ error $ printf "Tangle.implode: (%i, %i) connected to itself" c p
                PMV.unsafeWrite cr a b
                when (b < a) $ do
                    x <- PMV.unsafeRead cr b
                    when (x /= a) $ error $ printf "Tangle.implode: (%i, %i) points to unconsistent position" c p

        forM_ (list `zip` [0 ..]) $ \ ((!ns, !cs), !i) -> do
            MV.unsafeWrite st i cs
            case ns of
                [p0, p1, p2, p3] ->
                    forM_ [(p0, 0), (p1, 1), (p2, 2), (p3, 3)] $ \ ((!c, !p), !j) ->
                        write (4 * i + j) c p
                _                ->
                    error $ printf "Tangle.implode: there must be 4 neighbours for every crossing, but found %i for %i-th"
                                        (length ns) (i + 1)

        forM_ (brd `zip` [0 ..]) $ \ ((!c, !p), !i) ->
            write (4 * n + i) c p

        cr' <- PV.unsafeFreeze cr
        st' <- V.unsafeFreeze st

        return Tangle
            { loopsCount      = loops
            , vertexCount     = n
            , involutionArray = cr'
            , crossingsArray  = st'
            , legsCount       = l
            }


instance KnottedPlanar Tangle where
    numberOfFreeLoops = loopsCount

    changeNumberOfFreeLoops loops t | loops >= 0  = t { loopsCount = loops }
                                    | otherwise   = error $ printf "changeNumberOfFreeLoops: number of free loops %i is negative" loops 

    emptyKnotted =
        Tangle
            { loopsCount      = 0
            , vertexCount     = 0
            , involutionArray = PV.empty
            , crossingsArray  = V.empty
            , legsCount       = 0
            }


instance KnottedDiagram Tangle where


instance PlanarAlgebra Tangle where
    numberOfLegs = legsCount

    nthLeg t i | l == 0     = error "nthLeg: tangle has no legs"
               | otherwise  = Dart t (n + i `mod` l)
        where
            l = numberOfLegs t
            n = vertexCount t `shiftL` 2

    allLegs t =
        let n = vertexCount t `shiftL` 2
            l = numberOfLegs t
        in map (Dart t) [n .. n + l - 1]

    legPlace d@(Dart t i) | isDart d   = error $ printf "legPlace: taken from non-leg %s" $ show d
                          | otherwise  = i - 4 * numberOfVertices t

    isLeg = not . isDart


instance TangleLike Tangle where
    zeroTangle =
        Tangle
            { loopsCount      = 0
            , vertexCount     = 0
            , involutionArray = PV.fromList [3, 2, 1, 0]
            , crossingsArray  = V.empty
            , legsCount       = 4
            }

    infinityTangle =
        Tangle
            { loopsCount      = 0
            , vertexCount     = 0
            , involutionArray = PV.fromList [1, 0, 3, 2]
            , crossingsArray  = V.empty
            , legsCount       = 4
            }

    identityTangle =
        Tangle
            { loopsCount      = 0
            , vertexCount     = 0
            , involutionArray = PV.fromList [1, 0]
            , crossingsArray  = V.empty
            , legsCount       = 2
            }

    lonerTangle cr =
        Tangle
            { loopsCount      = 0
            , vertexCount     = 1
            , involutionArray = PV.fromList [4, 5, 6, 7, 0, 1, 2, 3]
            , crossingsArray  = V.singleton cr
            , legsCount       = 4
            }

    rotateTangle !rot tangle
        | l == 0 || rot == 0  = tangle
        | otherwise           =
            tangle
                { involutionArray = PV.create $ do
                    let n = 4 * numberOfVertices tangle
                        a = involutionArray tangle
                        modify i | i < n      = i
                                 | otherwise  = n + mod (i - n + rot) l
                    a' <- PMV.new (n + l)
                    forM_ [0 .. n - 1] $ \ !i ->
                        PMV.unsafeWrite a' i $ modify (a `PV.unsafeIndex` i)
                    forM_ [0 .. l - 1] $ \ !i ->
                        PMV.unsafeWrite a' (n + mod (i + rot) l) $ modify (a `PV.unsafeIndex` (n + i))
                    return a'
                }
        where
            l = numberOfLegs tangle

    mirrorTangleWith f tangle =
        tangle
            { involutionArray = PV.create $ do
                let l = numberOfLegs tangle
                    n = 4 * numberOfVertices tangle
                    a = involutionArray tangle
                    modify i | i < n      = (i .&. complement 3) + ((-i) .&. 3)
                             | otherwise  = n + mod (n - i) l
                a' <- PMV.new (n + l)
                forM_ [0 .. n + l - 1] $ \ !i ->
                    PMV.unsafeWrite a' (modify i) $ modify (a `PV.unsafeIndex` i)
                return a'
            , crossingsArray = f `fmap` crossingsArray tangle
            }

    glueTangles legsToGlue legA legB = runST $ do
        unless (isLeg legA) $
            fail $ printf "glueTangles: first leg parameter %s is not a leg" (show legA)
        unless (isLeg legB) $
            fail $ printf "glueTangles: second leg parameter %s is not a leg" (show legB)

        let tangleA = dartOwner legA
            lA = numberOfLegs tangleA
            nA = numberOfVertices tangleA
            lpA = legPlace legA

            tangleB = dartOwner legB
            lB = numberOfLegs tangleB
            nB = numberOfVertices tangleB
            lpB = legPlace legB

        when (legsToGlue < 0 || legsToGlue > min lA lB) $
            fail $ printf "glueTangles: number of legs to glue %i is out of bound" legsToGlue

        let newL = lA + lB - 2 * legsToGlue
            newC = nA + nB

        visited <- MV.replicate legsToGlue False

        cr <- do
            let {-# INLINE convertA #-}
                convertA !x
                    | x < 4 * nA        = return $! x
                    | ml >= legsToGlue  = return $! 4 * newC + ml - legsToGlue
                    | otherwise         = do
                        MV.unsafeWrite visited ml True
                        convertB (involutionArray tangleB `PV.unsafeIndex` (4 * nB + (lpB - ml) `mod` lB))
                    where
                        ml = (x - 4 * nA - lpA) `mod` lA

                {-# INLINE convertB #-}
                convertB !x
                    | x < 4 * nB            = return $! 4 * nA + x
                    | ml < lB - legsToGlue  = return $! 4 * newC + ml + lA - legsToGlue
                    | otherwise             = do
                        MV.unsafeWrite visited (lB - ml - 1) True
                        convertA (involutionArray tangleA `PV.unsafeIndex` (4 * nA + (lpA + lB - ml - 1) `mod` lA))
                    where
                        ml = (x - 4 * nB - lpB - 1) `mod` lB

            cr <- PMV.new (4 * newC + newL)
            forM_ [0 .. 4 * nA - 1] $ \ !i ->
                convertA (involutionArray tangleA `PV.unsafeIndex` i)
                    >>= PMV.unsafeWrite cr i

            forM_ [0 .. 4 * nB - 1] $ \ !i ->
                convertB (involutionArray tangleB `PV.unsafeIndex` i)
                    >>= PMV.unsafeWrite cr (4 * nA + i)

            forM_ [0 .. lA - legsToGlue - 1] $ \ !i ->
                convertA (involutionArray tangleA `PV.unsafeIndex` (4 * nA + (lpA + legsToGlue + i) `mod` lA))
                    >>= PMV.unsafeWrite cr (4 * newC + i)

            forM_ [0 .. lB - legsToGlue - 1] $ \ !i ->
                convertB (involutionArray tangleB `PV.unsafeIndex` (4 * nB + (lpB + 1 + i) `mod` lB))
                    >>= PMV.unsafeWrite cr (4 * newC + lA - legsToGlue + i) 
            PV.unsafeFreeze cr

        extraLoops <- do
            let markA a = do
                    let ai = 4 * nA + (lpA + a) `mod` lA
                        bi = involutionArray tangleA `PV.unsafeIndex` ai
                        b = (bi - 4 * nA - lpA) `mod` lA
                    v <- MV.unsafeRead visited b
                    unless v $ MV.unsafeWrite visited b True >> markB b

                markB a = do
                    let ai = 4 * nB + (lpB - a) `mod` lB
                        bi = involutionArray tangleB `PV.unsafeIndex` ai
                        b = (lpB - (bi - 4 * nB)) `mod` lB
                    v <- MV.unsafeRead visited b
                    unless v $ MV.unsafeWrite visited b True >> markA b

            foldM (\ !s !i -> do
                    v <- MV.unsafeRead visited i
                    if v
                        then return $! s
                        else markA i >> (return $! s + 1)
                ) 0 [0 .. legsToGlue - 1]

        return Tangle
            { loopsCount      = numberOfFreeLoops tangleA + numberOfFreeLoops tangleB + extraLoops
            , vertexCount     = newC
            , involutionArray = cr
            , crossingsArray  = crossingsArray tangleA V.++ crossingsArray tangleB
            , legsCount       = newL
            }

    glueToBorder leg legsToGlue !crossingToGlue = runST $ do
        unless (isLeg leg) $
            fail $ printf "glueToBorder: leg expected, but %s received" (show leg)

        when (legsToGlue < 0 || legsToGlue > 4) $
            fail $ printf "glueToBorder: legsToGlue must be in [0 .. 4], but %i found" legsToGlue

        let tangle = dartOwner leg
            oldL = numberOfLegs tangle
        when (oldL < legsToGlue) $
            fail $ printf "glueToBorder: not enough legs to glue (l = %i, legsToGlue = %i)" oldL legsToGlue

        let oldC = numberOfVertices tangle
            newC = oldC + 1
            newL = oldL + 4 - 2 * legsToGlue
            lp = legPlace leg 

        let result = Tangle
                { loopsCount      = numberOfFreeLoops tangle
                , vertexCount     = newC
                , involutionArray = PV.create $ do
                    cr <- PMV.new (4 * newC + newL)

                    let {-# INLINE copyModified #-}
                        copyModified !index !index' =
                            let y | x < 4 * oldC            = x
                                  | ml < oldL - legsToGlue  = 4 * newC + 4 - legsToGlue + ml
                                  | otherwise               = 4 * newC - 5 + oldL - ml
                                  where
                                      x = involutionArray tangle `PV.unsafeIndex` index'
                                      ml = (x - 4 * oldC - lp - 1) `mod` oldL
                            in PMV.unsafeWrite cr index y

                    forM_ [0 .. 4 * oldC - 1] $ \ !i ->
                        copyModified i i

                    forM_ [0 .. legsToGlue - 1] $ \ !i ->
                        copyModified (4 * (newC - 1) + i) (4 * oldC + ((lp - i) `mod` oldL))

                    forM_ [0 .. 3 - legsToGlue] $ \ !i -> do
                        let a = 4 * (newC - 1) + legsToGlue + i
                            b = 4 * newC + i
                        PMV.unsafeWrite cr a b
                        PMV.unsafeWrite cr b a

                    forM_ [0 .. oldL - 1 - legsToGlue] $ \ !i ->
                        copyModified (4 * newC + i + 4 - legsToGlue) (4 * oldC + ((lp + 1 + i) `mod` oldL))

                    return $! cr

                , crossingsArray  = V.snoc (crossingsArray tangle) crossingToGlue
                , legsCount       = newL
                }

        return $! nthVertex result newC 

    tensorSubst k crossF tangle = implode (k * numberOfFreeLoops tangle, border, body)
        where
            n = numberOfVertices tangle

            crossSubst =
                let substList = do
                        c <- allVertices tangle
                        let t = crossF c
                        when (numberOfLegs t /= 4 * k) $
                            fail "bad number of legs"
                        return $! t
                in V.fromListN (n + 1) $ undefined : substList

            crossOffset = UV.fromListN (n + 1) $
                0 : scanl (\ !p !i -> p + numberOfVertices (crossSubst V.! i)) 0 [1 .. n]

            resolveInCrossing !v !d
                | isLeg d    =
                    let p = legPlace d
                    in resolveOutside (opposite $ nthOutcomingDart v $ p `div` k) (p `mod` k)
                | otherwise  =
                    let (c, p) = beginPair' d
                    in ((crossOffset UV.! vertexIndex v) + c, p)

            resolveOutside !d !i
                | isLeg d    = (0, k * legPlace d + i)
                | otherwise  =
                    let (c, p) = beginPair d
                    in resolveInCrossing c $ opposite $
                            nthLeg (crossSubst V.! vertexIndex c) (k * p + k - 1 - i)

            border = do
                d <- allLegOpposites tangle
                i <- [0 .. k - 1]
                return $! resolveOutside d $ k - 1 - i

            body = do
                c <- allVertices tangle
                let t = crossSubst V.! vertexIndex c
                c' <- allVertices t
                return (map (resolveInCrossing c) $ incomingDarts c', vertexCrossing c')


instance (Show a) => Show (Tangle a) where
    show = printf "implode %s" . show . explode


instance (Show a) => Show (Vertex Tangle a) where
    show v =
        printf "(Crossing %i %s [ %s ])"
            (vertexIndex v)
            (show $ vertexCrossing v)
            (unwords $ map (show . opposite) $ outcomingDarts v)


instance Show (Dart Tangle a) where
    show d | isLeg d    = printf "(Leg %i)" $ legPlace d
           | otherwise  = let (c, p) = beginPair' d
                          in printf "(Dart %i %i)" c p


instance KnottedWithPrimeTest Tangle where
    isPrime tangle = connections == nub connections
        where
            idm = let faces = directedPathsDecomposition (nextCW, nextCCW)
                  in M.fromList $ concatMap (\ (face, i) -> zip face $ repeat i) $ zip faces [(0 :: Int) ..]

            connections =
                let getPair (da, db) =
                        let a = idm M.! da
                            b = idm M.! db
                        in (min a b, max a b)
                in sort $ map getPair $ allEdges tangle

            directedPathsDecomposition continue =
                let processDart (paths, s) d
                        | S.member d s  = (paths, s)
                        | otherwise     = (path : paths, nextS)
                        where
                            path = containingDirectedPath continue d
                            nextS = foldl' (flip S.insert) s path
                in fst $ foldl' processDart ([], S.empty) $ allHalfEdges tangle

            containingDirectedPath (adjForward, adjBackward) start
                | isCycle    = forward
                | otherwise  = walkBackward (start, forward)
                where
                    (forward, isCycle) = walkForward start

                    walkForward d
                        | isLeg opp     = ([d], False)
                        | start == nxt  = ([d], True)
                        | otherwise     = (d : nextPath, nextCycle)
                        where
                            opp = opposite d
                            nxt = adjForward opp
                            (nextPath, nextCycle) = walkForward nxt

                    walkBackward (d, path)
                        | isLeg d    = path
                        | otherwise  = let prev = opposite $ adjBackward d in walkBackward (prev, prev : path)


type TangleProjection = Tangle ProjectionCrossing
type TangleProjectionVertex = Vertex Tangle ProjectionCrossing
type TangleProjectionDart = Dart Tangle ProjectionCrossing


type TangleDiagram = Tangle DiagramCrossing
type TangleDiagramVertex = Vertex Tangle DiagramCrossing
type TangleDiagramDart = Dart Tangle DiagramCrossing
