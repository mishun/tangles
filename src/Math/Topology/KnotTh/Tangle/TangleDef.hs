{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TypeFamilies, UnboxedTuples #-}
module Math.Topology.KnotTh.Tangle.TangleDef
    ( Tangle
    , AsTangle(..)
    , Tangle0
    , Tangle2
    , Tangle4
    , Tangle6
    , Surgery(..)
    , TensorSurgery(..)

    , glueToBorder
    , emptyTangle
    , emptyPropagatorTangle
    , lonerPropagatorTangle
    , loopTangle
    , zeroTangle
    , infinityTangle
    , lonerTangle
    , lonerProjection
    , lonerOverCrossing
    , lonerUnderCrossing
    , chainTangle
    , zipTangles
    , zipTangles2
    , zipTangles4
    , zipTangles6
    , conwaySum

    , tangle0
    , tangle2
    , tangle4
    , tangle6
    ) where

import Control.Applicative (Applicative)
import Control.DeepSeq (NFData(..))
import Control.Monad (void, forM, forM_, when, foldM_, foldM, filterM, (>=>), guard)
import Control.Monad.IfElse (unlessM)
import qualified Control.Monad.ST as ST
import qualified Control.Monad.Reader as Reader
import Data.Bits ((.&.), complement, shiftL, shiftR)
import Data.List (nub, sort, foldl', find)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Primitive.Mutable as PMV
import Text.Printf
import Math.Topology.KnotTh.Algebra.Dihedral.D4
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Crossings.Projection
import Math.Topology.KnotTh.Knotted.Crossings.Diagram
import Math.Topology.KnotTh.Knotted.Threads
import Math.Topology.KnotTh.Moves.ModifyDSL


data Tangle a =
    Tangle
        { loopsN   :: {-# UNPACK #-} !Int
        , vertexN  :: {-# UNPACK #-} !Int
        , involArr :: {-# UNPACK #-} !(PV.Vector Int)
        , crossArr :: {-# UNPACK #-} !(V.Vector a)
        , legsN    :: {-# UNPACK #-} !Int
        }
    deriving (Functor)

class AsTangle t where
    toTangle :: t a -> Tangle a

instance AsTangle Tangle where
    toTangle t = t


newtype Tangle0 a = T0 (Tangle a)
    deriving (Show, Functor, NFData, MirrorAction, TransposeAction, DartDiagram, VertexDiagram, Knotted, KnottedDiagram, Surgery)

instance AsTangle Tangle0 where
    toTangle (T0 t) = t

instance DartDiagram' Tangle0 where
    newtype Dart Tangle0 a = D0 (Dart Tangle a)

instance VertexDiagram' Tangle0 where
    newtype Vertex Tangle0 a = V0 (Vertex Tangle a)

instance TensorSurgery Tangle0 where
    tensorSurgery k (T0 t) = T0 $ tensorSurgery k t

instance ExplodeKnotted Tangle0 where
    type ExplodeType Tangle0 a = (Int, [([(Int, Int)], a)])
    explode (T0 t) = let (f, [], l) = explode t in (f, l)
    implode (f, l) = T0 (implode (f, [], l))

instance (Crossing a) => KnotWithPrimeTest Tangle0 a where
    isPrime (T0 t) = isPrime t

instance (Show a) => Show (Vertex Tangle0 a) where
    show (V0 v) = show v

instance Show (Dart Tangle0 a) where
    show (D0 d) = show d


newtype Tangle2 a = T2 (Tangle a)
    deriving (Show, Functor, NFData, RotationAction, MirrorAction, TransposeAction, DartDiagram, LeggedDiagram, VertexDiagram, Knotted, Surgery)

instance AsTangle Tangle2 where
    toTangle (T2 t) = t

instance DartDiagram' Tangle2 where
    newtype Dart Tangle2 a = D2 (Dart Tangle a)

instance VertexDiagram' Tangle2 where
    newtype Vertex Tangle2 a = V2 (Vertex Tangle a)

instance (Show a) => Show (Vertex Tangle2 a) where
    show (V2 v) = show v

instance Show (Dart Tangle2 a) where
    show (D2 d) = show d


newtype Tangle4 a = T4 (Tangle a)
    deriving (Show, Functor, NFData, RotationAction, MirrorAction, TransposeAction, DartDiagram, LeggedDiagram, VertexDiagram, Knotted, Surgery)

instance AsTangle Tangle4 where
    toTangle (T4 t) = t

instance DartDiagram' Tangle4 where
    newtype Dart Tangle4 a = D4 (Dart Tangle a)

instance VertexDiagram' Tangle4 where
    newtype Vertex Tangle4 a = V4 (Vertex Tangle a)

instance (Show a) => Show (Vertex Tangle4 a) where
    show (V4 v) = show v

instance Show (Dart Tangle4 a) where
    show (D4 d) = show d

instance (MirrorAction a) => GroupAction D4 (Tangle4 a) where
    transform g t | reflection g  = mirrorIt $ rotateBy (rotation g) t
                  | otherwise     = rotateBy (rotation g) t


newtype Tangle6 a = T6 (Tangle a)
    deriving (Show, Functor, NFData, RotationAction, MirrorAction, TransposeAction, DartDiagram, LeggedDiagram, VertexDiagram, Knotted, Surgery)

instance AsTangle Tangle6 where
    toTangle (T6 t) = t

instance DartDiagram' Tangle6 where
    newtype Dart Tangle6 a = D6 (Dart Tangle a)

instance VertexDiagram' Tangle6 where
    newtype Vertex Tangle6 a = V6 (Vertex Tangle a)

instance (Show a) => Show (Vertex Tangle6 a) where
    show (V6 v) = show v

instance Show (Dart Tangle6 a) where
    show (D6 d) = show d


instance (NFData a) => NFData (Tangle a) where
    rnf t = rnf (crossArr t) `seq` t `seq` ()

instance (Show a) => Show (Tangle a) where
    show = printf "implode %s" . show . explode

instance RotationAction (Tangle a) where
    rotationOrder = legsN

    rotateByUnchecked !rot tangle =
        tangle
            { involArr = PV.create $ do
                let l = legsN tangle
                    n = 4 * vertexN tangle
                    a = involArr tangle
                    modify i | i < n      = i
                             | otherwise  = n + mod (i - n + rot) l
                a' <- PMV.new (n + l)
                forM_ [0 .. n - 1] $ \ !i ->
                    PMV.unsafeWrite a' i $ modify (a `PV.unsafeIndex` i)
                forM_ [0 .. l - 1] $ \ !i ->
                    PMV.unsafeWrite a' (n + mod (i + rot) l) $ modify (a `PV.unsafeIndex` (n + i))
                return a'
            }

instance (MirrorAction a) => MirrorAction (Tangle a) where
    mirrorIt tangle =
        tangle
            { involArr = PV.create $ do
                let l = legsN tangle
                    n = 4 * vertexN tangle
                    a = involArr tangle
                    modify i | i < n      = (i .&. complement 3) + ((-i) .&. 3)
                             | otherwise  = n + mod (n - i) l
                a' <- PMV.new (n + l)
                forM_ [0 .. n + l - 1] $ \ !i ->
                    PMV.unsafeWrite a' (modify i) $ modify (a `PV.unsafeIndex` i)
                return a'

            , crossArr = mirrorIt `fmap` crossArr tangle
            }

instance (TransposeAction a) => TransposeAction (Tangle a) where
    transposeIt = fmap transposeIt

instance DartDiagram' Tangle where
    data Dart Tangle a = Dart !(Tangle a) {-# UNPACK #-} !Int

instance (NFData a) => NFData (Dart Tangle a)

instance DartDiagram Tangle where
    dartOwner (Dart t _) = t
    dartIndex (Dart _ i) = i

    opposite (Dart t d) = Dart t (involArr t `PV.unsafeIndex` d)

    nextCCW (Dart t d) | d >= n     = Dart t (n + (d - n + 1) `mod` legsN t)
                       | otherwise  = Dart t ((d .&. complement 3) + ((d + 1) .&. 3))
        where n = vertexN t `shiftL` 2

    nextCW (Dart t d) | d >= n     = Dart t (n + (d - n - 1) `mod` legsN t)
                      | otherwise  = Dart t ((d .&. complement 3) + ((d - 1) .&. 3))
        where n = vertexN t `shiftL` 2

    nextBy delta (Dart t d) | d >= n     = Dart t (n + (d - n + delta) `mod` legsN t)
                            | otherwise  = Dart t ((d .&. complement 3) + ((d + delta) .&. 3))
        where n = vertexN t `shiftL` 2

    numberOfDarts t = PV.length (involArr t)
    numberOfEdges t = PV.length (involArr t) `shiftR` 1

    nthDart t i | i < 0 || i >= b  = error $ printf "Tangle.nthDart: index %i is out of bounds [0, %i)" i b
                | otherwise        = Dart t i
        where b = PV.length (involArr t)

    allDarts t = map (Dart t) [0 .. PV.length (involArr t) - 1]

    allEdges t =
        foldl' (\ !es !i ->
                let j = involArr t `PV.unsafeIndex` i
                in if i < j
                    then (Dart t i, Dart t j) : es
                    else es
            ) [] [0 .. PV.length (involArr t) - 1]

    dartIndicesRange t = (0, numberOfDarts t - 1)

instance LeggedDiagram Tangle where
    numberOfLegs = legsN

    nthLeg t i | l == 0     = error "Tangle.nthLeg: tangle has no legs"
               | otherwise  = Dart t (n + i `mod` l)
        where
            l = legsN t
            n = vertexN t `shiftL` 2

    allLegs t =
        let n = vertexN t `shiftL` 2
            l = legsN t
        in map (Dart t) [n .. n + l - 1]

    legPlace d@(Dart t i) | isLeg d    = i - 4 * vertexN t
                          | otherwise  = error $ printf "Tangle.legPlace: taken from non-leg %s" $ show d

    isLeg (Dart t i) = i >= (vertexN t `shiftL` 2)

instance VertexDiagram' Tangle where
    data Vertex Tangle a = Vertex !(Tangle a) {-# UNPACK #-} !Int

instance (NFData a) => NFData (Vertex Tangle a)

instance VertexDiagram Tangle where
    vertexContent (Vertex t i) = crossArr t `V.unsafeIndex` i

    mapVertices f t =
        t { crossArr =
                V.generate (numberOfVertices t) $ \ !i ->
                    f (nthVertex t $! i + 1)
          }

    vertexOwner (Vertex t _) = t
    vertexIndex (Vertex _ i) = i + 1

    vertexDegree _ = 4

    numberOfVertices = vertexN

    nthVertex t i | i < 1 || i > b  = error $ printf "Tangle.nthVertex: index %i is out of bounds [1, %i]" i b
                  | otherwise       = Vertex t (i - 1)
        where b = numberOfVertices t

    allVertices t = map (Vertex t) [0 .. numberOfVertices t - 1]

    nthOutcomingDart (Vertex t c) i = Dart t ((c `shiftL` 2) + (i .&. 3))

    outcomingDarts c = map (nthOutcomingDart c) [0 .. 3]

    maybeBeginVertex (Dart t d) | d >= n     = Nothing
                                | otherwise  = Just $! Vertex t (d `shiftR` 2)
        where n = vertexN t `shiftL` 2

    beginVertex (Dart t d) | d >= n     = error $ printf "Tangle.beginVertex: taken from %i-th leg" (d - n)
                           | otherwise  = Vertex t (d `shiftR` 2)
        where n = vertexN t `shiftL` 2

    beginPlace (Dart t d) | d >= n     = error $ printf "Tangle.beginPlace: taken from %i-th leg" (d - n)
                          | otherwise  = d .&. 3
        where n = vertexN t `shiftL` 2

    beginPair' d | isDart d   = (vertexIndex $ beginVertex d, beginPlace d)
                 | otherwise  = (0, legPlace d)

    isDart (Dart t i) = i < (vertexN t `shiftL` 2)

    vertexIndicesRange t = (1, numberOfVertices t)

instance Knotted Tangle where
    unrootedHomeomorphismInvariant tangle
        | n > 127    = error $ printf "Tangle.unrootedHomeomorphismInvariant: too many crossings (%i)" n
        | otherwise  = UV.concat $ UV.singleton (numberOfFreeLoops tangle) : UV.singleton (numberOfLegs tangle) : border : internal
        where
            n = numberOfVertices tangle
            l = numberOfLegs tangle

            border | l == 0    = UV.empty
                   | otherwise = minimum $ do
                baseLeg <- allLegs tangle
                dir <- bothDirections
                globalG <- fromMaybe [d4I] $ globalTransformations tangle

                return $! UV.create $ do
                    index <- UMV.replicate (n + 1) 0
                    incoming <- UMV.replicate (n + 1) 0
                    queue <- MV.new n
                    free <- newSTRef 1

                    let {-# INLINE look #-}
                        look !d | isLeg d    =
                                    let offset = (legPlace d - legPlace baseLeg) * directionSign dir
                                    in return $! -(offset `mod` l)

                                | otherwise  = do
                                    let (u, p) = beginPair' d
                                    ux <- UMV.read index u
                                    if ux > 0
                                        then do
                                            base <- UMV.unsafeRead incoming u
                                            return $! (ux `shiftL` 2) + (((p - base) * directionSign dir) .&. 3)
                                        else do
                                            nf <- readSTRef free
                                            writeSTRef free $! nf + 1
                                            UMV.write index u nf
                                            UMV.unsafeWrite incoming u p
                                            MV.write queue (nf - 1) d
                                            return $! nf `shiftL` 2

                    rc <- UMV.new (l + 5 * n)
                    foldM_ (\ !d !i -> do
                            look (opposite d) >>= UMV.write rc i
                            return $! nextDir dir d
                        ) baseLeg [0 .. l - 1]

                    let bfs !headI !offset = do
                            tailI <- readSTRef free
                            if headI >= tailI - 1
                                then return $ UMV.take offset rc
                                else do
                                    input <- MV.read queue headI
                                    case crossingCodeWithGlobal globalG dir input of
                                        (# be, le #) -> UMV.write rc offset $! (be `shiftL` 3) + le
                                    foldMIncomingDartsFrom input dir (\ !d !i -> do
                                            look d >>= UMV.write rc i
                                            return $! i + 1
                                        ) (offset + 1) >>= bfs (headI + 1)

                    bfs 0 l

            internal | UV.length border >= (l + 2 * n)  = []
                     | otherwise                        = sort $ map compCode comps
                where
                    comps = ST.runST $ do
                        visited <- UMV.replicate (n + 1) (-1)
                        let dfs mark v = do
                                vis <- UMV.read visited (vertexIndex v)
                                when (vis < 0) $ do
                                    UMV.write visited (vertexIndex v) mark
                                    forMIncomingDarts v $ \ !d ->
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
                        dir <- bothDirections
                        globalG <- fromMaybe [d4I] $ globalTransformations tangle

                        return $! UV.create $ do
                            index <- UMV.replicate (n + 1) 0
                            incoming <- UMV.replicate (n + 1) 0
                            queue <- MV.new n
                            free <- newSTRef 1

                            let {-# INLINE look #-}
                                look !d = do
                                    let (u, p) = beginPair' d
                                    ux <- UMV.read index u
                                    if ux > 0
                                        then do
                                            base <- UMV.unsafeRead incoming u
                                            return $! (ux `shiftL` 2) + (((p - base) * directionSign dir) .&. 3)
                                        else do
                                            nf <- readSTRef free
                                            writeSTRef free $! nf + 1
                                            UMV.write index u nf
                                            UMV.unsafeWrite incoming u p
                                            MV.write queue (nf - 1) d
                                            return $! nf `shiftL` 2

                            rc <- UMV.new (5 * n)
                            void $ look root

                            let bfs !headI !offset = do
                                    tailI <- readSTRef free
                                    if headI >= tailI - 1
                                        then return $! UMV.take offset rc
                                        else do
                                            input <- MV.read queue headI
                                            case crossingCodeWithGlobal globalG dir input of
                                                (# be, le #) -> UMV.write rc offset $! (be `shiftL` 3) + le
                                            foldMIncomingDartsFrom input dir (\ !d !i -> do
                                                    look d >>= UMV.write rc i
                                                    return $! i + 1
                                                ) (offset + 1) >>= bfs (headI + 1)

                            bfs 0 0

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

    numberOfFreeLoops = loopsN

    changeNumberOfFreeLoops loops t | loops >= 0  = t { loopsN = loops }
                                    | otherwise   = error $ printf "Tangle.changeNumberOfFreeLoops: number of free loops %i is negative" loops

instance ExplodeKnotted Tangle where
    type ExplodeType Tangle a = (Int, [(Int, Int)], [([(Int, Int)], a)])

    explode tangle =
        ( numberOfFreeLoops tangle
        , map endPair' $ allLegs tangle
        , map (\ v -> (map endPair' $ outcomingDarts v, vertexContent v)) $ allVertices tangle
        )

    implode (loops, brd, list) = ST.runST $ do
        when (loops < 0) $
            error $ printf "Tangle.implode: number of free loops %i is negative" loops

        let l = length brd
        when (odd l) $
            error $ printf "Tangle.implode: number of legs %i must be even" l

        let n = length list
        inv <- PMV.new (4 * n + l)
        st <- MV.new n

        let {-# INLINE write #-}
            write !a !c !p = do
                let b | c == 0 && p >= 0 && p < l  = 4 * n + p
                      | c == 0                     = error $ printf "Tangle.implode: leg index %i is out of bounds [0, %i)" p l
                      | c < 1 || c > n             = error $ printf "Tangle.implode: crossing index %i is out of bounds [1 .. %i]" c n
                      | p < 0 || p > 3             = error $ printf "Tangle.implode: place index %i is out of bounds [0 .. 3]" p
                      | otherwise                  = 4 * (c - 1) + p
                when (a == b) $ error $ printf "Tangle.implode: (%i, %i) connected to itself" c p
                PMV.unsafeWrite inv a b
                when (b < a) $ do
                    x <- PMV.unsafeRead inv b
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

        inv' <- PV.unsafeFreeze inv
        st' <- V.unsafeFreeze st

        return Tangle
            { loopsN   = loops
            , vertexN  = n
            , involArr = inv'
            , crossArr = st'
            , legsN    = l
            }

instance Show (Dart Tangle a) where
    show d | isLeg d    = printf "(Leg %i)" $ legPlace d
           | otherwise  = let (c, p) = beginPair' d
                          in printf "(Dart %i %i)" c p

instance (Show a) => Show (Vertex Tangle a) where
    show v =
        printf "(Crossing %i %s [ %s ])"
            (vertexIndex v)
            (show $ vertexContent v)
            (unwords $ map (show . opposite) $ outcomingDarts v)

instance PlanarAlgebra (Tangle a) where
    planarDegree = numberOfLegs

    planarEmpty = toTangle emptyTangle

    planarLoop = toTangle (loopTangle 1)

    planarPropagator n | n < 0      = error $ printf "Tangle.planarPropagator: parameter must be non-negative, but %i passed" n
                       | otherwise  =
        Tangle
            { loopsN   = 0
            , vertexN  = 0
            , involArr = PV.generate (2 * n) (\ i -> 2 * n - 1 - i)
            , crossArr = V.empty
            , legsN    = 2 * n
            }

    horizontalCompositionUnchecked gl (!tangleA, !posA) (!tangleB, !posB) =
        ST.runST $ do
            let legsA = numberOfLegs tangleA
                legsB = numberOfLegs tangleB
            when (gl < 0 || gl > min legsA legsB) $
                fail $ printf "Tangle.horizontalComposition: number of legs to glue %i is out of bound" gl

            let nA = numberOfVertices tangleA
                nB = numberOfVertices tangleB
                newL = legsA + legsB - 2 * gl
                newC = nA + nB

            visited <- UMV.replicate gl False
            inv <- do
                let {-# INLINE convertA #-}
                    convertA !x | x < 4 * nA  = return $! x
                                | ml >= gl    = return $! 4 * newC + ml - gl
                                | otherwise   = do
                                    UMV.unsafeWrite visited ml True
                                    let ml' = (4 * nB) + ((posB + gl - 1 - ml) `mod` legsB)
                                    convertB $! involArr tangleB `PV.unsafeIndex` ml'
                        where ml = (x - 4 * nA - posA) `mod` legsA

                    {-# INLINE convertB #-}
                    convertB !x | x < 4 * nB  = return $! (4 * nA) + x
                                | ml >= gl    = return $! (4 * newC) + (legsA - gl) + (ml - gl)
                                | otherwise   = do
                                    UMV.unsafeWrite visited (gl - 1 - ml) True
                                    let ml' = (4 * nA) + ((posA + gl - 1 - ml) `mod` legsA)
                                    convertA $! involArr tangleA `PV.unsafeIndex` ml'
                        where ml = (x - 4 * nB - posB) `mod` legsB

                cr <- PMV.new (4 * newC + newL)
                forM_ [0 .. 4 * nA - 1] $ \ !i ->
                    convertA (involArr tangleA `PV.unsafeIndex` i)
                        >>= PMV.unsafeWrite cr i
                forM_ [0 .. 4 * nB - 1] $ \ !i ->
                    convertB (involArr tangleB `PV.unsafeIndex` i)
                        >>= PMV.unsafeWrite cr (4 * nA + i)

                forM_ [0 .. legsA - gl - 1] $ \ !i ->
                    let i' = (4 * nA) + (posA + gl + i) `mod` legsA
                        j = (4 * newC) + i
                    in convertA (involArr tangleA `PV.unsafeIndex` i') >>= PMV.unsafeWrite cr j
                forM_ [0 .. legsB - gl - 1] $ \ !i ->
                    let i' = (4 * nB) + (posB + gl + i) `mod` legsB
                        j = (4 * newC) + (legsA - gl) + i
                    in convertB (involArr tangleB `PV.unsafeIndex` i') >>= PMV.unsafeWrite cr j

                PV.unsafeFreeze cr

            extraLoops <- do
                let markA !x =
                        unlessM (UMV.unsafeRead visited x) $ do
                            UMV.unsafeWrite visited x True
                            let xi = (4 * nA) + (posA + x) `mod` legsA
                                yi = involArr tangleA `PV.unsafeIndex` xi
                            markB $ (yi - (4 * nA) - posA) `mod` legsA

                    markB !x =
                        unlessM (UMV.unsafeRead visited x) $ do
                            UMV.unsafeWrite visited x True
                            let xi = (4 * nB) + (posB + gl - 1 - x) `mod` legsB
                                yi = involArr tangleB `PV.unsafeIndex` xi
                            markA $ gl - 1 - ((yi - (4 * nB) - posB) `mod` legsB)

                foldM (\ !s !i -> do
                        vis <- UMV.unsafeRead visited i
                        if vis then return $! s
                               else markA i >> (return $! s + 1)
                    ) 0 [0 .. gl - 1]

            return $!
                Tangle
                    { loopsN      = loopsN tangleA + loopsN tangleB + extraLoops
                    , vertexN     = newC
                    , involArr = inv
                    , crossArr = crossArr tangleA V.++ crossArr tangleB
                    , legsN       = newL
                    }

instance KnottedDiagram Tangle where
    isReidemeisterReducible =
        any (\ ab ->
                let ba = opposite ab
                    ac = nextCCW ab
                in (ac == ba) || (isDart ba && isPassingOver ab == isPassingOver ba && opposite ac == nextCW ba)
            ) . allOutcomingDarts

    tryReduceReidemeisterI tangle =  do
        d <- find (\ d -> opposite d == nextCCW d) (allOutcomingDarts tangle)
        return $! modifyKnot tangle $ do
            let ac = nextCW d
                ab = nextCW ac
                ba = opposite ab
            substituteC [(ba, ac)]
            maskC [beginVertex d]

    tryReduceReidemeisterII tangle = do
        abl <- find (\ abl ->
                let bal = opposite abl
                    abr = nextCCW abl
                in isDart bal && isPassingOver abl == isPassingOver bal && opposite abr == nextCW bal
            ) (allOutcomingDarts tangle)

        return $! modifyKnot tangle $ do
            let bal = opposite abl

                ap = threadContinuation abl
                aq = nextCW abl
                br = nextCCW bal
                bs = threadContinuation bal

                pa = opposite ap
                qa = opposite aq
                rb = opposite br
                sb = opposite bs

            if qa == ap || rb == bs
                then if qa == ap && rb == bs
                    then emitLoopsC 1
                    else do
                        when (qa /= ap) $ connectC [(pa, qa)]
                        when (rb /= bs) $ connectC [(rb, sb)]
                else do
                    if qa == br
                        then emitLoopsC 1
                        else connectC [(qa, rb)]

                    if pa == bs
                        then emitLoopsC 1
                        else connectC [(pa, sb)]

            maskC [beginVertex abl, beginVertex bal]

    reidemeisterIII tangle = do
        ab <- allOutcomingDarts tangle

        -- \sc           /rb             \sc   /rb
        --  \           /                 \   /
        -- cs\ cb   bc /br               ac\ /ab
        -- ---------------                  /
        --   ca\c   b/ba                 ap/a\aq
        --      \   /         -->         /   \
        --     ac\ /ab                 cs/c   b\br
        --        /                  ---------------
        --     ap/a\aq               ca/ cb   bc \ba
        --      /   \                 /           \
        --   pa/     \qa             /pa           \qa
        guard $ isDart ab

        let ac = nextCCW ab
            ba = opposite ab
            ca = opposite ac

        guard $ isDart ba && isDart ca

        let bc = nextCW ba
            cb = nextCCW ca

        guard $ bc == opposite cb

        let a = beginVertex ab
            b = beginVertex ba
            c = beginVertex ca

        guard $ (a /= b) && (a /= c) && (b /= c)
        guard $ isPassingOver bc == isPassingOver cb

        guard $ let altRoot | isPassingOver ab == isPassingOver ba  = ca
                            | otherwise                             = bc
                in ab < altRoot

        let ap = threadContinuation ab
            aq = nextCW ab
            br = nextCW bc
            cs = nextCCW cb

        return $! modifyKnot tangle $ do
            substituteC [(ca, ap), (ba, aq), (ab, br), (ac, cs)]
            connectC [(br, aq), (cs, ap)]

instance (Crossing a) => KnotWithPrimeTest Tangle a where
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
                in fst $ foldl' processDart ([], S.empty) $ allDarts tangle

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


class (Knotted k) => Surgery k where
    surgery      :: Tangle4 a -> Vertex k a -> k a
    multiSurgery :: k (Tangle4 a) -> k a

instance Surgery Tangle where
    surgery (T4 sub) v =
        ST.runST $ do
            let tangle = vertexOwner v
                legs = legsN tangle
                nEx = vertexN tangle
                nIn = vertexN sub
                idx = vertexIndex v - 1
                newC = nEx + nIn - 1

            visited <- UMV.replicate 4 False
            inv <- do
                let convertExt !x | x >= 4 * nEx        = return $! 4 * (nIn - 1) + x
                                  | x >= 4 * (idx + 1)  = return $! x - 4
                                  | x >= 4 * idx        = do
                                      let t = x .&. 3
                                      UMV.unsafeWrite visited t True
                                      convertInt $! involArr sub `PV.unsafeIndex` ((4 * nIn) + t)
                                  | otherwise           = return $! x

                    convertInt !x | x >= 4 * nIn  = do
                                      let t = x .&. 3
                                      UMV.unsafeWrite visited t True
                                      convertExt $! involArr tangle `PV.unsafeIndex` ((4 * idx) + t)
                                  | otherwise     = return $! 4 * (nEx - 1) + x

                cr <- PMV.new (4 * newC + legs)
                forM_ [0 .. 4 * idx - 1] $ \ !i ->
                    convertExt (involArr tangle `PV.unsafeIndex` i)
                        >>= PMV.unsafeWrite cr i
                forM_ [4 * (idx + 1) .. 4 * nEx - 1] $ \ !i ->
                    convertExt (involArr tangle `PV.unsafeIndex` i)
                        >>= PMV.unsafeWrite cr (i - 4)
                forM_ [0 .. 4 * nIn - 1] $ \ !i ->
                    convertInt (involArr sub `PV.unsafeIndex` i)
                        >>= PMV.unsafeWrite cr (i + 4 * nEx - 4)
                forM_ [0 .. legs - 1] $ \ !leg ->
                    convertExt (involArr sub `PV.unsafeIndex` ((4 * nEx) + leg))
                        >>= PMV.unsafeWrite cr ((4 * newC) + leg)
                PV.unsafeFreeze cr

            extraLoops <- do
                let markExt !x =
                        unlessM (UMV.unsafeRead visited x) $ do
                            UMV.unsafeWrite visited x True
                            markInt $ (involArr tangle `PV.unsafeIndex` (4 * idx + x)) .&. 3

                    markInt !x =
                        unlessM (UMV.unsafeRead visited x) $ do
                            UMV.unsafeWrite visited x True
                            markExt $ (involArr sub `PV.unsafeIndex` (4 * nIn + x)) .&. 3

                foldM (\ !s !i -> do
                        vis <- UMV.unsafeRead visited i
                        if vis then return $! s
                               else markExt i >> (return $! s + 1)
                    ) 0 [0 .. 3]

            return $!
                Tangle
                    { loopsN   = loopsN tangle + loopsN sub + extraLoops
                    , vertexN  = newC
                    , involArr = inv
                    , crossArr = let cr = crossArr tangle
                                 in V.concat [V.take idx cr, V.drop (idx + 1) cr, crossArr sub]
                    , legsN    = legs
                    }

    multiSurgery tangle = --tensorSubst 1 . fmap toTangle
        implode
            ( numberOfFreeLoops tangle
            , map oppositeExt $ allLegs tangle
            , do
                b <- allVertices tangle
                c <- allVertices $ vertexContent b
                let nb = map (oppositeInt b) $ outcomingDarts c
                return $! (nb, vertexContent c)
            )
        where
            offset = UV.prescanl' (+) 0 $
                UV.generate (numberOfVertices tangle) $ \ !i ->
                    numberOfVertices $ vertexContent $ nthVertex tangle (i + 1)

            oppositeInt b u | isLeg v    = oppositeExt $ nthOutcomingDart b (legPlace v) -- dartByCrossingLegId b (legPlace v)
                            | otherwise  = (w, beginPlace v)
                where v = opposite u
                      c = beginVertex v
                      w = (offset UV.! (vertexIndex b - 1)) + vertexIndex c

            oppositeExt u | isLeg v    = (0, legPlace v)
                          | otherwise  = oppositeInt c $ nthLeg (vertexContent $ beginVertex v) (beginPlace v) --subTangleLegFromDart v
                where v = opposite u
                      c = beginVertex v


class (Surgery k) => TensorSurgery k where
    tensorSurgery :: Int -> k (Tangle a) -> k a

instance TensorSurgery Tangle where
    tensorSurgery k tangle = implode (k * numberOfFreeLoops tangle, border, body)
        where
            n = numberOfVertices tangle

            crossSubst =
                let substList = do
                        v <- allVertices tangle
                        let t = vertexContent v
                        when (numberOfLegs t /= 4 * k) $
                            fail "Tangle.tensorSubst: bad number of legs"
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
                return (map (resolveInCrossing c) $ incomingDarts c', vertexContent c')


-- |     edgesToGlue = 1                 edgesToGlue = 2                 edgesToGlue = 3
-- ........|                       ........|                       ........|
-- (leg+1)-|---------------3       (leg+1)-|---------------2       (leg+1)-|---------------1
--         |  +=========+                  |  +=========+                  |  +=========+
--  (leg)--|--|-0-\ /-3-|--2        (leg)--|--|-0-\ /-3-|--1        (leg)--|--|-0-\ /-3-|--0
-- ........|  |    *    |                  |  |    *    |                  |  |    *    |
-- ........|  |   / \-2-|--1       (leg-1)-|--|-1-/ \-2-|--0       (leg-1)-|--|-1-/ \   |
-- ........|  |  1      |          ........|  +=========+                  |  |      2  |
-- ........|  |   \-----|--0       ........|                       (leg-2)-|--|-----/   |
-- ........|  +=========+          ........|                       ........|  +=========+
glueToBorder :: (AsTangle t) => Int -> (t a, Int) -> a -> Vertex Tangle a
glueToBorder !gl (!tangle', !lp) !cr | gl < 0 || gl > 4  = error $ printf "glueToBorder: legsToGlue must be in [0 .. 4], but %i found" gl
                                     | gl > oldL         = error $ printf "glueToBorder: not enough legs to glue (l = %i, gl = %i)" oldL gl
                                     | otherwise         =
    flip nthVertex newC $!
        Tangle
            { loopsN   = numberOfFreeLoops tangle
            , vertexN  = newC
            , involArr = PV.create $ do
                inv <- PMV.new (4 * newC + newL)

                let {-# INLINE copyModified #-}
                    copyModified !index !index' =
                        let y | x < 4 * oldC    = x
                              | ml < oldL - gl  = (4 * newC) + 4 - gl + ml
                              | otherwise       = (4 * newC) - 5 + oldL - ml
                              where
                                  x = involArr tangle `PV.unsafeIndex` index'
                                  ml = (x - 4 * oldC - lp - 1) `mod` oldL
                        in PMV.unsafeWrite inv index y

                forM_ [0 .. 4 * oldC - 1] $ \ !i ->
                    copyModified i i

                forM_ [0 .. gl - 1] $ \ !i ->
                    copyModified (4 * (newC - 1) + i) (4 * oldC + ((lp - i) `mod` oldL))

                forM_ [0 .. 3 - gl] $ \ !i -> do
                    let a = 4 * (newC - 1) + gl + i
                        b = 4 * newC + i
                    PMV.unsafeWrite inv a b
                    PMV.unsafeWrite inv b a

                forM_ [0 .. oldL - 1 - gl] $ \ !i ->
                    copyModified (4 * newC + i + 4 - gl) (4 * oldC + ((lp + 1 + i) `mod` oldL))

                return inv

            , crossArr = V.snoc (crossArr tangle) cr
            , legsN    = newL
            }
    where tangle = toTangle tangle'
          oldL = numberOfLegs tangle
          oldC = numberOfVertices tangle
          newC = oldC + 1
          newL = oldL + 4 - 2 * gl


emptyTangle :: Tangle0 a
emptyTangle = loopTangle 0


loopTangle :: Int -> Tangle0 a
loopTangle n | n < 0      = error "loopTangle: negative number of loops"
             | otherwise  =
    T0 $ Tangle
        { loopsN   = n
        , vertexN  = 0
        , involArr = PV.empty
        , crossArr = V.empty
        , legsN    = 0
        }


-- TODO: better name?
emptyPropagatorTangle :: Tangle2 a
emptyPropagatorTangle =
    T2 $ Tangle
        { loopsN   = 0
        , vertexN  = 0
        , involArr = PV.fromList [1, 0]
        , crossArr = V.empty
        , legsN    = 2
        }


-- TODO: better name?
lonerPropagatorTangle :: a -> Tangle2 a
lonerPropagatorTangle cr =
    T2 $ Tangle
        { loopsN   = 0
        , vertexN  = 1
        , involArr = PV.fromList [4, 5, 3, 2, 0, 1]
        , crossArr = V.singleton cr
        , legsN    = 2
        }


zeroTangle :: Tangle4 a
zeroTangle =
    T4 $ Tangle
        { loopsN   = 0
        , vertexN  = 0
        , involArr = PV.fromList [3, 2, 1, 0]
        , crossArr = V.empty
        , legsN    = 4
        }


infinityTangle :: Tangle4 a
infinityTangle =
    T4 $ Tangle
        { loopsN   = 0
        , vertexN  = 0
        , involArr = PV.fromList [1, 0, 3, 2]
        , crossArr = V.empty
        , legsN    = 4
        }


lonerTangle :: a -> Tangle4 a
lonerTangle cr =
    T4 $ Tangle
        { loopsN   = 0
        , vertexN  = 1
        , involArr = PV.fromList [4, 5, 6, 7, 0, 1, 2, 3]
        , crossArr = V.singleton cr
        , legsN    = 4
        }


lonerProjection :: Tangle4 ProjectionCrossing
lonerProjection = lonerTangle ProjectionCrossing


lonerOverCrossing, lonerUnderCrossing :: Tangle4 DiagramCrossing
lonerOverCrossing = lonerTangle OverCrossing
lonerUnderCrossing = lonerTangle UnderCrossing


chainTangle :: V.Vector a -> Tangle4 a
chainTangle cs | n == 0     = zeroTangle
               | otherwise  =
        T4 $ Tangle
            { loopsN   = 0
            , vertexN  = n
            , involArr = PV.create $ do
                inv <- PMV.new $ 4 * (n + 1)
                let connect !a !b = PMV.write inv a b >> PMV.write inv b a
                connect 0 (4 * n)
                connect 1 (4 * n + 1)
                connect (4 * (n - 1) + 2) (4 * n + 2)
                connect (4 * (n - 1) + 3) (4 * n + 3)
                forM_ [0 .. n - 2] $ \ !i -> do
                    let c = 4 * i
                    connect (c + 3) (c + 4)
                    connect (c + 2) (c + 5)
                return inv
            , crossArr = cs
            , legsN    = 4
            }
    where n = V.length cs


zipTangles :: Tangle a -> Tangle a -> Tangle0 a
zipTangles a b | l /= l'    = error $ printf "zipTangles: arguments must have same number of legs, but %i and %i provided" l l'
               | otherwise  = T0 $ horizontalComposition l (a, 0) (b, 1)
    where l = numberOfLegs a
          l' = numberOfLegs b


zipTangles2 :: Tangle2 a -> Tangle2 a -> Tangle0 a
zipTangles2 (T2 a) (T2 b) = T0 $ horizontalComposition 2 (a, 0) (b, 1)


zipTangles4 :: Tangle4 a -> Tangle4 a -> Tangle0 a
zipTangles4 (T4 a) (T4 b) = T0 $ horizontalComposition 4 (a, 0) (b, 1)


zipTangles6 :: Tangle6 a -> Tangle6 a -> Tangle0 a
zipTangles6 (T6 a) (T6 b) = T0 $ horizontalComposition 6 (a, 0) (b, 1)


-- See http://www.mi.sanu.ac.rs/vismath/sl/l14.htm
conwaySum :: Tangle4 a -> Tangle4 a -> Tangle4 a
conwaySum (T4 a) (T4 b) = T4 $ horizontalComposition 2 (a, 2) (b, 0)


{-# INLINE tangle0 #-}
tangle0 :: Tangle a -> Tangle0 a
tangle0 t | l == 0     = T0 t
          | otherwise  = error $ printf "tangle0: tangle must have 0 legs, but %i presented" l
    where l = numberOfLegs t


{-# INLINE tangle2 #-}
tangle2 :: Tangle a -> Tangle2 a
tangle2 t | l == 2     = T2 t
          | otherwise  = error $ printf "tangle2: tangle must have 2 legs, but %i presented" l
    where l = numberOfLegs t


{-# INLINE tangle4 #-}
tangle4 :: Tangle a -> Tangle4 a
tangle4 t | l == 4     = T4 t
          | otherwise  = error $ printf "tangle4: tangle must have 4 legs, but %i presented" l
    where l = numberOfLegs t


{-# INLINE tangle6 #-}
tangle6 :: Tangle a -> Tangle6 a
tangle6 t | l == 6     = T6 t
          | otherwise  = error $ printf "tangle6: tangle must have 6 legs, but %i presented" l
    where l = numberOfLegs t


data CrossingFlag a = Direct !a | Flipped !a | Masked

data MoveState s a =
    MoveState
        { stateSource      :: !(Tangle a)
        , stateMask        :: !(MV.STVector s (CrossingFlag a))
        , stateCircles     :: !(STRef s Int)
        , stateConnections :: !(MV.STVector s (Dart Tangle a))
        }

{-# INLINE withState #-}
withState :: (MoveState s a -> ST.ST s x) -> ModifyM Tangle a s x
withState f = ModifyTangleM $ do
    st <- Reader.ask
    Reader.lift (f st)

readMaskST :: MoveState s a -> Vertex Tangle a -> ST.ST s (CrossingFlag a)
readMaskST st c = MV.read (stateMask st) (vertexIndex c)

writeMaskST :: MoveState s a -> Vertex Tangle a -> CrossingFlag a -> ST.ST s ()
writeMaskST st c = MV.write (stateMask st) (vertexIndex c)

reconnectST :: MoveState s a -> [(Dart Tangle a, Dart Tangle a)] -> ST.ST s ()
reconnectST st connections =
    forM_ connections $ \ (!a, !b) -> do
        when (a == b) $ fail $ printf "reconnect: %s connect to itself" (show a)
        MV.write (stateConnections st) (dartIndex a) b
        MV.write (stateConnections st) (dartIndex b) a

instance ModifyDSL Tangle where
    newtype ModifyM Tangle a s x = ModifyTangleM { unM :: Reader.ReaderT (MoveState s a) (ST.ST s) x }
        deriving (Functor, Applicative, Monad)

    modifyKnot tangle modification = ST.runST $ do
        st <- do
            connections <- MV.new (numberOfDarts tangle)
            forM_ (allEdges tangle) $ \ (!a, !b) -> do
                MV.write connections (dartIndex a) b
                MV.write connections (dartIndex b) a

            mask <- MV.new (numberOfVertices tangle + 1)
            forM_ (allVertices tangle) $ \ v ->
                MV.write mask (vertexIndex v) (Direct $ vertexContent v)

            circlesCounter <- newSTRef $ numberOfFreeLoops tangle
            return MoveState
                { stateSource      = tangle
                , stateMask        = mask
                , stateCircles     = circlesCounter
                , stateConnections = connections
                }

        Reader.runReaderT (unM modification) st

        do
            offset <- UMV.new (numberOfVertices tangle + 1)
            foldM_ (\ !x !c -> do
                    msk <- readMaskST st c
                    case msk of
                        Masked -> return x
                        _      -> UMV.write offset (vertexIndex c) x >> (return $! x + 1)
                ) 1 (allVertices tangle)

            let pair d | isLeg d    = return $! (,) 0 $! legPlace d
                       | otherwise  = do
                           let i = beginVertexIndex d
                           msk <- MV.read (stateMask st) i
                           off <- UMV.read offset i
                           case msk of
                               Direct _  -> return (off, beginPlace d)
                               Flipped _ -> return (off, 3 - beginPlace d)
                               Masked    -> fail $ printf "Tangle.modifyKnot: %s is touching masked crossing %i at:\n%s" (show d) i (show $ stateSource st)

            let opp d = MV.read (stateConnections st) (dartIndex d)

            border <- forM (allLegs tangle) (opp >=> pair)
            connections <- do
                alive <- flip filterM (allVertices tangle) $ \ !c -> do
                    msk <- readMaskST st c
                    return $! case msk of
                        Masked -> False
                        _      -> True

                forM alive $ \ !c -> do
                        msk <- readMaskST st c
                        con <- mapM (opp >=> pair) $ outcomingDarts c
                        return $! case msk of
                            Direct s  -> (con, s)
                            Flipped s -> (reverse con, s)
                            Masked    -> error "internal error"

            circles <- readSTRef (stateCircles st)
            return $! implode (circles, border, connections)

    aliveCrossings = do
        tangle <- withState (return . stateSource)
        filterM (fmap not . isMaskedC) $ allVertices tangle

    emitLoopsC dn =
        withState $ \ !st ->
            modifySTRef' (stateCircles st) (+ dn)

    oppositeC d = do
        when (isDart d) $ do
            masked <- isMaskedC $ beginVertex d
            when masked $
                fail $ printf "Tangle.oppositeC: touching masked crossing when taking from %s" (show d)
        withState $ \ s ->
            MV.read (stateConnections s) (dartIndex d)

    passOverC d =
        withState $ \ !st -> do
            when (isLeg d) $ fail $ printf "Tangle.passOverC: leg %s passed" (show d)
            msk <- readMaskST st $ beginVertex d
            case msk of
                Masked    -> fail $ printf "Tangle.passOverC: touching masked crossing when taking from %s" (show d)
                Direct t  -> return $! isPassingOver' t (beginPlace d)
                Flipped t -> return $! isPassingOver' t (3 - beginPlace d)

    maskC crossings =
        withState $ \ !st ->
            forM_ crossings $ \ !c ->
                writeMaskST st c Masked

    isMaskedC c =
        withState $ \ !st -> do
            msk <- readMaskST st c
            return $! case msk of
                Masked -> True
                _      -> False

    modifyC needFlip f crossings =
        withState $ \ !st ->
            forM_ crossings $ \ !c -> do
                msk <- readMaskST st c
                writeMaskST st c $
                    case msk of
                        Direct s  | needFlip  -> Flipped $ f s
                                  | otherwise -> Direct $ f s
                        Flipped s | needFlip  -> Direct $ f s
                                  | otherwise -> Flipped $ f s
                        Masked                -> error $ printf "Tangle.modifyC: flipping masked crossing %s" (show c)

    connectC connections =
        withState $ \ !st ->
            reconnectST st connections

    substituteC substitutions = do
        reconnections <- mapM (\ (a, b) -> (,) a `fmap` oppositeC b) substitutions
        withState $ \ !st -> do
            let source = stateSource st

            arr <- MV.new (numberOfDarts source)
            forM_ (allEdges source) $ \ (!a, !b) -> do
                MV.write arr (dartIndex a) a
                MV.write arr (dartIndex b) b

            forM_ substitutions $ \ (a, b) ->
                if a == b
                    then modifySTRef' (stateCircles st) (+ 1)
                    else MV.write arr (dartIndex b) a

            (reconnectST st =<<) $ forM reconnections $ \ (a, b) ->
                (,) a `fmap` MV.read arr (dartIndex b)
