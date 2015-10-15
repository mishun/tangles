{-# LANGUAGE TypeFamilies, UnboxedTuples, RankNTypes, GeneralizedNewtypeDeriving #-}
module Math.Topology.KnotTh.EmbeddedLink
    ( module Math.Topology.KnotTh.Knotted
    , module Math.Topology.KnotTh.Knotted.Crossings.Projection
    , module Math.Topology.KnotTh.Knotted.Crossings.Diagram

    , EmbeddedLink
    , EmbeddedLinkProjection
    , EmbeddedLinkProjectionVertex
    , EmbeddedLinkProjectionDart
    , EmbeddedLinkDiagram
    , EmbeddedLinkDiagramVertex
    , EmbeddedLinkDiagramDart
    , fromLink
    , toLink
    , fromTangleAndStar
    , splitIntoTangleAndStar
    , testPrime
    , has4LegPlanarPart
    ) where

import Control.Applicative (Applicative)
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Control.Monad (filterM, foldM, foldM_, forM, forM_, guard, liftM2, unless, void, when)
import Control.Monad.IfElse (unlessM, whenM, whileM)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.ST as ST
import qualified Data.Array.ST as STArray
import qualified Data.Array.Unboxed as A
import Data.Bits ((.&.), shiftL, shiftR, complement)
import Data.Function (fix)
import Data.List (foldl', find)
import Data.Maybe (fromMaybe, fromJust)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Primitive.Mutable as PMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf
import Math.Topology.KnotTh.Dihedral.D4
import qualified Math.Topology.KnotTh.SurfaceGraph as SG
import Math.Topology.KnotTh.ChordDiagram
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Crossings.Projection
import Math.Topology.KnotTh.Knotted.Crossings.Diagram
import Math.Topology.KnotTh.Knotted.Threads
import Math.Topology.KnotTh.Moves.ModifyDSL
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Tangle


data EmbeddedLink a =
    EmbeddedLink
        { loopsCount      :: {-# UNPACK #-} !Int
        , vertexCount     :: {-# UNPACK #-} !Int
        , involutionArray :: {-# UNPACK #-} !(PV.Vector Int)
        , crossingsArray  :: {-# UNPACK #-} !(V.Vector a)
        , faceSystem      :: FaceSystem
        }

data FaceSystem =
    FaceSystem
        { faceCount       :: {-# UNPACK #-} !Int
        , faceDataOffset  :: {-# UNPACK #-} !(PV.Vector Int)
        , faceCCWBrdDart  :: {-# UNPACK #-} !(PV.Vector Int)
        , faceLLookup     :: {-# UNPACK #-} !(PV.Vector Int)
        }


instance DartDiagram EmbeddedLink where
    data Dart EmbeddedLink a = Dart !(EmbeddedLink a) {-# UNPACK #-} !Int

    dartOwner (Dart k _) = k
    dartIndex (Dart _ i) = i

    opposite (Dart k d) = Dart k (involutionArray k `PV.unsafeIndex` d)

    nextCCW (Dart k d) = Dart k ((d .&. complement 3) + ((d + 1) .&. 3))

    nextCW (Dart k d) = Dart k ((d .&. complement 3) + ((d - 1) .&. 3))

    nextBy delta (Dart k d) = Dart k ((d .&. complement 3) + ((d + delta) .&. 3))

    numberOfEdges l = PV.length (involutionArray l) `shiftR` 1

    numberOfDarts l = PV.length (involutionArray l)

    nthDart k i | i < 0 || i >= b  = error $ printf "nthDart: index %i is out of bounds (0, %i)" i b
                | otherwise        = Dart k i
        where b = PV.length (involutionArray k)

    allDarts k = map (Dart k) [0 .. PV.length (involutionArray k) - 1]

    allEdges k =
        foldl' (\ !es !i ->
                let j = involutionArray k `PV.unsafeIndex` i
                in if i < j
                    then (Dart k i, Dart k j) : es
                    else es
            ) [] [0 .. PV.length (involutionArray k) - 1]

    dartIndicesRange k = (0, numberOfDarts k - 1)


instance VertexDiagram EmbeddedLink where
    data Vertex EmbeddedLink a = Vertex !(EmbeddedLink a) {-# UNPACK #-} !Int

    vertexOwner (Vertex k _) = k
    vertexIndex (Vertex _ i) = i + 1

    vertexDegree _ = 4

    numberOfVertices = vertexCount

    nthVertex k i | i < 1 || i > b  = error $ printf "nthVertex: index %i is out of bounds (1, %i)" i b
                  | otherwise       = Vertex k (i - 1)
        where b = numberOfVertices k

    allVertices k = map (Vertex k) [0 .. numberOfVertices k - 1]

    nthOutcomingDart (Vertex k c) i = Dart k ((c `shiftL` 2) + (i .&. 3))

    outcomingDarts c = map (nthOutcomingDart c) [0 .. 3]

    maybeBeginVertex (Dart k d) = Just $! Vertex k (d `shiftR` 2)

    beginVertex (Dart k d) = Vertex k (d `shiftR` 2)

    beginPlace (Dart _ d) = d .&. 3

    isDart _ = True

    vertexIndicesRange k = (1, numberOfVertices k)


instance (NFData a) => NFData (EmbeddedLink a) where
    rnf k = rnf (crossingsArray k) `seq` k `seq` ()

instance (NFData a) => NFData (Vertex EmbeddedLink a)

instance (NFData a) => NFData (Dart EmbeddedLink a)


instance Functor EmbeddedLink where
    fmap f k = k { crossingsArray = f `fmap` crossingsArray k }


instance Knotted EmbeddedLink where
    vertexCrossing (Vertex k i) = crossingsArray k `V.unsafeIndex` i

    mapCrossings f t =
        t { crossingsArray =
                V.generate (numberOfVertices t) $ \ i -> f (nthVertex t $ i + 1)
          }

    unrootedHomeomorphismInvariant link = UV.singleton (numberOfFreeLoops link) UV.++ internal
        where
            internal | numberOfVertices link == 0  = UV.empty
                     | otherwise                   = minimum $ do
                dart <- allDarts link
                dir <- bothDirections
                globalG <- fromMaybe [d4I] $ globalTransformations link
                return $! codeWithDirection globalG dir dart

            codeWithDirection !globalG !dir !start = UV.create $ do
                let n = numberOfVertices link

                index <- UMV.replicate (n + 1) 0
                incoming <- UMV.replicate (n + 1) 0
                queue <- MV.new n
                free <- newSTRef 1

                let {-# INLINE look #-}
                    look !d = do
                        let u = beginVertexIndex d
                        ux <- UMV.unsafeRead index u
                        if ux > 0
                            then do
                                up <- UMV.unsafeRead incoming u
                                return $! (ux `shiftL` 2) + (((beginPlace d - up) * directionSign dir) .&. 3)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                UMV.unsafeWrite index u nf
                                UMV.unsafeWrite incoming u (beginPlace d)
                                MV.unsafeWrite queue (nf - 1) d
                                return $! nf `shiftL` 2

                rc <- UMV.replicate (6 * n + 1) 0
                UMV.unsafeWrite rc 0 $! numberOfFreeLoops link

                let {-# INLINE lookAndWrite #-}
                    lookAndWrite !d !offset = do
                        look d >>= UMV.unsafeWrite rc offset
                        return $! offset + 1

                void $ look start
                flip fix 0 $ \ bfs !headI ->
                    whenM ((headI + 1 <) `fmap` readSTRef free) $ do
                        input <- MV.unsafeRead queue headI
                        void $ foldMIncomingDartsFrom input dir lookAndWrite (6 * headI + 3)
                        case crossingCodeWithGlobal globalG dir input of
                            (# be, le #) -> do
                                UMV.unsafeWrite rc (6 * headI + 1) be
                                UMV.unsafeWrite rc (6 * headI + 2) le
                        bfs $! headI + 1

                fix $ \ _ -> do
                    whenM ((<= n) `fmap` readSTRef free) $
                        fail "codeWithDirection: disconnected diagram (not implemented)"

                return rc

    isConnected link =
        numberOfFreeLoops link < (if numberOfVertices link == 0 then 2 else 1)

    numberOfFreeLoops = loopsCount

    changeNumberOfFreeLoops loops k | loops >= 0  = k { loopsCount = loops }
                                    | otherwise   = error $ printf "changeNumberOfFreeLoops: number of free loops %i is negative" loops

    emptyKnotted =
        EmbeddedLink
            { loopsCount      = 0
            , vertexCount     = 0
            , involutionArray = PV.empty
            , crossingsArray  = V.empty
            , faceSystem      =
                FaceSystem
                    { faceCount       = 1
                    , faceDataOffset  = PV.replicate 2 0
                    , faceCCWBrdDart  = PV.empty
                    , faceLLookup     = PV.empty
                    }
            }

    type ExplodeType EmbeddedLink a = (Int, [([(Int, Int)], a)])

    explode link =
        ( numberOfFreeLoops link
        , map (\ v -> (map endPair' $ outcomingDarts v, vertexCrossing v)) $ allVertices link
        )

    implode (loops, list) = ST.runST $ do
        when (loops < 0) $
            error $ printf "EmbeddedLink.implode: number of free loops %i is negative" loops

        let n = length list
        cr <- PMV.new (4 * n)
        st <- MV.new n

        forM_ (list `zip` [0 ..]) $ \ ((!ns, !cs), !i) -> do
            MV.unsafeWrite st i cs
            case ns of
                [p0, p1, p2, p3] ->
                    forM_ [(p0, 0), (p1, 1), (p2, 2), (p3, 3)] $ \ ((!c, !p), !j) -> do
                        let a = 4 * i + j
                            b | c < 1 || c > n  = error $ printf "EmbeddedLink.implode: crossing index %i is out of bounds [1 .. %i]" c n
                              | p < 0 || p > 3  = error $ printf "EmbeddedLink.implode: place index %i is out of bounds [0 .. 3]" p
                              | otherwise       = 4 * (c - 1) + p
                        when (a == b) $
                            error $ printf "EmbeddedLink.implode: (%i, %i) connected to itself" c p
                        PMV.unsafeWrite cr a b
                        when (b < a) $ do
                            x <- PMV.unsafeRead cr b
                            when (x /= a) $
                                error $ printf "EmbeddedLink.implode: (%i, %i) points to unconsistent position" c p

                _                ->
                    error $ printf "EmbeddedLink.implode: there must be 4 neighbours for every crossing, but found %i for %i-th"
                                        (length ns) (i + 1)

        cr' <- PV.unsafeFreeze cr
        st' <- V.unsafeFreeze st

        let link = EmbeddedLink
                { loopsCount      = loops
                , vertexCount     = n
                , involutionArray = cr'
                , crossingsArray  = st'
                , faceSystem      = makeFaceSystem link
                }

        return $! link


makeFaceSystem :: EmbeddedLink a -> FaceSystem
makeFaceSystem link =
    let n = numberOfVertices link

        (fcN, fllookN, fccwdN) = ST.runST $ do
            fccwd <- PMV.new (4 * n)
            fllook <- PMV.replicate (8 * n) (-1)

            (fc, _) <- foldM (\ (!fid, !base) !start -> do
                mi <- PMV.read fllook (2 * start)
                if mi >= 0
                    then return (fid, base)
                    else do
                        sz <- fix (\ mark !offset !i -> do
                            PMV.write fllook (2 * i) fid
                            PMV.write fllook (2 * i + 1) offset
                            PMV.write fccwd (base + offset) i

                            let i' = involutionArray link `PV.unsafeIndex` i
                                j = (i' .&. complement 3) + ((i' - 1) .&. 3)
                            mj <- PMV.read fllook (2 * j)
                            if mj >= 0
                                then return $! offset + 1
                                else mark (offset + 1) j
                            ) 0 start
                        return (fid + 1, base + sz)
                ) (0, 0) [0 .. 4 * n - 1]

            fccwd' <- PV.unsafeFreeze fccwd
            fllook' <- PV.unsafeFreeze fllook
            return (fc, fllook', fccwd')

        foffN = PV.create $ do
            foff <- PMV.replicate (fcN + 1) 0
            forM_ [0 .. 4 * n - 1] $ \ !i -> do
                let fid = fllookN PV.! (2 * i)
                cur <- PMV.read foff fid
                PMV.write foff fid $! cur + 1
            foldM_ (\ !offset !i -> do
                    cur <- PMV.read foff i
                    PMV.write foff i offset
                    return $! offset + cur
                ) 0 [0 .. fcN]
            return foff

    in FaceSystem
            { faceCount       = fcN
            , faceDataOffset  = foffN
            , faceCCWBrdDart  = fccwdN
            , faceLLookup     = fllookN
            }


instance KnottedDiagram EmbeddedLink where
    isReidemeisterReducible =
        any (\ ab ->
                let ba = opposite ab
                    ac = nextCCW ab
                in (ac == ba) || (passOver ab == passOver ba && opposite ac == nextCW ba)
            ) . allOutcomingDarts

    tryReduceReidemeisterI link = do
        d <- find (\ d -> opposite d == nextCCW d) (allOutcomingDarts link)
        return $! modifyKnot link $ do
            let ac = nextCW d
                ab = nextCW ac
                ba = opposite ab
            substituteC [(ba, ac)]
            maskC [beginVertex d]

    tryReduceReidemeisterII link = do
        abl <- find (\ abl ->
                let bal = opposite abl
                    abr = nextCCW abl
                    bar = nextCW bal
                in passOver abl == passOver bal
                    && abr == opposite bar
                    && beginVertex abl /= beginVertex bal
            ) (allOutcomingDarts link)

        let bal = opposite abl
            a = beginVertex abl
            b = beginVertex bal

            ap = threadContinuation abl
            aq = nextCW abl
            br = nextCCW bal
            bs = threadContinuation bal

            pa = opposite ap
            qa = opposite aq
            rb = opposite br
            sb = opposite bs

        return $! if rightFace (nextCW abl) == leftFace (nextCCW bal)
            then emptyKnotted
            else modifyKnot link $ do
                if | qa == ap || rb == bs ->
                        if qa == ap && rb == bs
                            then emitLoopsC 1
                            else connectC $ [(pa, qa) | qa /= ap] ++ [(rb, sb) | rb /= bs]

                   | qa == bs || rb == ap ->
                        if qa == bs && rb == ap
                            then error "strange configuration"
                            else connectC $ [(sb, qa) | qa /= bs] ++ [(rb, pa) | rb /= ap]

                   | otherwise            -> do
                        if qa == br
                            then emitLoopsC 1
                            else connectC [(qa, rb)]
                        if pa == bs
                            then emitLoopsC 1
                            else connectC [(pa, sb)]

                maskC [a, b]

    reidemeisterIII link = do
        ab <- allOutcomingDarts link
        let ac = nextCCW ab
            ba = opposite ab
            ca = opposite ac
            bc = nextCW ba
            cb = nextCCW ca

        guard $ bc == opposite cb

        let a = beginVertex ab
            b = beginVertex ba
            c = beginVertex ca

        guard $ (a /= b) && (a /= c) && (b /= c)
        guard $ passOver bc == passOver cb

        guard $ let altRoot | passOver ab == passOver ba  = ca
                            | otherwise                   = bc
                in ab < altRoot

        let ap = threadContinuation ab
            aq = nextCW ab
            br = nextCW bc
            cs = nextCCW cb

        return $! modifyKnot link $ do
            substituteC [(ca, ap), (ba, aq), (ab, br), (ac, cs)]
            connectC [(br, aq), (cs, ap)]


instance (Show a) => Show (EmbeddedLink a) where
    show = printf "implode %s" . show . explode


instance (Show a) => Show (Vertex EmbeddedLink a) where
    show v =
        printf "(Crossing %i %s [ %s ])"
            (vertexIndex v)
            (show $ vertexCrossing v)
            (unwords $ map (show . opposite) $ outcomingDarts v)


instance Show (Dart EmbeddedLink a) where
    show d = let (c, p) = beginPair' d
             in printf "(Dart %i %i)" c p


instance SurfaceDiagram EmbeddedLink where
    numberOfFaces = faceCount . faceSystem

    nthFace link i | i > 0 && i <= n  = Face link (i - 1)
                   | otherwise        = error $ printf "nthFace: index %i is out of bounds (1, %i)" i n
        where
            n = numberOfFaces link

    allFaces link = map (Face link) [0 .. numberOfFaces link - 1]

    data Face EmbeddedLink ct = Face !(EmbeddedLink ct) {-# UNPACK #-} !Int

    faceDegree (Face l i) =
        let cur = faceDataOffset (faceSystem l) `PV.unsafeIndex` i
            nxt = faceDataOffset (faceSystem l) `PV.unsafeIndex` (i + 1)
        in nxt - cur

    faceOwner (Face l _) = l

    faceIndex (Face _ i) = i + 1

    leftFace (Dart l i) = Face l $ faceLLookup (faceSystem l) `PV.unsafeIndex` (2 * i)

    leftPlace (Dart l i) = faceLLookup (faceSystem l) `PV.unsafeIndex` (2 * i + 1)

    nthDartInCCWTraverse (Face l i) p =
        let cur = faceDataOffset (faceSystem l) `PV.unsafeIndex` i
            nxt = faceDataOffset (faceSystem l) `PV.unsafeIndex` (i + 1)
        in Dart l $ faceCCWBrdDart (faceSystem l) `PV.unsafeIndex` (cur + p `mod` (nxt - cur))

    faceIndicesRange l = (1, numberOfFaces l)


instance SurfaceKnotted EmbeddedLink


type EmbeddedLinkProjection = EmbeddedLink ProjectionCrossing
type EmbeddedLinkProjectionVertex = Vertex EmbeddedLink ProjectionCrossing
type EmbeddedLinkProjectionDart = Dart EmbeddedLink ProjectionCrossing

type EmbeddedLinkDiagram = EmbeddedLink DiagramCrossing
type EmbeddedLinkDiagramVertex = Vertex EmbeddedLink DiagramCrossing
type EmbeddedLinkDiagramDart = Dart EmbeddedLink DiagramCrossing


fromLink :: Link a -> EmbeddedLink a
fromLink = implode . explode


toLink :: EmbeddedLink a -> Link a
toLink sl | eulerCharOf sl == 2       = l
          | numberOfVertices sl == 0  = l
          | otherwise                 = error "toLink: euler char must be 2"
    where
        l = implode $ explode sl


fromTangleAndStar :: (ChordDiagram cd) => cd -> Tangle a -> EmbeddedLink a
fromTangleAndStar cd tangle
    | numberOfChordEnds cd /= numberOfLegs tangle  = error "fromTangleAndStar: size conflict"
    | otherwise                                    = fromTangleAndStar' (nthLeg tangle . chordMate cd . legPlace) tangle


{-# INLINE fromTangleAndStar' #-}
fromTangleAndStar' :: (Dart Tangle a -> Dart Tangle a) -> Tangle a -> EmbeddedLink a
fromTangleAndStar' withLeg tangle =
    let watch d | isDart d   = beginPair' d
                | otherwise  = watch $ opposite $ withLeg d
    in implode
        ( numberOfFreeLoops tangle + div (length $ filter (\ l -> opposite l == withLeg l) $ allLegs tangle) 2
        , map (\ v -> (map watch $ incomingDarts v, vertexCrossing v)) $ allVertices tangle
        )


splitIntoTangleAndStar :: EmbeddedLink a -> (Tangle a, Vertex SG.SurfaceGraph a')
splitIntoTangleAndStar link =
    let g = SG.constructFromList $
            map (map (first (+ (-1)) . endPair') . outcomingDarts) $
                allVertices link

        (sp, star, _, _) = SG.sphereStarDecomposition g

        tangle =
            let l = vertexDegree sp

                tend d = case endPair' d of
                            (0, p) -> (0, l - 1 - p)
                            pair   -> pair

                border = reverse $ map tend $ outcomingDarts sp

                body = do
                    v <- tail $ allVertices $ vertexOwner sp
                    return (map tend $ outcomingDarts v, vertexCrossing $ nthVertex link $ vertexIndex v)
            in implode (numberOfFreeLoops link, border, body)

    in (tangle, star)


instance Surgery EmbeddedLink where
    surgery = error "not implemented"

    tensorSubst k crossF link = implode (k * numberOfFreeLoops link, body)
        where
            n = numberOfVertices link

            crossSubst =
                let substList = do
                        c <- allVertices link
                        let t = crossF c
                        when (numberOfLegs t /= 4 * k) $
                            fail "EmbeddedLink.tensorSubst: bad number of legs"
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

            resolveOutside !d !i =
                let (c, p) = beginPair d
                in resolveInCrossing c $ opposite $
                        nthLeg (crossSubst V.! vertexIndex c) (k * p + k - 1 - i)

            body = do
                c <- allVertices link
                let t = crossSubst V.! vertexIndex c
                c' <- allVertices t
                return (map (resolveInCrossing c) $ incomingDarts c', vertexCrossing c')


instance KnotWithPrimeTest EmbeddedLink ProjectionCrossing where
    isPrime = testPrime

instance KnotWithPrimeTest EmbeddedLink DiagramCrossing where
    isPrime link = testPrime link && testLayering link


testPrime :: EmbeddedLink a -> Bool
testPrime link | numberOfFreeLoops link > 0  = False
               | numberOfVertices link < 2   = True
               | otherwise                   = stoerWagner link >= 4


testLayering :: EmbeddedLinkDiagram -> Bool
testLayering link | numberOfFreeLoops link > 0  = False
                  | numberOfVertices link == 0  = False
                  | otherwise                   = ST.runST $ do
    let (n, marks, threads) = allThreadsWithMarks link

    comp <- STArray.newArray (1, n) 0 :: ST.ST s (STArray.STUArray s Int Int)
    lowlink <- STArray.newArray (1, n) 0 :: ST.ST s (STArray.STUArray s Int Int)
    time <- newSTRef 1
    stack <- newSTRef []

    let dfs u = do
            readSTRef time >>= STArray.writeArray lowlink u
            modifySTRef' time (+ 1)
            modifySTRef' stack (u :)
            isComponentRoot <- newSTRef True

            forM_ (snd $ fromJust $ find ((== u) . fst) threads) $ \ (_, d) -> do
                let v = abs (marks A.! nextCCW d)
                when (passUnder d && v /= u) $ do
                    do
                        vlw <- STArray.readArray lowlink v
                        unless (vlw > 0) $ dfs v

                    vlw <- STArray.readArray lowlink v
                    ulw <- STArray.readArray lowlink u
                    when (ulw > vlw) $ do
                        STArray.writeArray lowlink u vlw
                        writeSTRef isComponentRoot False

            isRoot <- readSTRef isComponentRoot
            when isRoot $ do
                let extract = do
                        st <- readSTRef stack
                        case st of
                            []       -> return ()
                            k : rest -> do
                                writeSTRef stack rest
                                STArray.writeArray lowlink k (n + 1)
                                STArray.writeArray comp k u
                                when (k /= u) extract
                extract

    forM_ [1 .. n] $ \ !i -> do
        lw <- STArray.readArray lowlink i
        unless (lw > 0) $ dfs i

    cmp <- mapM (STArray.readArray comp) [1 .. n]
    return $ all (== head cmp) cmp


cfor :: (Monad m) => (m a, a -> m Bool, a -> m a) -> (a -> m ()) -> m ()
cfor (initial, cond, next) body =
    let loop !i =
            whenM (cond i) $ do
                body i
                next i >>= loop
    in initial >>= loop


stoerWagner :: EmbeddedLink a -> Int
stoerWagner link = ST.runST $ do
    let sz = numberOfVertices link
    g <- STArray.newArray ((1, 1), (sz, sz)) 0 :: ST.ST s (STArray.STUArray s (Int, Int) Int)

    forM_ (allVertices link) $ \ u ->
        forM_ (outcomingDarts u) $ \ d -> do
            let v = endVertex d
                i = (vertexIndex u, vertexIndex v)
            w <- STArray.readArray g i
            STArray.writeArray g i $! w + 1

    v <- STArray.newListArray (1, sz) [1 .. sz] :: ST.ST s (STArray.STUArray s Int Int)

    a <- STArray.newArray_ (1, sz) :: ST.ST s (STArray.STUArray s Int Bool)
    w <- STArray.newArray_ (1, sz) :: ST.ST s (STArray.STUArray s Int Int)
    na <- STArray.newArray_ (1, sz) :: ST.ST s (STArray.STUArray s Int Int)

    let setA i x = flip (STArray.writeArray a) x =<< STArray.readArray v i

    best <- newSTRef $! 1 + numberOfEdges link
    n <- newSTRef sz

    whileM ((> 1) `fmap` readSTRef n) $ do
        setA 1 True

        do
            n' <- readSTRef n
            forM_ [2 .. n'] $ \ i -> do
                setA i False
                STArray.writeArray na (i - 1) i
                STArray.writeArray w i =<< STArray.readArray g =<<
                    liftM2 (,) (STArray.readArray v 1) (STArray.readArray v i)

        prev <- newSTRef =<< STArray.readArray v 1
        cfor (return 2, \ i -> (i <=) `fmap` readSTRef n, \ i -> return $! i + 1) $ \ !i -> do
            zj <- do
                n' <- readSTRef n
                zj <- newSTRef (-1)
                forM_ [2 .. n'] $ \ !j ->
                    unlessM (STArray.readArray a =<< STArray.readArray v j) $ do
                        zj' <- readSTRef zj
                        ok <- if zj' < 1
                                then return True
                                else liftM2 (>) (STArray.readArray w j) (STArray.readArray w zj')
                        when ok $ writeSTRef zj j
                readSTRef zj

            flip (STArray.writeArray a) True =<< STArray.readArray v zj

            lastIt <- (== i) `fmap` readSTRef n
            if lastIt
                then do
                    modifySTRef' best =<< (min `fmap` STArray.readArray w zj)
                    n' <- readSTRef n

                    forM_ [1 .. n'] $ \ !j -> do
                        delta <- STArray.readArray g =<< liftM2 (,) (STArray.readArray v zj) (STArray.readArray v j)
                        index <- liftM2 (,) (readSTRef prev) (STArray.readArray v j)
                        tmp <- STArray.readArray g index
                        STArray.writeArray g index $! tmp + delta
                        index' <- liftM2 (,) (STArray.readArray v j) (readSTRef prev)
                        STArray.writeArray g index' $! tmp + delta

                    STArray.writeArray v zj =<< STArray.readArray v n'
                    modifySTRef' n (+ (-1))

                else do
                    writeSTRef prev =<< STArray.readArray v zj
                    n' <- readSTRef n
                    forM_ [2 .. n'] $ \ !j ->
                        unlessM (STArray.readArray a =<< STArray.readArray v j) $ do
                            delta <- STArray.readArray g =<< liftM2 (,) (STArray.readArray v zj) (STArray.readArray v j)
                            tmp <- STArray.readArray w j
                            STArray.writeArray w j $! tmp + delta

    readSTRef best


has4LegPlanarPart :: EmbeddedLink a -> Bool
has4LegPlanarPart =
    let planar link start darts = ST.runST $ do
            vertex <- (STArray.newArray :: (STArray.Ix i) => (i, i) -> Bool -> ST.ST s (STArray.STUArray s i Bool)) (verticesRange link) False
            edge <- (STArray.newArray :: (STArray.Ix i) => (i, i) -> Bool -> ST.ST s (STArray.STUArray s i Bool)) (dartsRange link) False

            face <- (STArray.newArray :: (STArray.Ix i) => (i, i) -> Bool -> ST.ST s (STArray.STUArray s i Bool)) (facesRange link) False
            mapM_ (\ !e -> STArray.writeArray face (leftFace e) True) darts

            queue <- (STArray.newArray_ :: (STArray.Ix i) => (i, i) -> ST.ST s (STArray.STArray s i a)) (0, numberOfVertices link)
            qtail <- newSTRef 1

            STArray.writeArray vertex start True
            STArray.writeArray queue 0 start

            nv <- newSTRef 1
            ne <- newSTRef 4
            nf <- newSTRef 4

            let testFace f = do
                    fi <- STArray.readArray face f
                    unless fi $ do
                        STArray.writeArray face f True
                        modifySTRef' nf (+ 1)

            let testEdge e = do
                    ei <- STArray.readArray edge e
                    unless ei $ do
                        STArray.writeArray edge e True
                        STArray.writeArray edge (opposite e) True
                        modifySTRef' ne (+ 1)

            let testVertex v = do
                    vi <- STArray.readArray vertex v
                    unless vi $ do
                        STArray.writeArray vertex v True
                        modifySTRef' nv (+ 1)
                        t <- readSTRef qtail
                        STArray.writeArray queue t v
                        writeSTRef qtail $! t + 1

            let loop !qhead = do
                    whenM ((qhead <) `fmap` readSTRef qtail) $ do
                        v <- STArray.readArray queue qhead
                        forM_ [0 .. 3] $ \ !i -> do
                            let e = nthOutcomingDart v i
                            when (e `notElem` darts) $ do
                                testEdge e
                                testFace (leftFace e)
                                testFace (rightFace e)
                                testVertex (endVertex e)

                        loop $! qhead + 1

            loop 0

            nv' <- readSTRef nv
            nf' <- readSTRef nf
            ne' <- readSTRef ne
            let euler = nv' + nf' - ne'
            return $! (nv' > 1) && (nv' < numberOfVertices link) && (euler == 1)

    in \ link ->
        let select [] _ = False
            select (h : t) f = f h t || select t f
        in select (allEdges link) $ \ e1 r1 ->
            select r1 $ \ e2 r2 ->
                select r2 $ \ e3 r3 ->
                    select r3 $ \ e4 _ ->
                        let darts = [fst e1, snd e1, fst e2, snd e2, fst e3, snd e3, fst e4, snd e4]
                        in planar link (beginVertex $ fst e1) darts || planar link (beginVertex $ snd e1) darts


data CrossingMask a = Direct !a | Flipped !a | Masked
    deriving (Show)

data ModifyState a s =
    ModifyState
        { stateSource     :: !(EmbeddedLink a)
        , stateCircles    :: !(STRef s Int)
        , stateInvolution :: !(PMV.STVector s Int)
        , stateMask       :: !(MV.STVector s (CrossingMask a))
        }


{-# INLINE withState #-}
withState :: (ModifyState a s -> ST.ST s x) -> ModifyM EmbeddedLink a s x
withState f = ModifyEmbeddedLinkM $ do
    st <- Reader.ask
    Reader.lift (f st)


instance ModifyDSL EmbeddedLink where
    newtype ModifyM EmbeddedLink a s x = ModifyEmbeddedLinkM { unM :: Reader.ReaderT (ModifyState a s) (ST.ST s) x }
            deriving (Functor, Applicative, Monad)

    modifyKnot link modification = ST.runST $ do
        s <- do
            circ <- newSTRef $ numberOfFreeLoops link
            inv <- PV.thaw $ involutionArray link
            mask <- MV.new (numberOfVertices link)
            V.copy mask $ V.map Direct $ crossingsArray link
            return $! ModifyState
                          { stateSource     = link
                          , stateCircles    = circ
                          , stateInvolution = inv
                          , stateMask       = mask
                          }

        Reader.runReaderT (unM modification) s

        do
            mask <- V.freeze $ stateMask s
            let crs = V.ifilter (\ !i _ -> case mask V.! i of Masked -> False ; _ -> True) $ crossingsArray link
                n = V.length crs
                idx = UV.fromList $ do
                    let offsets = UV.prescanl'
                                      (\ off i -> off + case mask V.! i of Masked -> 0 ; _ -> 1)
                                      0 (UV.enumFromN 0 $ vertexCount link)
                    i <- [0 .. vertexCount link - 1]
                    let d = 4 * (offsets UV.! i)
                    case mask V.! i of
                        Masked    -> [-1, -1, -1, -1]
                        Direct _  -> [d, d + 1, d + 2, d + 3]
                        Flipped _ -> [d + 3, d + 2, d + 1, d]

            inv <- PV.freeze $ stateInvolution s
            forM_ [0 .. vertexCount link - 1] $ \ !v ->
                case mask V.! v of
                    Masked -> return ()
                    _      ->
                        forM_ [0 .. 3] $ \ !i ->
                            let a = 4 * v + i
                                b = inv PV.! a
                            in case mask V.! (b `shiftR` 2) of
                                Masked -> fail $ printf "EmbeddedLink.modifyKnot: touching masked crossing\nlink: %s\nmask: %s\ninvolution: %s"
                                       (show link) (show mask) (show inv)
                                _      -> return ()

            loops <- readSTRef $ stateCircles s
            let result = EmbeddedLink
                    { loopsCount      = loops
                    , vertexCount     = n
                    , involutionArray =
                        PV.map (idx UV.!) $ PV.concat $ do
                            i <- [0 .. vertexCount link - 1]
                            return $! case mask V.! i of
                                Masked    -> PV.empty
                                Direct _  -> PV.slice (4 * i) 4 inv
                                Flipped _ -> PV.reverse $ PV.slice (4 * i) 4 inv
                    , crossingsArray  = crs
                    , faceSystem      = makeFaceSystem result
                    }
            return $! result


    aliveCrossings = do
        link <- withState (return . stateSource)
        filterM (fmap not . isMaskedC) $ allVertices link

    emitLoopsC dn =
        withState $ \ !s ->
            modifySTRef' (stateCircles s) (+ dn)

    oppositeC (Dart link d) =
        withState $ \ !s ->
            Dart link `fmap` PMV.read (stateInvolution s) d

    passOverC d =
        withState $ \ !s -> do
            msk <- MV.read (stateMask s) $ vertexIndex $ beginVertex d
            case msk of
                Masked    -> fail "EmbeddedLink.passOverC: touching masked crossing when taking from %s" (show d)
                Direct  t -> return $! passOver' t (beginPlace d)
                Flipped t -> return $! passOver' t (3 - beginPlace d)

    maskC crossings =
        withState $ \ !s ->
            forM_ crossings $ \ (Vertex _ i) ->
                MV.write (stateMask s) i Masked

    isMaskedC (Vertex _ i) =
        withState $ \ !s -> do
            msk <- MV.read (stateMask s) i
            return $! case msk of
                Masked -> True
                _      -> False

    modifyC needFlip f crossings =
        withState $ \ !s ->
            forM_ crossings $ \ (Vertex _ c) -> do
                msk <- MV.read (stateMask s) c
                MV.write (stateMask s) c $
                    case msk of
                        Direct t  | needFlip  -> Flipped $ f t
                                  | otherwise -> Direct $ f t
                        Flipped t | needFlip  -> Direct $ f t
                                  | otherwise -> Flipped $ f t
                        Masked                -> error $ printf "EmbeddedLink.modifyC: flipping masked crossing %s" (show c)

    connectC connections =
        withState $ \ !s ->
            forM_ connections $ \ (Dart _ !a, Dart _ !b) -> do
                when (a == b) $ fail $ printf "EmbeddedLink.connectC: %s connect to itself" (show a)
                PMV.write (stateInvolution s) a b
                PMV.write (stateInvolution s) b a

    substituteC substitutions = do
        reconnections <- mapM (\ (a, b) -> (,) a `fmap` oppositeC b) substitutions
        x <- withState $ \ !st -> do
            let source = stateSource st

            arr <- MV.new (numberOfDarts source)
            forM_ (allEdges source) $ \ (!a, !b) -> do
                MV.write arr (dartIndex a) a
                MV.write arr (dartIndex b) b

            forM_ substitutions $ \ (a, b) ->
                if a == b
                    then modifySTRef' (stateCircles st) (+ 1)
                    else MV.write arr (dartIndex b) a

            forM reconnections $ \ (a, b) ->
                (,) a `fmap` MV.read arr (dartIndex b)
        connectC x
