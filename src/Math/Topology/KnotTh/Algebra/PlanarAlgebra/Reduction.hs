{-# LANGUAGE MultiWayIf, RankNTypes #-}
module Math.Topology.KnotTh.Algebra.PlanarAlgebra.Reduction
    ( reducePlanarAlgebra
    ) where

import Control.Arrow (first)
import Control.Monad (filterM, foldM, forM, forM_, unless, when)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.ST as ST
import Data.Function (fix)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf
import Math.Topology.KnotTh.Algebra.Dihedral
import Math.Topology.KnotTh.Algebra.PlanarAlgebra


{-
data VertexM a =
    VertexM
        { incidence :: !(UV.Vector Int)
        , value     :: !a
        }

data DartM a =
    DartM
        { incidentVertex :: !(VertexM a)
        , dartPlace      :: {-# UNPACK #-} !Int
        }


data Context s a =
    Context
        { _opposite :: {-# UNPACK #-} !(MV.STVector s (DartM a))
        , _free     :: {-# UNPACK #-} !(STRef s Int)
        }


data StrategyResult a = Return a | Contract (DartM a)

type Strategy a = forall s. [DartM a] -> PlanarM s a (StrategyResult a)


reduceWithStrategy :: (PlanarAlgebra a, PlanarState t) => a x -> (Vertex a x -> t) -> Strategy t -> t
reduceWithStrategy alg weight strategy =
    runST $ do
        context <- do
            let (1, n) = vertexIndicesRange alg

                vs = (V.replicate (n + 1) undefined V.//) $
                    flip map (allVertices alg) $ \ v ->
                        let d = vertexDegree v
                            inc = UV.fromListN d $ map dartIndex $ outcomingDarts v
                        in (vertexIndex v, VertexM d inc (weight v))

            opp <- MV.new (numberOfDarts alg)
            forM_ (allHalfEdges alg) $ \ d ->
                let (u, p) = endPair' d
                in MV.write opp (dartIndex d) (DartM (vs ! u) p)

            fi <- newSTRef $ 1 + snd (vertexIndicesRange alg)
            return Context { _opposite = opp, _free = fi }

        flip runReaderT context $
            fix $ \ continue -> do
                action <- strategy []
                case action of
                    Return r   -> return r
                    Contract _ -> continue


oppositeM :: DartM a -> PlanarM s a (DartM a)
oppositeM (DartM v p) =
    ask >>= \ context -> lift $
        MV.read (_opposite context) (incidence v UV.! p)


nextCCWM :: DartM a -> DartM a
nextCCWM (DartM v p) =
    let d = degree v
    in DartM v ((p + 1) `mod` d)


nextCWM :: DartM a -> DartM a
nextCWM (DartM v p) =
    let d = degree v
    in DartM v ((p - 1) `mod` d)
-}


type PlanarM s a = Reader.ReaderT (Context s a) (ST.ST s)

newtype VertexM a = VertexM Int deriving (Eq, Show)

type DartM a = (VertexM a, Int)

data StrategyResult a = Contract (DartM a)

type Strategy a = forall s. [DartM a] -> PlanarM s a (StrategyResult a)


oppositeM :: DartM a -> PlanarM s a (DartM a)
oppositeM (VertexM !v, p) = Reader.ask >>= \ !s -> Reader.lift $ do
    (!u, !q) <- neighbourST s (v, p)
    when (u == 0) $ fail $ printf "touching border at (%i, %i) <-> (%i, %i)" v p u q
    return (VertexM u, q)


reducePlanarAlgebra :: (LeggedDiagram d, VertexDiagram d, PlanarAlgebra a) => d a -> a
reducePlanarAlgebra =
    reduceWithStrategy $
        let try [] = error "standardReductionStrategy: no reduction places left"
            try ((v, i) : t) = do
                (u, _) <- oppositeM (v, i)
                if u /= v
                    then return $! Contract (v, i)
                    else try t
        in try


reduceWithStrategy :: (LeggedDiagram d, VertexDiagram d, PlanarAlgebra a) => Strategy a -> d a -> a
reduceWithStrategy strategy diagram =
    ST.runST $ do
        context <- do
            s <- do
                let n = numberOfVertices diagram
                adjacent' <- MV.new (n + 1)
                MV.new (numberOfLegs diagram) >>= MV.write adjacent' 0

                forM_ [1 .. n] $ \ !i ->
                    MV.new 4 >>= MV.write adjacent' i

                alive' <- newSTRef n
                active' <- UMV.replicate (n + 1) True
                state' <- MV.new (n + 1)
                queue' <- newSTRef [1 .. n]
                queued' <- UMV.replicate (n + 1) True
                multiple' <- newSTRef planarEmpty

                return Context
                    { size     = n
                    , alive    = alive'
                    , active   = active'
                    , state    = state'
                    , adjacent = adjacent'
                    , queue    = queue'
                    , queued   = queued'
                    , multiple = multiple'
                    }

            forM_ (allVertices diagram) $ \ v -> do
                let w = vertexContent v
                unless (planarDegree w == vertexDegree v) $
                    fail $ printf "Bad state degree: %i instead of %i" (planarDegree w) (vertexDegree v)
                setStateSumST s (vertexIndex v) w

            forM_ (allEdges diagram) $ \ (a, b) ->
                connectST s (beginPair' a) (beginPair' b)

            return $! s

        fix $ \ continue -> do
            greedyReductionST context
            edges <- internalEdgesST context
            case edges of
                [] -> extractStateSumST context
                _  -> do
                    action <- Reader.runReaderT (strategy $ map (first VertexM) edges) context
                    case action of
                        Contract (VertexM v, p) -> contractEdgeST context (v, p)
                    continue


data Context s a =
    Context
        { size     :: !Int
        , alive    :: !(STRef s Int)
        , active   :: !(UMV.STVector s Bool)
        , state    :: !(MV.STVector s a)
        , adjacent :: !(MV.STVector s (MV.STVector s (Int, Int)))
        , queue    :: !(STRef s [Int])
        , queued   :: !(UMV.STVector s Bool)
        , multiple :: !(STRef s a)
        }


appendMultipleST :: (PlanarAlgebra a) => Context s a -> a -> ST.ST s ()
appendMultipleST s b =
    modifySTRef' (multiple s) $ \ !a ->
        horizontalComposition 0 (a, 0) (b, 0)


connectST :: Context s a -> (Int, Int) -> (Int, Int) -> ST.ST s ()
connectST s a@(!v, !p) b@(!u, !q) = do
    MV.read (adjacent s) v >>= \ d -> MV.write d p b
    MV.read (adjacent s) u >>= \ d -> MV.write d q a


vertexDegreeST :: Context s a -> Int -> ST.ST s Int
vertexDegreeST s v =
    MV.length `fmap` MV.read (adjacent s) v


neighbourST :: Context s a -> (Int, Int) -> ST.ST s (Int, Int)
neighbourST s (!v, !p) = do
    x <- MV.read (adjacent s) v
    MV.read x $ p `mod` MV.length x


killVertexST :: Context s a -> Int -> ST.ST s ()
killVertexST s v = do
    a <- UMV.read (active s) v
    unless a $ fail "killVertexST: vertex is already dead"
    UMV.write (active s) v False
    MV.write (state s) v $ error "do not touch!"
    MV.write (adjacent s) v $ error "do not touch!"
    modifySTRef' (alive s) (+ (-1))


enqueueST :: Context s a -> Int -> ST.ST s ()
enqueueST s v = do
    a <- UMV.read (active s) v
    e <- UMV.read (queued s) v
    when (a && not e) $ do
        UMV.write (queued s) v True
        modifySTRef' (queue s) (v :)


dequeueST :: Context s a -> ST.ST s (Maybe Int)
dequeueST s = do
    l <- readSTRef $ queue s
    case l of
        []    -> return Nothing
        h : t -> do
            writeSTRef (queue s) t
            UMV.write (queued s) h False
            ok <- UMV.read (active s) h
            if ok
                then return $! Just $! h
                else dequeueST s


getAdjListST :: Context s a -> Int -> ST.ST s (MV.STVector s (Int, Int))
getAdjListST s = MV.read (adjacent s)


resizeAdjListST :: Context s a -> Int -> Int -> ST.ST s (MV.STVector s (Int, Int))
resizeAdjListST s v degree = do
    prev <- MV.read (adjacent s) v
    next <- MV.new degree
    MV.write (adjacent s) v next
    return prev


getStateSumST :: Context s a -> Int -> ST.ST s a
getStateSumST s = MV.read (state s)


setStateSumST :: Context s a -> Int -> a -> ST.ST s ()
setStateSumST s = MV.write (state s)


aliveVerticesST :: Context s a -> ST.ST s [Int]
aliveVerticesST s = filterM (UMV.read $ active s) [1 .. size s]


extractStateSumST :: (PlanarAlgebra a) => Context s a -> ST.ST s a
extractStateSumST s = do
    brd <- MV.read (adjacent s) 0
    globalFactor <- readSTRef (multiple s)
    let legsN = MV.length brd

    visited <- UMV.replicate legsN False
    (rotCredit, res) <-
        foldM (\ (!rotCredit, !part) !leg -> do
                vis <- UMV.read visited leg
                if vis
                    then return (rotCredit + 1, part)
                    else do
                        (v, pos) <- MV.read brd leg
                        case v of
                            0 -> do
                                UMV.write visited leg True
                                UMV.write visited pos True
                                return (0, horizontalComposition 0 (planarPropagator 1, 0) (part, 1 + rotCredit))

                            _ -> do
                                adj <- MV.read (adjacent s) v
                                forM_ [0 .. MV.length adj - 1] $ \ !i -> do
                                    (0, p') <- MV.read adj i
                                    UMV.write visited p' True
                                ex <- MV.read (state s) v
                                return (0, horizontalComposition 0 (ex, pos) (part, 1 + rotCredit))
            ) (0, globalFactor) [0 .. legsN - 1]

    return $! rotateBy (-1 - rotCredit) res


tryGreedyContract :: (PlanarAlgebra a) => Context s a -> Int -> ST.ST s Bool
tryGreedyContract s v = do
    vd <- vertexDegreeST s v
    let tryContract !p
            | p >= vd    = return False
            | otherwise  = do
                (u, q) <- neighbourST s (v, p)
                if u == 0 then tryContract (p + 1) else do
                    ud <- vertexDegreeST s u
                    let go !w !i !j
                            | w >= vd    = return $! vd
                            | otherwise  = do
                                nb <- neighbourST s (v, i)
                                if nb == (u, j)
                                    then go (w + 1) ((i + 1) `mod` vd) ((j - 1) `mod` ud)
                                    else return $! w

                    w <- go 0 p q
                    if ud + vd - 2 * w <= max vd ud
                        then contractEdgeST s (v, p) >> return True
                        else tryContract (p + w)
    tryContract 0


contractEdgeST :: (PlanarAlgebra a) => Context s a -> (Int, Int) -> ST.ST s ()
contractEdgeST s (!v, !p) = do
    (!u, !q) <- neighbourST s (v, p)
    when (v == 0 || u == 0 || v == u) $ fail $
        printf "contract: can not contract (%i, %i) <-> (%i, %i)" v p u q

    degreeV <- vertexDegreeST s v
    degreeU <- vertexDegreeST s u
    enqueueST s =<<
        if degreeV >= degreeU
            then contract s (v, p) (u, q)
            else contract s (u, q) (v, p)


contract :: (PlanarAlgebra a) => Context s a -> (Int, Int) -> (Int, Int) -> ST.ST s Int
contract s (!v, !p) (!u, !q) = do
    degreeV <- vertexDegreeST s v
    degreeU <- vertexDegreeST s u

    sumV <- getStateSumST s v
    sumU <- getStateSumST s u

    let gl = 1
    setStateSumST s v $! horizontalComposition gl (sumV, p) (sumU, q)

    let !substV = UV.create $ do
            a <- UMV.replicate degreeV (-1)
            forM_ [0 .. degreeV - 2] $ \ !i ->
                UMV.write a ((p + 1 + i) `mod` degreeV) i
            return a

        !substU = UV.create $ do
            a <- UMV.replicate degreeU (-1)
            forM_ [0 .. degreeU - 2] $ \ !i ->
                UMV.write a ((q + 1 + i) `mod` degreeU) (degreeV - 1 + i)
            return a

    do
        prevV <- resizeAdjListST s v $ degreeV + degreeU - 2
        prevU <- getAdjListST s u

        forM_ [0 .. degreeV - 1] $ \ !i ->
            when (i /= p) $ do
                (w, k) <- MV.read prevV i
                connectST s (v, substV UV.! i) $
                    if | w == v    -> (v, substV UV.! k)
                       | w == u    -> (v, substU UV.! k)
                       | otherwise -> (w, k)

        forM_ [0 .. degreeU - 1] $ \ !i ->
            when (i /= q) $ do
                (w, k) <- MV.read prevU i
                connectST s (v, substU UV.! i) $
                    if | w == v    -> (v, substV UV.! k)
                       | w == u    -> (v, substU UV.! k)
                       | otherwise -> (w, k)

    killVertexST s u
    return $! v


tryRelaxVertex :: (PlanarAlgebra a) => Context s a -> Int -> ST.ST s Bool
tryRelaxVertex s v = do
    degree <- vertexDegreeST s v
    case degree of
        0 -> dissolveVertexST s v >> return True

        _ -> do
            let findLoop [] = return False
                findLoop (i : t) = do
                    (u, j) <- neighbourST s (v, i)
                    if u /= v || j /= (i + 1) `mod` degree
                        then findLoop t
                        else do
                            contractLoopST s (v, i)
                            enqueueST s v
                            return True
            findLoop [0 .. degree - 1]


dissolveVertexST :: (PlanarAlgebra a) => Context s a -> Int -> ST.ST s ()
dissolveVertexST s v = do
    stateSum <- getStateSumST s v
    appendMultipleST s stateSum
    killVertexST s v


contractLoopST :: (PlanarAlgebra a) => Context s a -> (Int, Int) -> ST.ST s ()
contractLoopST s (!v, !p) = do
    degree <- vertexDegreeST s v
    (!v', !p') <- neighbourST s (v, p)
    when (v == 0 || v' /= v || p' /= (p + 1) `mod` degree) $
        fail $ printf "contractLoopST: not loop (%i, %i) <-> (%i, %i) / %i" v p v' p' degree

    preSum <- getStateSumST s v
    setStateSumST s v $! horizontalLooping 1 (preSum, p)

    let subst = UV.create $ do
                a <- UMV.replicate degree (-1)
                forM_ [0 .. degree - 3] $ \ !i ->
                    UMV.write a ((p + 2 + i) `mod` degree) i
                return a

    prev <- resizeAdjListST s v $ degree - 2
    forM_ [0 .. degree - 1] $ \ !i -> when (i /= p' && i /= p) $ do
        (u, j) <- MV.read prev i
        connectST s (v, subst UV.! i) $
            if u /= v
                then (u, j)
                else (v, subst UV.! j)


internalEdgesST :: (PlanarAlgebra a) => Context s a -> ST.ST s [(Int, Int)]
internalEdgesST s = do
    vs <- aliveVerticesST s
    fmap concat $ forM vs $ \ v -> do
        d <- vertexDegreeST s v
        fmap concat $ forM [0 .. d - 1] $ \ p -> do
            (u, q) <- neighbourST s (v, p)
            return [(v, p) | u > v || (u == v && q > p)]


greedyReductionST :: (PlanarAlgebra a) => Context s a -> ST.ST s ()
greedyReductionST s = do
    mv <- dequeueST s
    case mv of
        Nothing -> return ()
        Just v  -> do
            let tryReductions [] = return ()
                tryReductions (h : t) = do
                    r <- h s v
                    unless r $ tryReductions t

            tryReductions [tryRelaxVertex, tryGreedyContract]
            greedyReductionST s
