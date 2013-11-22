{-# LANGUAGE RankNTypes #-}
module Math.Algebra.PlanarAlgebra.Reduction
    ( PlanarStateSum(..)
    , PlanarM
    , VertexM
    , StrategyResult(..)
    , Strategy
    , vertexDegreeM
    , oppositeM
    , reduceWithStrategy
    ) where

import Data.Function (fix)
import Data.Monoid (Monoid(..))
import Data.Array.IArray ((!), array, listArray)
import Data.Array.MArray (newArray_, newArray, readArray, writeArray, getBounds)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, unless, forM_, forM, filterM)
import Control.Arrow (first)
import Text.Printf
import Math.Algebra.PlanarAlgebra.Definition


class (Monoid s) => PlanarStateSum s where
    stateDegree   :: s -> Int

    rotateState   :: Int -> s -> s
    mirrorState   :: s -> s

    loopState     :: Int -> (s, Int) -> (s, UArray Int Int)
    connectStates :: Int -> (s, Int) -> (s, Int) -> (s, UArray Int Int, UArray Int Int)
    assemble      :: Array Int (Int, Int) -> Array Int (Array Int Int) -> Array Int s -> s -> s


{-
data VertexM a =
    VertexM
        { degree    :: {-# UNPACK #-} !Int
        , incidence :: {-# UNPACK #-} !(UArray Int Int)
        , value     :: !a
        }

data DartM a =
    DartM
        { incidentVertex :: !(VertexM a)
        , dartPlace      :: {-# UNPACK #-} !Int
        }


data Context s a =
    Context
        { _opposite :: {-# UNPACK #-} !(STArray s Int (DartM a))
        , _free     :: {-# UNPACK #-} !(STRef s Int)
        }


data StrategyResult a = Return a | Contract (DartM a)

type Strategy a = forall s. [DartM a] -> PlanarM s a (StrategyResult a)


reduceWithStrategy :: (PlanarAlgebra a, PlanarState t) => a x -> (Vertex a x -> t) -> Strategy t -> t
reduceWithStrategy alg weight strategy =
    runST $ do
        context <- do
            let (1, n) = vertexIndicesRange alg

                vs = (array (0, n) :: [(Int, a)] -> Array Int a) $
                    flip map (allVertices alg) $ \ v ->
                        let d = vertexDegree v
                            inc = listArray (0, d - 1) $ map dartIndex $ outcomingDarts v
                        in (vertexIndex v, VertexM d inc (weight v))

            opp <- newArray_ (dartIndicesRange alg)
            forM_ (allHalfEdges alg) $ \ d ->
                let (u, p) = endPair' d
                in writeArray opp (dartIndex d) (DartM (vs ! u) p)

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
        readArray (_opposite context) (incidence v ! p)


nextCCWM :: DartM a -> DartM a
nextCCWM (DartM v p) =
    let d = degree v
    in DartM v ((p + 1) `mod` d)


nextCWM :: DartM a -> DartM a
nextCWM (DartM v p) =
    let d = degree v
    in DartM v ((p - 1) `mod` d)
-}


type PlanarM s a = ReaderT (Context s a) (ST s)

newtype VertexM a = VertexM Int deriving (Eq, Show)

type DartM a = (VertexM a, Int)

data StrategyResult a = Contract (DartM a)

type Strategy a = forall s. [DartM a] -> PlanarM s a (StrategyResult a)


vertexDegreeM :: VertexM a -> PlanarM s a Int
vertexDegreeM (VertexM v) =
    ask >>= \ context ->
        lift $ vertexDegreeST context v


oppositeM :: DartM a -> PlanarM s a (DartM a)
oppositeM (VertexM !v, p) = ask >>= \ !s -> lift $ do
    (!u, !q) <- neighbourST s (v, p)
    when (u == 0) $ fail $ printf "touching border at (%i, %i) <-> (%i, %i)" v p u q
    return (VertexM u, q)


reduceWithStrategy :: (PlanarAlgebra a, PlanarStateSum t) => a x -> (Vertex a x -> t) -> Strategy t -> t
reduceWithStrategy alg weight strategy =
    runST $ do
        s <- makeContext alg weight
        fix $ \ continue -> do
            greedyReductionST s
            edges <- internalEdgesST s
            case edges of
                [] -> extractStateSumST s
                _  -> do
                    action <- runReaderT (strategy $ map (first VertexM) edges) s
                    case action of
                        Contract (VertexM v, p) -> contractEdgeST s (v, p)
                    continue


data Context s a =
    Context
        { size     :: !Int
        , alive    :: !(STRef s Int)
        , active   :: !(STUArray s Int Bool)
        , state    :: !(STArray s Int a)
        , adjacent :: !(STArray s Int (STArray s Int (Int, Int)))
        , queue    :: !(STRef s [Int])
        , queued   :: !(STUArray s Int Bool)
        , multiple :: !(STRef s a)
        }


makeContext :: (PlanarAlgebra a, PlanarStateSum t) => a x -> (Vertex a x -> t) -> ST s (Context s t)
makeContext alg weight = do
    s <- do
        let n = numberOfVertices alg
        adjacent' <- newArray_ (0, n)
        newArray_ (0, numberOfLegs alg - 1) >>= writeArray adjacent' 0

        forM_ [1 .. n] $ \ !i ->
            newArray_ (0, 3) >>= writeArray adjacent' i

        alive' <- newSTRef n
        active' <- newArray (1, n) True
        state' <- newArray_ (1, n)
        queue' <- newSTRef [1 .. n]
        queued' <- newArray (1, n) True
        multiple' <- newSTRef mempty

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

    forM_ (allVertices alg) $ \ v -> do
        let w = weight v
        unless (stateDegree w == vertexDegree v) $
            fail $ printf "Bad state degree: %i instead of %i" (stateDegree w) (vertexDegree v)
        setStateSumST s (vertexIndex v) w

    forM_ (allEdges alg) $ \ (a, b) ->
        connectST s (beginPair' a) (beginPair' b)

    return $! s


dumpStateST :: (Show a) => Context s a -> ST s String
dumpStateST s = do
    cross <- forM [1 .. size s] $ \ i -> do
        act <- readArray (active s) i
        if not act
            then return "???"
            else do
                adj <- readArray (adjacent s) i
                (0, bound) <- getBounds adj
                con <- fmap concat $ forM [0 .. bound] $ \ j -> show `fmap` readArray adj j
                st <- readArray (state s) i
                return $ printf "{ %s } %s" con (show st)

    alive' <- readSTRef $ alive s
    multiple' <- readSTRef $ multiple s
    return $ printf "\nalive = %i\nmultiple=%s\n%s" alive' (show multiple') $ unlines cross


appendMultipleST :: (PlanarStateSum a) => Context s a -> a -> ST s ()
appendMultipleST s x =
    modifySTRef' (multiple s) (mappend x)


connectST :: Context s a -> (Int, Int) -> (Int, Int) -> ST s ()
connectST s a@(!v, !p) b@(!u, !q) = do
    readArray (adjacent s) v >>= \ d -> writeArray d p b
    readArray (adjacent s) u >>= \ d -> writeArray d q a


vertexDegreeST :: Context s a -> Int -> ST s Int
vertexDegreeST s v =
    readArray (adjacent s) v >>=
        getBounds >>= \ (0, n) ->
            return $! n + 1


neighbourST :: Context s a -> (Int, Int) -> ST s (Int, Int)
neighbourST s (!v, !p) = do
    x <- readArray (adjacent s) v
    (_, d) <- getBounds x
    readArray x $ p `mod` (d + 1)


killVertexST :: Context s a -> Int -> ST s ()
killVertexST s v = do
    a <- readArray (active s) v
    unless a $ fail "killVertexST: vertex is already dead"
    writeArray (active s) v False
    writeArray (state s) v $ error "do not touch!"
    writeArray (adjacent s) v $ error "do not touch!"
    modifySTRef' (alive s) (+ (-1))


enqueueST :: Context s a -> Int -> ST s ()
enqueueST s v = do
    a <- readArray (active s) v
    e <- readArray (queued s) v
    when (a && not e) $ do
        writeArray (queued s) v True
        modifySTRef' (queue s) (v :)


dequeueST :: Context s a -> ST s (Maybe Int)
dequeueST s = do
    l <- readSTRef $ queue s
    case l of
        []    -> return Nothing
        h : t -> do
            writeSTRef (queue s) t
            writeArray (queued s) h False
            ok <- readArray (active s) h
            if ok
                then return $! Just $! h
                else dequeueST s


getAdjListST :: Context s a -> Int -> ST s (STArray s Int (Int, Int))
getAdjListST s = readArray (adjacent s)


resizeAdjListST :: Context s a -> Int -> Int -> ST s (STArray s Int (Int, Int))
resizeAdjListST s v degree = do
    prev <- readArray (adjacent s) v
    next <- newArray_ (0, degree - 1)
    writeArray (adjacent s) v next
    return $! prev


getStateSumST :: Context s a -> Int -> ST s a
getStateSumST s = readArray (state s)


setStateSumST :: Context s a -> Int -> a -> ST s ()
setStateSumST s = writeArray (state s)


numberOfAliveVerticesST :: Context s a -> ST s Int
numberOfAliveVerticesST s = readSTRef $ alive s


aliveVerticesST :: Context s a -> ST s [Int]
aliveVerticesST s = filterM (readArray $ active s) [1 .. size s]


extractStateSumST :: (PlanarStateSum a) => Context s a -> ST s a
extractStateSumST s = do
    vertices <- aliveVerticesST s
    let n = length vertices

    border <- do
        let ix :: UArray Int Int
            ix = array (0, size s) $ zip vertices [1 ..]
        b <- readArray (adjacent s) 0
        (0, l) <- getBounds b
        list <- forM [0 .. l] $ \ !i -> do
            (v, p) <- readArray b i
            return $! if v == 0 then (0, p) else (ix ! v, p)
        return $! listArray (0, l) list

    connections <- fmap (listArray (1, n)) $ forM vertices $ \ !v -> do
        a <- readArray (adjacent s) v
        (0, k) <- getBounds a
        fmap (listArray (0, k)) $ forM [0 .. k] $ \ !i -> do
            (0, p) <- readArray a i
            return $! p

    internals <- listArray (1, n) `fmap` forM vertices (readArray (state s))
    global <- readSTRef (multiple s)
    return $! assemble border connections internals global


tryGreedyContract :: (PlanarStateSum a) => Context s a -> Int -> ST s Bool
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


contractEdgeST :: (PlanarStateSum a) => Context s a -> (Int, Int) -> ST s ()
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


contract :: (PlanarStateSum a) => Context s a -> (Int, Int) -> (Int, Int) -> ST s Int
contract s (!v, !p) (!u, !q) = do
    degreeV <- vertexDegreeST s v
    degreeU <- vertexDegreeST s u

    sumV <- getStateSumST s v
    sumU <- getStateSumST s u
    let (resultSum, substV, substU) = connectStates 1 (sumV, p) (sumU, q)
    setStateSumST s v resultSum

    do
        prevV <- resizeAdjListST s v $ degreeV + degreeU - 2
        prevU <- getAdjListST s u

        forM_ [0 .. degreeV - 1] $ \ !i -> when (i /= p) $ do
            (w, k) <- readArray prevV i
            connectST s (v, substV ! i) $ case () of
                _ | w == v    -> (v, substV ! k)
                  | w == u    -> (v, substU ! k)
                  | otherwise -> (w, k)

        forM_ [0 .. degreeU - 1] $ \ !i -> when (i /= q) $ do
            (w, k) <- readArray prevU i
            connectST s (v, substU ! i) $ case () of
                _ | w == v    -> (v, substV ! k)
                  | w == u    -> (v, substU ! k)
                  | otherwise -> (w, k)

    killVertexST s u
    return $! v


tryRelaxVertex :: (PlanarStateSum a) => Context s a -> Int -> ST s Bool
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


dissolveVertexST :: (PlanarStateSum a) => Context s a -> Int -> ST s ()
dissolveVertexST s v = do
    stateSum <- getStateSumST s v
    appendMultipleST s stateSum
    killVertexST s v


contractLoopST :: (PlanarStateSum a) => Context s a -> (Int, Int) -> ST s ()
contractLoopST s (!v, !p) = do
    degree <- vertexDegreeST s v
    (!v', !p') <- neighbourST s (v, p)
    when (v == 0 || v' /= v || p' /= (p + 1) `mod` degree) $
        fail $ printf "contractLoopST: not loop (%i, %i) <-> (%i, %i) / %i" v p v' p' degree

    preSum <- getStateSumST s v
    let (postSum, subst) = loopState 1 (preSum, p)
    setStateSumST s v postSum

    prev <- resizeAdjListST s v $ degree - 2
    forM_ [0 .. degree - 1] $ \ !i -> when (i /= p' && i /= p) $ do
        (u, j) <- readArray prev i
        connectST s (v, subst ! i) $ if u /= v then (u, j) else (v, subst ! j)


internalEdgesST :: (PlanarStateSum a) => Context s a -> ST s [(Int, Int)]
internalEdgesST s = do
    vs <- aliveVerticesST s
    fmap concat $ forM vs $ \ v -> do
        d <- vertexDegreeST s v
        fmap concat $ forM [0 .. d - 1] $ \ p -> do
            (u, q) <- neighbourST s (v, p)
            return [(v, p) | u > v || (u == v && q > p)]


greedyReductionST :: (PlanarStateSum a) => Context s a -> ST s ()
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
