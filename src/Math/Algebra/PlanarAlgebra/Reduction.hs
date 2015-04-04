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
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, unless, forM_, forM, filterM)
import Control.Arrow (first)
import Text.Printf
import Math.Algebra.PlanarAlgebra


class (Monoid s) => PlanarStateSum s where
    stateDegree   :: s -> Int

    rotateState   :: Int -> s -> s
    mirrorState   :: s -> s

    loopState     :: Int -> (s, Int) -> (s, UV.Vector Int)
    connectStates :: Int -> (s, Int) -> (s, Int) -> (s, UV.Vector Int, UV.Vector Int)
    assemble      :: V.Vector (Int, Int) -> V.Vector (UV.Vector Int) -> V.Vector s -> s -> s


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
        , active   :: !(UMV.STVector s Bool)
        , state    :: !(MV.STVector s a)
        , adjacent :: !(MV.STVector s (MV.STVector s (Int, Int)))
        , queue    :: !(STRef s [Int])
        , queued   :: !(UMV.STVector s Bool)
        , multiple :: !(STRef s a)
        }


makeContext :: (PlanarAlgebra a, PlanarStateSum t) => a x -> (Vertex a x -> t) -> ST s (Context s t)
makeContext alg weight = do
    s <- do
        let n = numberOfVertices alg
        adjacent' <- MV.new (n + 1)
        MV.new (numberOfLegs alg) >>= MV.write adjacent' 0

        forM_ [1 .. n] $ \ !i ->
            MV.new 4 >>= MV.write adjacent' i

        alive' <- newSTRef n
        active' <- UMV.replicate (n + 1) True
        state' <- MV.new (n + 1)
        queue' <- newSTRef [1 .. n]
        queued' <- UMV.replicate (n + 1) True
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

{-
dumpStateST :: (Show a) => Context s a -> ST s String
dumpStateST s = do
    cross <- forM [1 .. size s] $ \ i -> do
        act <- UMV.read (active s) i
        if not act
            then return "???"
            else do
                adj <- MV.read (adjacent s) i
                let bound = MV.length adj
                con <- fmap concat $ forM [0 .. bound - 1] $ \ j ->
                    show `fmap` MV.read adj j
                st <- MV.read (state s) i
                return $ printf "{ %s } %s" con (show st)

    alive' <- readSTRef $ alive s
    multiple' <- readSTRef $ multiple s
    return $ printf "\nalive = %i\nmultiple=%s\n%s" alive' (show multiple') $ unlines cross
-}

appendMultipleST :: (PlanarStateSum a) => Context s a -> a -> ST s ()
appendMultipleST s x =
    modifySTRef' (multiple s) (mappend x)


connectST :: Context s a -> (Int, Int) -> (Int, Int) -> ST s ()
connectST s a@(!v, !p) b@(!u, !q) = do
    MV.read (adjacent s) v >>= \ d -> MV.write d p b
    MV.read (adjacent s) u >>= \ d -> MV.write d q a


vertexDegreeST :: Context s a -> Int -> ST s Int
vertexDegreeST s v = do
    MV.length `fmap` MV.read (adjacent s) v


neighbourST :: Context s a -> (Int, Int) -> ST s (Int, Int)
neighbourST s (!v, !p) = do
    x <- MV.read (adjacent s) v
    MV.read x $ p `mod` MV.length x


killVertexST :: Context s a -> Int -> ST s ()
killVertexST s v = do
    a <- UMV.read (active s) v
    unless a $ fail "killVertexST: vertex is already dead"
    UMV.write (active s) v False
    MV.write (state s) v $ error "do not touch!"
    MV.write (adjacent s) v $ error "do not touch!"
    modifySTRef' (alive s) (+ (-1))


enqueueST :: Context s a -> Int -> ST s ()
enqueueST s v = do
    a <- UMV.read (active s) v
    e <- UMV.read (queued s) v
    when (a && not e) $ do
        UMV.write (queued s) v True
        modifySTRef' (queue s) (v :)


dequeueST :: Context s a -> ST s (Maybe Int)
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


getAdjListST :: Context s a -> Int -> ST s (MV.STVector s (Int, Int))
getAdjListST s = MV.read (adjacent s)


resizeAdjListST :: Context s a -> Int -> Int -> ST s (MV.STVector s (Int, Int))
resizeAdjListST s v degree = do
    prev <- MV.read (adjacent s) v
    next <- MV.new degree
    MV.write (adjacent s) v next
    return prev


getStateSumST :: Context s a -> Int -> ST s a
getStateSumST s = MV.read (state s)


setStateSumST :: Context s a -> Int -> a -> ST s ()
setStateSumST s = MV.write (state s)


--numberOfAliveVerticesST :: Context s a -> ST s Int
--numberOfAliveVerticesST s = readSTRef $ alive s


aliveVerticesST :: Context s a -> ST s [Int]
aliveVerticesST s = filterM (UMV.read $ active s) [1 .. size s]


extractStateSumST :: (PlanarStateSum a) => Context s a -> ST s a
extractStateSumST s = do
    vertices <- aliveVerticesST s
    let n = length vertices

    border <- do
        let ix = (UV.replicate (size s + 1) 0) UV.// (vertices `zip` [1 ..])
        b <- MV.read (adjacent s) 0
        let l = MV.length b
        list <- forM [0 .. l - 1] $ \ !i -> do
            (v, p) <- MV.read b i
            return $! if v == 0 then (0, p) else (ix UV.! v, p)
        return $! V.fromListN l list

    connections <- do
        conns <- forM vertices $ \ !v -> do
            a <- MV.read (adjacent s) v
            let k = MV.length a
            fmap (UV.fromListN k) $ forM [0 .. k - 1] $ \ !i -> do
                (0, p) <- MV.read a i
                return $! p
        return $ V.fromListN (n + 1) $ undefined : conns

    internals <- do
         states <- forM vertices (MV.read (state s))
         return $ V.fromListN (n + 1) (undefined : states)

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

        forM_ [0 .. degreeV - 1] $ \ !i ->
            when (i /= p) $ do
                (w, k) <- MV.read prevV i
                connectST s (v, substV UV.! i) $ case () of
                    _ | w == v    -> (v, substV UV.! k)
                      | w == u    -> (v, substU UV.! k)
                      | otherwise -> (w, k)

        forM_ [0 .. degreeU - 1] $ \ !i ->
            when (i /= q) $ do
                (w, k) <- MV.read prevU i
                connectST s (v, substU UV.! i) $ case () of
                    _ | w == v    -> (v, substV UV.! k)
                      | w == u    -> (v, substU UV.! k)
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
        (u, j) <- MV.read prev i
        connectST s (v, subst UV.! i) $
            if u /= v
                then (u, j)
                else (v, subst UV.! j)


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
