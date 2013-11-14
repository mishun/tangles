module Math.Topology.KnotTh.Invariants.Skein.SkeinM.State
    ( SkeinState
    , relation
    , stateFromKnotted
    , dumpStateST
    , appendMultipleST
    , connectST
    , vertexDegreeST
    , vertexRankST
    , neighbourST
    , killVertexST
    , enqueueST
    , dequeueST
    , getAdjListST
    , resizeAdjListST
    , getStateSumST
    , setStateSumST
    , numberOfAliveVerticesST
    , aliveVerticesST
    , extractStateSumST
    ) where

import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')
import Data.Array.IArray ((!), array, listArray)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, getBounds)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray, STArray)
import Control.Monad.ST (ST)
import Control.Monad (forM, forM_, when, unless, filterM)
import Text.Printf
import Math.Topology.KnotTh.Crossings.Arbitrary
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Invariants.Skein.Relation


data (SkeinRelation r a) => SkeinState s r a = SkeinState
    { relation :: !r
    , size     :: !Int
    , alive    :: !(STRef s Int)
    , active   :: !(STUArray s Int Bool)
    , state    :: !(STArray s Int (SkeinRelationModel r a))
    , adjacent :: !(STArray s Int (STArray s Int (Int, Int)))
    , queue    :: !(STRef s [Int])
    , queued   :: !(STUArray s Int Bool)
    , multiple :: !(STRef s a)
    }


stateFromKnotted :: (SkeinRelation rel a, SkeinStructure k) => rel -> k ArbitraryCrossing -> ST s (SkeinState s rel a)
stateFromKnotted relation' knot = do
    s <- do
        let n = numberOfCrossings knot
        adjacent' <- newArray_ (0, n)
        newArray_ (0, numberOfEndpoints knot - 1) >>= writeArray adjacent' 0
        forM_ [1 .. n] $ \ !i -> newArray_ (0, 3) >>= writeArray adjacent' i

        alive' <- newSTRef n
        active' <- newArray (1, n) True
        state' <- newArray (1, n) $ initialize relation' $ initialLplus relation'
        queue' <- newSTRef [1 .. n]
        queued' <- newArray (1, n) True
        multiple' <- newSTRef $ circleFactor relation' ^ numberOfFreeLoops knot

        return SkeinState
            { relation = relation'
            , size     = n
            , alive    = alive'
            , active   = active'
            , state    = state'
            , adjacent = adjacent'
            , queue    = queue'
            , queued   = queued'
            , multiple = multiple'
            }

    let pair d | isEndpoint d                      = (0, endpointPlace d)
               | isOverCrossing (crossingState c)  = (crossingIndex c, p)
               | otherwise                         = (crossingIndex c, (p + 1) `mod` 4)
            where
                (c, p) = begin d

    forM_ (allEdges knot) $ \ (a, b) ->
        connectST s (pair a) (pair b)

    return $! s


dumpStateST :: (SkeinRelation r a, Show (SkeinRelationModel r a)) => SkeinState s r a -> ST s String
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


appendMultipleST :: (SkeinRelation r a) => SkeinState s r a -> a -> ST s ()
appendMultipleST s x =
    modifySTRef' (multiple s) (* x)


connectST :: (SkeinRelation r a) => SkeinState s r a -> (Int, Int) -> (Int, Int) -> ST s ()
connectST s a@(!v, !p) b@(!u, !q) = do
    readArray (adjacent s) v >>= \ d -> writeArray d p b
    readArray (adjacent s) u >>= \ d -> writeArray d q a


vertexDegreeST :: (SkeinRelation r a) => SkeinState s r a -> Int -> ST s Int
vertexDegreeST s v =
    readArray (adjacent s) v >>=
        getBounds >>= \ (0, n) ->
            return $! n + 1


vertexRankST :: (SkeinRelation r a) => SkeinState s r a -> Int -> ST s Int
vertexRankST s v =
    readArray (state s) v >>= \ st ->
        return $! complexityRank st


neighbourST :: (SkeinRelation r a) => SkeinState s r a -> (Int, Int) -> ST s (Int, Int)
neighbourST s (!v, !p) = do
    x <- readArray (adjacent s) v
    (_, d) <- getBounds x
    readArray x $ p `mod` (d + 1)


killVertexST :: (SkeinRelation r a) => SkeinState s r a -> Int -> ST s ()
killVertexST s v = do
    a <- readArray (active s) v
    unless a $ fail "killVertexST: vertex is already dead"
    writeArray (active s) v False
    writeArray (state s) v $ error "do not touch!"
    writeArray (adjacent s) v $ error "do not touch!"
    modifySTRef' (alive s) (+ (-1))


enqueueST :: (SkeinRelation r a) => SkeinState s r a -> Int -> ST s ()
enqueueST s v = do
    a <- readArray (active s) v
    e <- readArray (queued s) v
    when (a && not e) $ do
        writeArray (queued s) v True
        modifySTRef' (queue s) (v :)


dequeueST :: (SkeinRelation r a) => SkeinState s r a -> ST s (Maybe Int)
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


getAdjListST :: (SkeinRelation r a) => SkeinState s r a -> Int -> ST s (STArray s Int (Int, Int))
getAdjListST s = readArray (adjacent s)


resizeAdjListST :: (SkeinRelation r a) => SkeinState s r a -> Int -> Int -> ST s (STArray s Int (Int, Int))
resizeAdjListST s v degree = do
    prev <- readArray (adjacent s) v
    next <- newArray_ (0, degree - 1)
    writeArray (adjacent s) v next
    return $! prev


getStateSumST :: (SkeinRelation r a) => SkeinState s r a -> Int -> ST s (SkeinRelationModel r a)
getStateSumST s = readArray (state s)


setStateSumST :: (SkeinRelation r a) => SkeinState s r a -> Int -> SkeinRelationModel r a -> ST s ()
setStateSumST s = writeArray (state s)


numberOfAliveVerticesST :: (SkeinRelation r a) => SkeinState s r a -> ST s Int
numberOfAliveVerticesST s = readSTRef $ alive s


aliveVerticesST :: (SkeinRelation r a) => SkeinState s r a -> ST s [Int]
aliveVerticesST s = filterM (readArray $ active s) [1 .. size s]


extractStateSumST :: (SkeinRelation r a) => SkeinState s r a -> ST s (SkeinRelationModel r a)
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
    return $! assemble (relation s) border connections internals global