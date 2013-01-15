module Math.KnotTh.Invariants.Skein.SkeinM.State
    ( SkeinState
    , relation
    , stateFromKnotted
    , dumpStateST
    , appendMultipleST
    , connectST
    , vertexDegreeST
    , neighbourST
    , killVertexST
    , enqueueST
    , dequeueST
    , getAdjListST
    , resizeAdjListST
    , getStateSumST
    , modifyStateSumST
    , numberOfAliveVerticesST
    , aliveVerticesST
    , extractStateSumST
    ) where

import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Array.Base ((!))
import Data.Array.Unboxed (UArray)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, getBounds)
import Data.Array.ST (STUArray, STArray)
import Data.Array.Unsafe (unsafeFreeze)
import Control.Monad.ST (ST)
import Control.Monad (forM, forM_, when, unless, filterM)
import Text.Printf
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import Math.KnotTh.Invariants.Skein.StateSum
import Math.KnotTh.Invariants.Skein.Knotted
import Math.KnotTh.Invariants.Skein.Relation


data SkeinState s r a = SkeinState
    { relation :: !r
    , size     :: !Int
    , alive    :: !(STRef s Int)
    , active   :: !(STUArray s Int Bool)
    , state    :: !(STArray s Int (StateSum a))
    , adjacent :: !(STArray s Int (STArray s Int (Int, Int)))
    , queue    :: !(STRef s [Int])
    , queued   :: !(STUArray s Int Bool)
    , multiple :: !(STRef s a)
    }


stateFromKnotted :: (SkeinRelation rel a, SkeinKnotted k c d) => rel -> k ArbitraryCrossing -> ST s (SkeinState s rel a)
stateFromKnotted relation' knot = do
    s <- do
        let n = numberOfCrossings knot
        adjacent' <- newArray_ (0, n)
        newArray_ (0, numberOfEndpoints knot - 1) >>= writeArray adjacent' 0
        forM_ [1 .. n] $ \ !i -> newArray_ (0, 3) >>= writeArray adjacent' i

        alive' <- newSTRef n
        active' <- newArray (1, n) True
        state' <- newArray (1, n) $ fromInitialSum $ initialLplus relation'
        queue' <- newSTRef [1 .. n]
        queued' <- newArray (1, n) True
        multiple' <- newSTRef $ circleFactor relation' ^ numberOfFreeLoops knot

        return $! SkeinState
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

    let pair d
            | isEndpoint d                      = (0, endpointPlace d)
            | isOverCrossing (crossingState c)  = (crossingIndex c, p)
            | otherwise                         = (crossingIndex c, (p + 1) `mod` 4)
            where
                (c, p) = begin d

    forM_ (allEdges knot) $ \ (a, b) ->
        connectST s (pair a) (pair b)

    return $! s

{-
copyState :: SkeinState s a -> ST s (SkeinState s a)
copyState s = do
    let n = size s

    alive' <- newSTRef =<< readSTRef (alive s)
    active' <- mapArray id (active s)
    state' <- mapArray id (state s)

    adjacent' <- newArray_ (0, n)
    forM_ [0 .. n] $ \ i ->
        readArray (adjacent s) i >>= mapArray id >>= writeArray adjacent' i

    return $! SkeinState
        { relation = relation s
        , size     = n
        , alive    = alive'
        , active   = active'
        , state    = state'
        , adjacent = adjacent'
        }
-}

dumpStateST :: (Show a) => SkeinState s r a -> ST s String
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
    return $! printf "\nalive = %i\nmultiple=%s\n%s" alive' (show multiple') $ concatMap (++ "\n") cross


appendMultipleST :: (Num a) => SkeinState s r a -> a -> ST s ()
appendMultipleST s x =
    readSTRef (multiple s) >>= \ !m ->
        writeSTRef (multiple s) $! x * m


connectST :: SkeinState s r a -> (Int, Int) -> (Int, Int) -> ST s ()
connectST s a@(!v, !p) b@(!u, !q) = do
    readArray (adjacent s) v >>= \ d -> writeArray d p b
    readArray (adjacent s) u >>= \ d -> writeArray d q a


vertexDegreeST :: SkeinState s r a -> Int -> ST s Int
vertexDegreeST s v = do
    readArray (adjacent s) v >>=
        getBounds >>= \ (0, n) ->
            return $! n + 1


neighbourST :: SkeinState s r a -> (Int, Int) -> ST s (Int, Int)
neighbourST s (!v, !p) = do
    x <- readArray (adjacent s) v
    (_, d) <- getBounds x
    readArray x $ p `mod` (d + 1)


killVertexST :: SkeinState s r a -> Int -> ST s ()
killVertexST s v = do
    a <- readArray (active s) v
    unless a $ fail "killVertexST: vertex is already dead"
    writeArray (active s) v False
    writeArray (state s) v $ error "do not touch!"
    writeArray (adjacent s) v $ error "do not touch!"
    readSTRef (alive s) >>= \ !x ->
        writeSTRef (alive s) $! x - 1


enqueueST :: SkeinState s r a -> Int -> ST s ()
enqueueST s v = do
    a <- readArray (active s) v
    e <- readArray (queued s) v
    when (a && not e) $ do
        writeArray (queued s) v True
        readSTRef (queue s) >>= \ !l -> writeSTRef (queue s) $! v : l


dequeueST :: SkeinState s r a -> ST s (Maybe Int)
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


getAdjListST :: SkeinState s r a -> Int -> ST s (STArray s Int (Int, Int))
getAdjListST s v = readArray (adjacent s) v


resizeAdjListST :: SkeinState s r a -> Int -> Int -> ST s (STArray s Int (Int, Int))
resizeAdjListST s v degree = do
    prev <- readArray (adjacent s) v
    next <- newArray_ (0, degree - 1)
    writeArray (adjacent s) v next
    return $! prev


getStateSumST :: SkeinState s r a -> Int -> ST s (StateSum a)
getStateSumST s v = readArray (state s) v


modifyStateSumST :: SkeinState s r a -> Int -> (StateSum a -> StateSum a) -> ST s ()
modifyStateSumST s v f = do
    sumV <- readArray (state s) v
    writeArray (state s) v $ f sumV


numberOfAliveVerticesST :: SkeinState s r a -> ST s Int
numberOfAliveVerticesST s = readSTRef $ alive s


aliveVerticesST :: SkeinState s r a -> ST s [Int]
aliveVerticesST s = filterM (readArray $ active s) [1 .. size s]


extractStateSumST :: (SkeinRelation r a) => SkeinState s r a -> ST s (StateSum a)
extractStateSumST s = do
    brd <- readArray (adjacent s) 0
    (0, l) <- getBounds brd
    
    st <- newArray_ (0, size s) :: ST s (STArray s Int (UArray Int Int))
    let substState f [] = do
            t <- newArray_ (0, l) :: ST s (STUArray s Int Int)
            forM_ [0 .. l] $ \ !i -> do
                (v, x) <- readArray brd i
                j <- if v == 0
                    then return x
                    else do
                        vt <- readArray st v
                        (0, x') <- neighbourST s (v, vt ! x)
                        return x'
                writeArray t i j
            t' <- unsafeFreeze t
            return [StateSummand t' f]

        substState f (h : t) = do
            stateSum <- getStateSumST s h
            fmap concat $ forM stateSum $ \ (StateSummand ct cf) -> do
                writeArray st h ct
                substState (f * cf) t

    global <- readSTRef $ multiple s
    aliveVerticesST s >>= substState global >>= (return . normalizeStateSum)
