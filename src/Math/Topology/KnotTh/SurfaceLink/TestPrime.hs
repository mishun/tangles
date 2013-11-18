module Math.Topology.KnotTh.SurfaceLink.TestPrime
    ( isReducable
    , testPrime
    , has4LegPlanarPart
    ) where

import Data.Ix (Ix)
import Data.Array.MArray (newListArray, newArray, newArray_, readArray, writeArray)
import Data.Array.ST (STUArray, STArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import Control.Monad.ST (ST, runST)
import Control.Monad (when, unless, forM_, liftM2)
import Control.Applicative ((<$>))
import Control.Monad.IfElse (whenM, whileM)
import Math.Topology.KnotTh.SurfaceLink


isReducable :: SurfaceLink ct -> Bool
isReducable link = or $ do
    c <- allVertices link
    a <- outcomingDarts c
    return $! nextCW (opposite a) == opposite (nextCCW a)


testPrime :: SurfaceLink ct -> Bool
testPrime link
    | numberOfVertices link < 2  = True
    | otherwise                  =
        let mincut = stoerWagner link
        in mincut >= 4


cfor :: (Monad m) => (m a, a -> m Bool, a -> m a) -> (a -> m ()) -> m ()
cfor (initial, cond, next) body =
    let loop !i = do
            continue <- cond i
            when continue $ do
                body i
                next i >>= loop
    in initial >>= loop


stoerWagner :: SurfaceLink ct -> Int
stoerWagner link = runST $ do
    let sz = numberOfVertices link
    g <- newArray ((1, 1), (sz, sz)) 0 :: ST s (STUArray s (Int, Int) Int)

    forM_ (allVertices link) $ \ u ->
        forM_ (outcomingDarts u) $ \ d -> do
            let v = endVertex d
                i = (vertexIndex u, vertexIndex v)
            w <- readArray g i
            writeArray g i $! w + 1

    v <- newListArray (1, sz) [1 .. sz] :: ST s (STUArray s Int Int)

    a <- newArray_ (1, sz) :: ST s (STUArray s Int Bool)
    w <- newArray_ (1, sz) :: ST s (STUArray s Int Int)
    na <- newArray_ (1, sz) :: ST s (STUArray s Int Int)

    let setA i x = flip (writeArray a) x =<< readArray v i

    best <- newSTRef $! 1 + numberOfEdges link
    n <- newSTRef sz

    whileM ((> 1) <$> readSTRef n) $ do
        setA 1 True

        do
            n' <- readSTRef n
            forM_ [2 .. n'] $ \ i -> do
                setA i False
                writeArray na (i - 1) i
                writeArray w i =<< readArray g =<<
                    liftM2 (,) (readArray v 1) (readArray v i)

        prev <- newSTRef =<< readArray v 1
        cfor (return 2, \ i -> (i <=) <$> readSTRef n, \ i -> return $! i + 1) $ \ !i -> do
            zj <- do
                n' <- readSTRef n
                zj <- newSTRef (-1)
                forM_ [2 .. n'] $ \ !j ->
                    whenM (not <$> (readArray a =<< readArray v j)) $ do
                        zj' <- readSTRef zj
                        ok <- if zj' < 1
                                then return True
                                else liftM2 (>) (readArray w j) (readArray w zj')
                        when ok $ writeSTRef zj j
                readSTRef zj

            flip (writeArray a) True =<< readArray v zj

            lastIt <- (== i) <$> readSTRef n
            if lastIt
                then do
                    modifySTRef' best =<< (min <$> readArray w zj)
                    n' <- readSTRef n

                    forM_ [1 .. n'] $ \ !j -> do
                        delta <- readArray g =<< liftM2 (,) (readArray v zj) (readArray v j)
                        index <- liftM2 (,) (readSTRef prev) (readArray v j)
                        tmp <- readArray g index
                        writeArray g index $! tmp + delta
                        index' <- liftM2 (,) (readArray v j) (readSTRef prev)
                        writeArray g index' $! tmp + delta

                    writeArray v zj =<< readArray v n'
                    modifySTRef' n (+ (-1))

                else do
                    writeSTRef prev =<< readArray v zj
                    n' <- readSTRef n
                    forM_ [2 .. n'] $ \ !j ->
                        whenM (not <$> (readArray a =<< readArray v j)) $ do
                            delta <- readArray g =<< liftM2 (,) (readArray v zj) (readArray v j)
                            tmp <- readArray w j
                            writeArray w j $! tmp + delta

    readSTRef best


has4LegPlanarPart :: SurfaceLink ct -> Bool
has4LegPlanarPart =
    let planar link start darts = runST $ do
            vertex <- (newArray :: (Ix i) => (i, i) -> Bool -> ST s (STUArray s i Bool)) (verticesRange link) False
            edge <- (newArray :: (Ix i) => (i, i) -> Bool -> ST s (STUArray s i Bool)) (dartsRange link) False

            face <- (newArray :: (Ix i) => (i, i) -> Bool -> ST s (STUArray s i Bool)) (facesRange link) False
            mapM_ (\ !e -> writeArray face (leftFace e) True) darts

            queue <- (newArray_ :: (Ix i) => (i, i) -> ST s (STArray s i a)) (0, numberOfVertices link)
            qtail <- newSTRef 1

            writeArray vertex start True
            writeArray queue 0 start

            nv <- newSTRef 1
            ne <- newSTRef 4
            nf <- newSTRef 4

            let testFace f = do
                    fi <- readArray face f
                    unless fi $ do
                        writeArray face f True
                        modifySTRef' nf (+ 1)

            let testEdge e = do
                    ei <- readArray edge e
                    unless ei $ do
                        writeArray edge e True
                        writeArray edge (opposite e) True
                        modifySTRef' ne (+ 1)

            let testVertex v = do
                    vi <- readArray vertex v
                    unless vi $ do
                        writeArray vertex v True
                        modifySTRef' nv (+ 1)
                        t <- readSTRef qtail
                        writeArray queue t v
                        writeSTRef qtail $! t + 1

            let loop !qhead = do
                    ok <- (qhead <) `fmap` readSTRef qtail
                    when ok $ do
                        v <- readArray queue qhead
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
