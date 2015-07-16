{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
module Math.Topology.KnotTh.Link
    ( module Math.Topology.KnotTh.Knotted
    , module Math.Topology.KnotTh.Knotted.Crossings.Projection
    , module Math.Topology.KnotTh.Knotted.Crossings.Diagram
    , Link
    , LinkProjection
    , LinkProjectionVertex
    , LinkProjectionDart
    , LinkDiagram
    , LinkDiagramVertex
    , LinkDiagramDart
    , emptyLink
    , tangleToLink
    , tangleDoublingLink
    , toDTCode
    , fromDTCode
    , toGaussCode
    , fromGaussCode
    , linkTable
    , knotTable
    , unlink
    , unknot
    , singleCrossingUnknot
    , hopfLink
    , leftTrefoilKnot
    , rightTrefoilKnot
    , figureEightKnot
    , leftCinquefoilKnot
    , rightCinquefoilKnot
    , threeTwistKnot
    , whiteheadLink
    , grannyKnot
    , squareKnot
    , stevedoreKnot
    , borromeanRingsLink
    , conwayKnot
    , kinoshitaTerasakaKnot
    ) where

import Control.Arrow ((***))
import Control.Monad (foldM_, forM, forM_, liftM2, unless, when)
import qualified Control.Monad.ST as ST
import qualified Data.Array.ST as STArray
import qualified Data.Array.Unboxed as A
import Data.List (mapAccumL, findIndices)
import qualified Data.Map as M

import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Text.Printf
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Crossings.Projection
import Math.Topology.KnotTh.Knotted.Crossings.Diagram
import Math.Topology.KnotTh.Knotted.Threads
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link.TableOfCodes


newtype Link a = L (Tangle a)
    deriving (Functor, KnottedDiagram, Surgery, MirrorAction, AsTangle)


instance DartDiagram Link where
    newtype Dart Link a = D (Dart Tangle a)

    dartOwner (D d) = L (dartOwner d)
    dartIndex (D d) = dartIndex d
    opposite (D d) = D (opposite d)

    nextCCW (D d) = D (nextCCW d)
    nextCW (D d) = D (nextCW d)
    nextBy n (D d) = D (nextBy n d)

    numberOfEdges (L t) = numberOfEdges t
    numberOfDarts (L t) = numberOfDarts t
    nthDart (L t) n = D (nthDart t n)
    allEdges (L t) = map (D *** D) (allEdges t)
    allDarts (L t) = map D (allDarts t)

    dartIndicesRange (L t) = dartIndicesRange t

instance VertexDiagram Link where
    numberOfVertices (L t) = numberOfVertices t
    nthVertex (L t) n = V (nthVertex t n)
    allVertices (L t) = map V (allVertices t)

    newtype Vertex Link a = V (Vertex Tangle a)

    vertexDegree (V v) = vertexDegree v
    vertexOwner (V v) = L (vertexOwner v)
    vertexIndex (V v) = vertexIndex v
    nthOutcomingDart (V v) n = D (nthOutcomingDart v n)
    outcomingDarts (V v) = map D (outcomingDarts v)

    maybeBeginVertex (D d) = V `fmap` maybeBeginVertex d
    beginVertex (D d) = V (beginVertex d)
    beginPlace (D d) = beginPlace d

    vertexIndicesRange (L t) = vertexIndicesRange t

instance Knotted Link where
    vertexCrossing (V v) = vertexCrossing v

    mapCrossings f (L t) = L $ mapCrossings (f . V) t

    numberOfFreeLoops (L t) = numberOfFreeLoops t

    changeNumberOfFreeLoops loops (L t) = L $ changeNumberOfFreeLoops loops t

    emptyKnotted = L emptyKnotted

    type ExplodeType Link a = (Int, [([(Int, Int)], a)])
    explode (L t) = let (f, [], l) = explode t in (f, l)
    implode (f, l) = L (implode (f, [], l))

    unrootedHomeomorphismInvariant (L t) = unrootedHomeomorphismInvariant t

    isConnected (L t) = isConnected t

instance (Crossing a) => KnotWithPrimeTest Link a where
    isPrime (L t) = isPrime t


instance (Show a) => Show (Link a) where
    show = printf "implode %s" . show . explode


instance (Show a) => Show (Vertex Link a) where
    show v =
        printf "(Crossing %i %s [ %s ])"
            (vertexIndex v)
            (show $ vertexCrossing v)
            (unwords $ map (show . opposite) $ outcomingDarts v)


instance Show (Dart Link a) where
    show d = let (c, p) = beginPair' d
             in printf "(Dart %i %i)" c p


type LinkProjection = Link ProjectionCrossing
type LinkProjectionVertex = Vertex Link ProjectionCrossing
type LinkProjectionDart = Dart Link ProjectionCrossing


type LinkDiagram = Link DiagramCrossing
type LinkDiagramVertex = Vertex Link DiagramCrossing
type LinkDiagramDart = Dart Link DiagramCrossing


emptyLink :: Link a
emptyLink = emptyKnotted


{-# INLINE tangleToLink #-}
tangleToLink :: Tangle a -> Link a
tangleToLink t | l == 0     = L t
               | otherwise  = error $ printf "tangleToLink: expected 0 legs, found %i" l
    where l = numberOfLegs t


tangleDoublingLink :: (Crossing a) => (a -> a) -> Tangle a -> Link a
tangleDoublingLink f t =
    let l = numberOfLegs t
        t' = mirrorIt $ fmap f t
    in tangleToLink $ horizontalComposition l (t, 0) (t', 1)


toDTCode :: LinkDiagram -> [[Int]]
toDTCode _ = error "toDTCode: not implemented"


fromDTCode :: [[Int]] -> LinkDiagram
fromDTCode code =
    let common = concat code
        sz = length common

        a :: A.UArray Int Int
        a = A.array (1, 2 * sz) $ do
            (i, x) <- zip [0 ..] common
            when (odd x) $ error $
                printf "fromDTCode: at %s: all numbers must be even, but %i is not" (show code) x
            when (abs x < 1 || abs x > 2 * sz) $ error $
                printf "fromDTCode: at %s: absolute value of %i is out of bounds (1, %i)" (show code) x (2 * sz)
            [(abs x, x), (2 * i + 1, -x)]

    in fromGaussCode $ snd $ mapAccumL
        (\ !i !thread ->
            let n = 2 * length thread
            in (i + n, map ((a A.!) . (i +)) [0 .. n - 1])
        ) 1 code


toGaussCode :: LinkDiagram -> [[Int]]
toGaussCode link =
    let encode d = vertexIndex (beginVertex d) * (if passOver d then 1 else -1)
    in map (map (encode . snd)) $ allThreads link


fromGaussCode :: [[Int]] -> LinkDiagram
fromGaussCode = decode . simplifyGaussCode


splice :: Int -> [[Int]] -> [[Int]]
splice i = go
    where
        go [] = error "internal error: not found"
        go (thread : threads) =
            case span ((/= i) . abs) thread of
                (_, [])     -> thread : go threads
                (pref, suf) ->
                    case span ((/= i) . abs) $ tail suf of
                        (_, [])   ->
                            let (x, r) = match threads
                            in (pref ++ [head suf] ++ x ++ tail suf) : r
                        (int, tl) ->
                            let r = reverse $ map (\ j -> if abs j < abs i then -j else j) int
                            in (pref ++ [head suf] ++ r ++ [-head tl] ++ tail tl) : threads

        match [] = error "internal error: not found"
        match (thread : threads) =
            case span ((/= i) . abs) thread of
                (_, [])     ->
                    let (a, b) = match threads
                    in (a, thread : b)
                (pref, suf) -> (tail suf ++ pref ++ [head suf], threads)


decode :: (Int, [[Int]]) -> LinkDiagram
decode (n, threads) = implode (length $ filter null threads, incidence)
    where
        chords = foldl (\ l i -> splice i l) (filter (not . null) threads) [1 .. n]

        color = STArray.runSTUArray $ do
            vis <- STArray.newArray (1, n) False :: ST.ST s (STArray.STUArray s Int Bool)
            col <- STArray.newArray_ (1, n) :: ST.ST s (STArray.STUArray s Int Bool)

            let edge [] _ _ = False
                edge (h : t) i j =
                    case (findIndices ((== i) . abs) h, findIndices ((== j) . abs) h) of
                        ([], [])         -> edge t i j
                        ([a, b], [c, d]) -> (a < c && b > c && b < d) || (a > c && a < d && b > d)
                        _                -> False

            let dfs c i = do
                    v <- STArray.readArray vis i
                    if v
                        then do
                            c' <- STArray.readArray col i
                            when (c' /= c) $ fail "fromGaussCode: gauss code is not planar"
                        else do
                            STArray.writeArray vis i True
                            STArray.writeArray col i c
                            forM_ [1 .. n] $ \ j ->
                                when (edge chords i j) $ dfs (not c) j

            forM_ [1 .. n] $ \ !i -> do
                v <- STArray.readArray vis i
                unless v $ dfs False i

            return $! col

        incidence = ST.runST $ do
            conn <- STArray.newArray_ ((1, 0), (n, 3)) :: ST.ST s (STArray.STArray s (Int, Int) (Int, Int))
            state <- STArray.newArray_ (1, n) :: ST.ST s (STArray.STArray s Int DiagramCrossing)

            let connect a b = STArray.writeArray conn a b >> STArray.writeArray conn b a

            let connection second out i =
                    let j | not second && not out        = 0
                          | second     && not out        = 2
                          | (color A.! abs i) == second  = 3
                          | otherwise                    = 1
                    in (abs i, j)

            vis <- STArray.newArray (1, n) False :: ST.ST s (STArray.STUArray s Int Bool)
            forM_ chords $ \ chord -> do
                foldM_ (\ prev i -> do
                    second <- STArray.readArray vis $ abs i
                    STArray.writeArray vis (abs i) True
                    connect prev (connection second False i)

                    let crossing = overCrossingIf (i > 0)

                    if second
                        then STArray.readArray state (abs i) >>= flip when (fail "gauss code internal error") . (/= crossing)
                        else STArray.writeArray state (abs i) crossing

                    return $! connection second True i
                    ) (connection True True $ last chord) chord

            forM [1 .. n] $ \ i -> liftM2 (,)
                (forM [0 .. 3] $ \ j -> STArray.readArray conn (i, j))
                (STArray.readArray state i)


simplifyGaussCode :: [[Int]] -> (Int, [[Int]])
simplifyGaussCode code = ST.runST $ do
    let n = sum $ flip map code $ \ x ->
            let n2 = length x
            in if even n2
                then n2 `div` 2
                else error "fromGaussCode: lengths must be even"

    index <- do
        free <- newSTRef 1
        indices <- newSTRef $ M.empty
        return $! \ !x -> do
            m <- readSTRef indices
            if M.member x m
                then return $! m M.! x
                else do
                    y <- readSTRef free
                    when (y > n) $ fail "fromGaussCode: too many different values in gauss code"
                    writeSTRef free $! y + 1
                    writeSTRef indices $! M.insert x y m
                    return $! y

    visitedP <- STArray.newArray (1, n) False :: ST.ST s (STArray.STUArray s Int Bool)
    visitedN <- STArray.newArray (1, n) False :: ST.ST s (STArray.STUArray s Int Bool)

    let pickV x | x > 0      = visitedP
                | x < 0      = visitedN
                | otherwise  = error "fromGaussCode: zero presented"

    simplified <-
        forM code $ mapM $ \ !raw -> do
            i <- index $ abs raw
            STArray.readArray (pickV raw) i >>= \ v ->
                when v $ fail $ "fromGaussCode: duplication on " ++ show raw
            STArray.writeArray (pickV raw) i True
            return $! i * signum raw

    return $! (n, simplified)


linkTable :: Int -> Int -> [LinkDiagram]
linkTable cross comps =
    maybe [] (map fromDTCode) $
        lookup (cross, comps) listOfDTCodes


knotTable :: Int -> [LinkDiagram]
knotTable cross = linkTable cross 1


unlink :: Int -> LinkDiagram
unlink k | k < 0      = error $ printf "unlink: number of components %i is negative" k
         | otherwise  = implode (k, [])


unknot :: LinkDiagram
unknot = unlink 1


singleCrossingUnknot :: LinkDiagram
singleCrossingUnknot = fromGaussCode [[1, -1]]


hopfLink :: LinkDiagram
hopfLink = fromDTCode [[4], [2]]


leftTrefoilKnot :: LinkDiagram
leftTrefoilKnot = invertCrossings rightTrefoilKnot


rightTrefoilKnot :: LinkDiagram
rightTrefoilKnot = fromDTCode [[4, 6, 2]]


figureEightKnot :: LinkDiagram
figureEightKnot = fromDTCode [[4, 6, 8, 2]]


leftCinquefoilKnot :: LinkDiagram
leftCinquefoilKnot = invertCrossings rightCinquefoilKnot


rightCinquefoilKnot :: LinkDiagram
rightCinquefoilKnot = fromDTCode [[6, 8, 10, 2, 4]]


threeTwistKnot :: LinkDiagram
threeTwistKnot = invertCrossings $ fromDTCode [[4, 8, 10, 2, 6]]


whiteheadLink :: LinkDiagram
whiteheadLink = fromGaussCode [[-1, 4, -5, 3], [-3, 1, -2, 5, -4, 2]]


grannyKnot :: LinkDiagram
grannyKnot = fromGaussCode [[-1, 2, -3, 1, -2, 3, -4, 5, -6, 4, -5, 6]]


squareKnot :: LinkDiagram
squareKnot = fromGaussCode [[1, -2, 3, -1, 2, -3, -4, 5, -6, 4, -5, 6]]


stevedoreKnot :: LinkDiagram
stevedoreKnot = invertCrossings $ fromDTCode [[4, 8, 12, 10, 2, 6]]


borromeanRingsLink :: LinkDiagram
borromeanRingsLink = fromGaussCode [[1, -6, 5, -3], [4, -1, 2, -5], [6, -4, 3, -2]]


conwayKnot :: LinkDiagram
conwayKnot = invertCrossings $ fromDTCode [[4, 8, 12, 2, -16, -18, 6, -20, -22, -14, -10]]


kinoshitaTerasakaKnot :: LinkDiagram
kinoshitaTerasakaKnot = invertCrossings $ fromDTCode [[4, 8, 12, 2, -18, -20, 6, -10, -22, -14, -16]]
