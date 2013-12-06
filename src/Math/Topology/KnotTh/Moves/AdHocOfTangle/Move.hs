{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.Moves.AdHocOfTangle.Move
    ( MoveM
    , move
    , assemble
    , oppositeC
    , passOverC
    , emitCircle
    , maskC
    , isMasked
    , aliveCrossings
    , modifyC
    , connectC
    , substituteC
    , greedy
    ) where

import Data.Array.ST (STArray, STUArray, newArray_, newListArray, readArray, writeArray)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Control.Monad.ST (ST, runST)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad ((>=>), when, forM, forM_, foldM_, filterM)
import Text.Printf
import Math.Topology.KnotTh.Tangle


data CrossingFlag a = Direct !a
                    | Flipped !a
                    | Masked


data MoveState s a =
    MoveState
        { stateSource      :: !(Tangle a)
        , stateMask        :: !(STArray s Int (CrossingFlag a))
        , stateCircles     :: !(STRef s Int)
        , stateConnections :: !(STArray s Int (Dart Tangle a))
        }


readMaskST :: MoveState s a -> Vertex Tangle a -> ST s (CrossingFlag a)
readMaskST st c = readArray (stateMask st) (vertexIndex c)


writeMaskST :: MoveState s a -> Vertex Tangle a -> CrossingFlag a -> ST s ()
writeMaskST st c = writeArray (stateMask st) (vertexIndex c)


reconnectST :: MoveState s a -> [(Dart Tangle a, Dart Tangle a)] -> ST s ()
reconnectST st connections =
    forM_ connections $ \ (!a, !b) -> do
        when (a == b) $ fail $ printf "reconnect: %s connect to itself" (show a)
        writeArray (stateConnections st) (dartIndex a) b
        writeArray (stateConnections st) (dartIndex b) a


disassembleST :: Tangle a -> ST s (MoveState s a)
disassembleST tangle = do
    connections <- newArray_ (dartIndicesRange tangle)
    forM_ (allEdges tangle) $ \ (!a, !b) -> do
        writeArray connections (dartIndex a) b
        writeArray connections (dartIndex b) a

    mask <- newListArray (vertexIndicesRange tangle) $
        map (Direct . vertexCrossing) $ allVertices tangle
    circlesCounter <- newSTRef $ numberOfFreeLoops tangle
    return MoveState
        { stateSource      = tangle
        , stateMask        = mask
        , stateCircles     = circlesCounter
        , stateConnections = connections
        }


assembleST :: (Show a) => MoveState s a -> ST s (Tangle a)
assembleST st = do
    let source = stateSource st

    offset <- newArray_ (vertexIndicesRange source) :: ST s (STUArray s Int Int)
    foldM_ (\ !x !c -> do
            msk <- readMaskST st c
            case msk of
                Masked -> return x
                _      -> writeArray offset (vertexIndex c) x >> (return $! x + 1)
        ) 1 (allVertices source)

    let pair d | isLeg d    = return $! (,) 0 $! legPlace d
               | otherwise  = do
                   let i = beginVertexIndex d
                   msk <- readArray (stateMask st) i
                   off <- readArray offset i
                   case msk of
                       Direct _  -> return (off, beginPlace d)
                       Flipped _ -> return (off, 3 - beginPlace d)
                       Masked    -> fail $ printf "assemble: %s is touching masked crossing %i at:\n%s" (show d) i (show $ stateSource st)

    let opp d = readArray (stateConnections st) (dartIndex d)

    border <- forM (allLegs source) (opp >=> pair)
    connections <- do
        alive <- flip filterM (allVertices source) $ \ !c -> do
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
                    Masked    -> error "assemble: internal error"

    circles <- readSTRef (stateCircles st)
    return $! implode (circles, border, connections)


type MoveM s a r = ReaderT (MoveState s a) (ST s) r


move :: (Show a) => Tangle a -> (forall s. MoveM s a ()) -> Tangle a
move initial modification = runST $ do
    st <- disassembleST initial
    runReaderT modification st
    assembleST st


assemble :: (Show a) => MoveM s a (Tangle a)
assemble =
    ask >>= \ st ->
        lift $ assembleST st


oppositeC :: Dart Tangle ct -> MoveM s ct (Dart Tangle ct)
oppositeC d = do
    when (isDart d) $ do
        masked <- isMasked $ beginVertex d
        when masked $
            fail $ printf "oppositeC: touching masked crossing when taking from %s" (show d)
    ask >>= \ st -> lift $
        readArray (stateConnections st) (dartIndex d)


passOverC :: TangleDiagramDart -> MoveM s DiagramCrossing Bool
passOverC d =
    ask >>= \ st -> lift $ do
        when (isLeg d) $ fail $ printf "passOverC: leg %s passed" (show d)
        msk <- readMaskST st $ beginVertex d
        case msk of
            Masked    -> fail "passOverC: touching masked crossing when taking from %s" (show d)
            Direct t  -> return $! passOver' t (beginPlace d)
            Flipped t -> return $! passOver' t (3 - beginPlace d)


emitCircle :: Int -> MoveM s a ()
emitCircle dn =
    ask >>= \ !st -> lift $
        modifySTRef' (stateCircles st) (+ dn)


maskC :: [Vertex Tangle a] -> MoveM s a ()
maskC crossings =
    ask >>= \ !st -> lift $
        forM_ crossings $ \ !c ->
            writeMaskST st c Masked


isMasked :: Vertex Tangle a -> MoveM s a Bool
isMasked c =
    ask >>= \ !st -> lift $ do
        msk <- readMaskST st c
        return $! case msk of
            Masked -> True
            _      -> False


aliveCrossings :: MoveM s a [Vertex Tangle a]
aliveCrossings = do
    st <- ask
    filterM (fmap not . isMasked) $ allVertices $ stateSource st


modifyC :: (Show a) => Bool -> (a -> a) -> [Vertex Tangle a] -> MoveM s a ()
modifyC needFlip f crossings =
    ask >>= \ !st -> lift $
        forM_ crossings $ \ !c -> do
            msk <- readMaskST st c
            writeMaskST st c $
                case msk of
                    Direct s  | needFlip  -> Flipped $ f s
                              | otherwise -> Direct $ f s
                    Flipped s | needFlip  -> Direct $ f s
                              | otherwise -> Flipped $ f s
                    Masked                -> error $ printf "modifyC: flipping masked crossing %s" (show c)


connectC :: [(Dart Tangle ct, Dart Tangle ct)] -> MoveM s ct ()
connectC connections =
    ask >>= \ st -> lift $
        reconnectST st connections


substituteC :: [(Dart Tangle a, Dart Tangle a)] -> MoveM s a ()
substituteC substitutions = do
    reconnections <- mapM (\ (a, b) -> (,) a `fmap` oppositeC b) substitutions
    st <- ask
    lift $ do
        let source = stateSource st

        arr <- newArray_ (dartIndicesRange source) :: ST s (STArray s Int (Dart Tangle ct))
        forM_ (allEdges source) $ \ (!a, !b) -> do
            writeArray arr (dartIndex a) a
            writeArray arr (dartIndex b) b

        forM_ substitutions $ \ (a, b) ->
            if a == b
                then modifySTRef' (stateCircles st) (+ 1)
                else writeArray arr (dartIndex b) a

        (reconnectST st =<<) $ forM reconnections $ \ (a, b) ->
            (,) a `fmap` readArray arr (dartIndex b)


greedy :: [Dart Tangle a -> MoveM s a Bool] -> MoveM s a ()
greedy reductionsList = iteration
    where
        iteration = do
            crs <- aliveCrossings
            changed <- anyM (\ d -> anyM ($ d) reductionsList) $ crs >>= outcomingDarts
            when changed iteration

        anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
        anyM _ [] = return False
        anyM f (cur : rest) = do
            res <- f cur
            if res
                then return True
                else anyM f rest
