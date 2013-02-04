{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Tangle.Moves.Move
    ( MoveM
    , move
    , oppositeC
    , emitCircle
    , maskC
    , modifyC
    , connectC
    , substituteC
    , greedy
    ) where

import Data.Array.ST (STArray, STUArray, newArray_, newListArray, readArray, writeArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad ((>=>), when, forM, forM_, foldM_, filterM)
import Text.Printf
import Math.KnotTh.Tangle


data CrossingFlag ct = Direct !(CrossingState ct) | Flipped !(CrossingState ct) | Masked


data MoveState s ct = MoveState
    { stateSource      :: !(Tangle ct)
    , stateMask        :: !(STArray s Int (CrossingFlag ct))
    , stateCircles     :: !(STRef s Int)
    , stateConnections :: !(STArray s Int (Dart ct))
    }


disassemble :: (CrossingType ct) => Tangle ct -> ST s (MoveState s ct)
disassemble tangle = do
    connections <- newArray_ (dartIndexRange tangle)
    forM_ (allEdges tangle) $ \ (!a, !b) -> do
        writeArray connections (dartIndex a) b
        writeArray connections (dartIndex b) a

    mask <- newListArray (crossingIndexRange tangle) $ map (Direct . crossingState) $ allCrossings tangle
    circlesCounter <- newSTRef $ numberOfFreeLoops tangle
    return $! MoveState
        { stateSource      = tangle
        , stateMask        = mask
        , stateCircles     = circlesCounter
        , stateConnections = connections
        }


assemble :: (CrossingType ct) => MoveState s ct -> ST s (Tangle ct)
assemble st = do
    let source = stateSource st

    offset <- newArray_ (crossingIndexRange source) :: ST s (STUArray s Int Int)
    foldM_ (\ !x !c -> do
            let i = crossingIndex c
            msk <- readArray (stateMask st) i
            case msk of
                Masked -> return x
                _      -> do
                    writeArray offset (crossingIndex c) x
                    return $! x + 1
        ) 1 (allCrossings source)

    let pair d
            | isLeg d    = return $! (,) 0 $! legPlace d
            | otherwise  = do
                let i = crossingIndex $ incidentCrossing d
                msk <- readArray (stateMask st) i
                off <- readArray offset i
                case msk of
                    Direct _  -> return $! (off, dartPlace d)
                    Flipped _ -> return $! (off, 3 - dartPlace d)
                    Masked    -> fail $ printf "assemble: %s is touching masked crossing %i at:\n%s" (show d) i (show $ stateSource st)

    let opp d = readArray (stateConnections st) (dartIndex d)

    border <- forM (allLegs source) (opp >=> pair)
    connections <- do
        alive <- flip filterM (allCrossings source) $ \ !c -> do
            msk <- readArray (stateMask st) (crossingIndex c)
            return $! case msk of
                Masked -> False
                _      -> True

        forM alive $ \ !c -> do
                msk <- readArray (stateMask st) (crossingIndex c)
                con <- mapM (opp >=> pair) $ incidentDarts c
                return $! case msk of
                    Direct s  -> (con, s)
                    Flipped s -> (reverse con, s)
                    Masked    -> error "assemble: internal error"

    circles <- readSTRef (stateCircles st)
    return $! implode (circles, border, connections)


reconnect :: MoveState s ct -> [(Dart ct, Dart ct)] -> ST s ()
reconnect st connections =
    forM_ connections $ \ (!a, !b) -> do
        when (a == b) $ fail $ printf "reconnect: %s connect to itself" (show a)
        writeArray (stateConnections st) (dartIndex a) b
        writeArray (stateConnections st) (dartIndex b) a


type MoveM s ct r = ReaderT (MoveState s ct) (ST s) r


move :: (CrossingType ct) => Tangle ct -> (forall s. MoveM s ct ()) -> Tangle ct
move initial modification = runST $ do
    st <- disassemble initial
    runReaderT modification st
    assemble st


oppositeC :: Dart ct -> MoveM s ct (Dart ct)
oppositeC d = ask >>= \ st -> lift $
    readArray (stateConnections st) (dartIndex d)


emitCircle :: Int -> MoveM s ct ()
emitCircle dn = ask >>= \ !st -> lift $ do
    !n <- readSTRef (stateCircles st)
    writeSTRef (stateCircles st) $! n + dn


maskC :: [Crossing ct] -> MoveM s ct ()
maskC crossings = ask >>= \ !st -> lift $
    forM_ crossings $ \ !c ->
        writeArray (stateMask st) (crossingIndex c) Masked


modifyC :: (CrossingType ct) => Bool -> (CrossingState ct -> CrossingState ct) -> [Crossing ct] -> MoveM s ct ()
modifyC needFlip f crossings = ask >>= \ !st -> lift $
    forM_ crossings $ \ !c -> do
        msk <- readArray (stateMask st) (crossingIndex c)
        writeArray (stateMask st) (crossingIndex c) $
            case msk of
                Direct s  | needFlip  -> Flipped $ f s
                          | otherwise -> Direct $ f s
                Flipped s | needFlip  -> Direct $ f s
                          | otherwise -> Flipped $ f s
                Masked                -> error $ printf "modifyC: flipping masked crossing %s" (show c)


connectC :: [(Dart ct, Dart ct)] -> MoveM s ct ()
connectC connections = ask >>= \ st -> lift $
    reconnect st connections


substituteC :: [(Dart ct, Dart ct)] -> MoveM s ct ()
substituteC substitutions = do
    reconnections <- mapM (\ (a, b) -> (,) a `fmap` oppositeC b) substitutions
    st <- ask
    lift $ do
        let source = stateSource st

        arr <- newArray_ (dartIndexRange source) :: ST s (STArray s Int (Dart ct))
        forM_ (allEdges source) $ \ (!a, !b) -> do
            writeArray arr (dartIndex a) a
            writeArray arr (dartIndex b) b

        forM_ substitutions $ \ (a, b) -> if a == b
            then modifySTRef (stateCircles st) (+ 1)
            else writeArray arr (dartIndex b) a

        (reconnect st =<<) $ forM reconnections $ \ (a, b) -> do
            b' <- readArray arr (dartIndex b)
            return $! (a, b')


greedy :: [Dart ct -> MoveM s ct Bool] -> MoveM s ct ()
greedy reductionsList = iteration
    where
        iteration = do
            darts <- ask >>= \ !st -> lift $ return $ allDartsOfCrossings $ stateSource st
            changed <- anyM processDart darts
            when changed iteration

        processDart d = do
            masked <- ask >>= \ st -> lift $ readArray (stateMask st) (crossingIndex $ incidentCrossing d)
            case masked of
                Masked -> return False
                _      -> anyM (\ r -> r d) reductionsList

        anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
        anyM _ [] = return False
        anyM f (cur : rest) = do
            res <- f cur
            if res
                then return True
                else anyM f rest
