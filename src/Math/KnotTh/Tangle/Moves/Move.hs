{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Tangle.Moves.Move
	( MoveM
	, move
	, oppositeC
	, emitCircle
	, maskC
	, flipC
	, connectC
	, substituteC
	, greedy
	) where

import Data.Array.ST (STArray, STUArray, newArray, newArray_, readArray, writeArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (StateT, execStateT, get, lift)
import Control.Monad ((>=>), when, forM, forM_, foldM_, filterM)
import Math.KnotTh.Tangle


data CrossingFlag = Direct | Flipped | Masked deriving (Eq, Enum)

data MoveState s ct = MoveState
	{ stateSource      :: !(Tangle ct)
	, stateMask        :: !(STArray s Int CrossingFlag)
	, stateCircles     :: !(STRef s Int)
	, stateConnections :: !(STArray s Int (Dart ct))
	}


disassemble :: (CrossingType ct) => Tangle ct -> ST s (MoveState s ct)
disassemble tangle = do
	let n = numberOfCrossings tangle

	connections <- newArray_ (0, 2 * numberOfEdges tangle - 1)
	forM_ (allLegsAndDarts tangle) $ \ !d ->
		writeArray connections (dartArrIndex d) (opposite d)

	mask <- newArray (1, n) Direct
	circlesCounter <- newSTRef $! numberOfFreeLoops tangle
	return $! MoveState
		{ stateSource      = tangle
		, stateMask        = mask
		, stateCircles     = circlesCounter
		, stateConnections = connections
		}


assemble :: (CrossingType ct) => MoveState s ct -> ST s (Tangle ct)
assemble st = do
	let source = stateSource st

	offset <- newArray_ (1, numberOfCrossings source) :: ST s (STUArray s Int Int)
	foldM_ (\ !x !c -> do
		let i = crossingIndex c
		msk <- readArray (stateMask st) i
		case msk of
			Masked -> return x
			_      -> do { writeArray offset (crossingIndex c) x ; return $! x + 1 }
		) 1 (allCrossings source)

	let pair d
		| isLeg d    = return $! (0, legPlace d)
		| otherwise  = do
			let i = crossingIndex $! incidentCrossing d
			msk <- readArray (stateMask st) i
			off <- readArray offset i
			case msk of
				Direct  -> return $! (off, dartPlace d)
				Flipped -> return $! (off, 3 - dartPlace d)
				Masked  -> fail "assemble: touching masked crossing"

	let opp d = readArray (stateConnections st) (dartArrIndex d)

	border <- forM (allLegs source) (opp >=> pair)
	connections <-
		filterM (\ !c -> do { msk <- readArray (stateMask st) (crossingIndex c) ; return $! msk /= Masked }) (allCrossings source)
			>>= mapM (\ !c -> do
				msk <- readArray (stateMask st) (crossingIndex c)
				conn <- mapM (opp >=> pair) $
					case msk of
						Direct  -> incidentDarts c
						Flipped -> reverse $ incidentDarts c
						Masked  -> error "assemble: internal error"
				return $! (conn, crossingState c)
			)

	circles <- readSTRef (stateCircles st)
	return $! implode (circles, border, connections)


reconnect :: MoveState s ct -> [(Dart ct, Dart ct)] -> ST s ()
reconnect st connections =
	forM_ connections $ \ (!a, !b) -> do
		when (a == b) (fail "reconnect: connect to itself")
		writeArray (stateConnections st) (dartArrIndex a) b
		writeArray (stateConnections st) (dartArrIndex b) a


type MoveM s ct r = StateT (MoveState s ct) (ST s) r


move :: (CrossingType ct) => Tangle ct -> (forall s. MoveM s ct ()) -> Tangle ct
move initial modification = runST $ disassemble initial >>= execStateT modification >>= assemble


oppositeC :: Dart ct -> MoveM s ct (Dart ct)
oppositeC d = get >>= \ st -> lift $ readArray (stateConnections st) (dartArrIndex d)


emitCircle :: MoveM s ct ()
emitCircle = get >>= \ !st -> lift $ do
	!n <- readSTRef (stateCircles st)
	writeSTRef (stateCircles st) $! n + 1


maskC :: [Crossing ct] -> MoveM s ct ()
maskC crossings = get >>= \ !st -> lift $
	forM_ crossings $ \ !c -> writeArray (stateMask st) (crossingIndex c) Masked


flipC :: [Crossing ct] -> MoveM s ct ()
flipC crossings = get >>= \ !st -> lift $
	forM_ crossings $ \ !c -> do
		msk <- readArray (stateMask st) (crossingIndex c)
		writeArray (stateMask st) (crossingIndex c) $!
			case msk of
				Direct  -> Flipped
				Flipped -> Direct
				Masked  -> error "flipC: flipping masked crossing"


connectC :: [(Dart ct, Dart ct)] -> MoveM s ct ()
connectC connections = get >>= \ st -> lift $
	reconnect st connections


substituteC :: [(Dart ct, Dart ct)] -> MoveM s ct ()
substituteC substitutions = do
	reconnections <- mapM (\ (a, b) -> do { ob <- oppositeC b ; return $! (a, ob) }) substitutions
	st <- get
	lift $ do
		let source = stateSource st

		arr <- newArray_ (0, 2 * numberOfEdges source - 1) :: ST s (STArray s Int (Dart ct))
		forM_ (allLegsAndDarts source) $ \ !d ->
			writeArray arr (dartArrIndex d) d

		forM_ substitutions $ \ (a, b) -> if a == b
			then modifySTRef (stateCircles st) (+ 1)
			else writeArray arr (dartArrIndex b) a

		mapM (\ (a, b) -> do { b' <- readArray arr (dartArrIndex b) ; return (a, b' ) }) reconnections >>= reconnect st


greedy :: [Dart ct -> MoveM s ct Bool] -> MoveM s ct ()
greedy reductionsList = iteration
	where
		iteration = do
			darts <- get >>= \ !st -> lift $ return $ allDarts $ stateSource st
			changed <- anyM processDart darts
			when changed iteration

		processDart d = do
			masked <- get >>= \ st -> lift $ readArray (stateMask st) (crossingIndex $ incidentCrossing d)
			if masked == Masked
				then return False
				else anyM (\ r -> r d) reductionsList

		anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
		anyM _ [] = return False
		anyM f (cur : rest) = do
			res <- f cur
			if res
				then return True
				else anyM f rest
