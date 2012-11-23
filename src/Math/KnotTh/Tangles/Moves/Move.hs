{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Tangles.Moves.Move
	( MoveM
	, moveZ
	, move
	, oppositeC
	, emitCircle
	, maskC
	, flipC
	, connectC
	, substituteC
	, greedy
	) where

import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (StateT, execStateT, get, lift)
import Control.Monad (forM_)
import Math.KnotTh.Tangles


data CrossingFlag = Direct | Flipped | Masked deriving (Eq, Enum)

data MoveState s ct = MoveState
	{ stateSource  :: !(Tangle ct)
	, stateMask    :: !(STArray s Int CrossingFlag)
	, stateCircles :: !(STRef s Int)
	}

type MoveM s ct r = StateT (MoveState s ct) (ST s) r


moveZ :: Tangle ct -> (forall s. MoveM s ct ()) -> (Tangle ct, Int)
moveZ tangle = move (tangle, 0)


move :: (Tangle ct, Int) -> (forall s. MoveM s ct ()) -> (Tangle ct, Int)
move (tangle, circles) modification = runST $ disassemble >>= execStateT modification >>= assemble
	where
		disassemble = do
			let n = numberOfCrossings tangle
			--connections <- createMDA tangle opposite
			mask <- newArray (1, n) Direct
			circlesCounter <- newSTRef circles
			return $! MoveState
				{ stateSource  = tangle
				, stateMask    = mask
				, stateCircles = circlesCounter
				}

		assemble st = do
			return undefined
{-
			pair <- do
				idxList <- scanl (\ i f -> if f == Masked then i else i + 1) 1 <$> (IOArray.getElems $ stateMask st)
				let idx = Array.listArray (crossingsRange source) idxList
				msk <- freeze (stateMask st)
				return $! \ !d ->
					if isLeg d
						then (0 :: Int, legPosition d)
						else	let (c, place) = begin d
							in case msk Array.! c of
								Direct  -> (idx Array.! c, place)
								Flipped -> (idx Array.! c, (3 - place) Bits..&. 3)
								Masked  -> error "assemble: using masked crossing"

			left <-	let el c = do
					m <- IOArray.readArray (stateMask st) c
					return (c, m)
				in filter ((/= Masked) . snd) <$> (mapM el $ allCrossings source)

			opp <- extractMDAState $ stateConnections st

			let result = constructFromList (border : internal, map (state . fst) left)
				where
					border = map (pair . opp) $ allLegs source

					internal = map (map (pair . opp) . surroundings) left
						where
							surroundings (!c, !m) = case m of
								Direct  -> incidentDarts c
								Flipped -> reverse $ incidentDarts c
								_       -> error "wtf"

			circles <- IORef.readIORef (stateCircles st)

			return $! (result, circles)
			where
				source = stateSource st
				freeze :: (Array.Ix d) => IOArray.IOArray d a -> IO (Array.Array d a)
				freeze = IOArray.freeze
-}


oppositeC :: Dart ct -> MoveM s ct (Dart ct)
oppositeC = undefined --d = State.get >>= \ st -> State.lift $ readMDA (stateConnections st) d


emitCircle :: MoveM s ct ()
emitCircle = get >>= \ !st -> lift $!
	readSTRef (stateCircles st) >>= \ !n ->
		writeSTRef (stateCircles st) $! n + 1


maskC :: [Crossing ct] -> MoveM s ct ()
maskC crossings = get >>= \ !st -> lift $!
	forM_ crossings $ \ !c ->
		writeArray (stateMask st) (crossingIndex c) Masked


flipC :: [Crossing ct] -> MoveM s ct ()
flipC crossings = get >>= \ !st -> lift $!
	forM_ crossings $ \ !c -> do
		msk <- readArray (stateMask st) (crossingIndex c)
		writeArray (stateMask st) (crossingIndex c) $!
			case msk of
				Direct  -> Flipped
				Flipped -> Direct
				Masked  -> error "flipC: flipping masked crossing"


connectC :: [(Dart ct, Dart ct)] -> MoveM s ct ()
connectC = undefined --connections =
--	State.get >>= \ st -> State.lift $
--		reconnect st connections


substituteC :: [(Dart ct, Dart ct)] -> MoveM s ct ()
substituteC = undefined --substitutions = do
--	reconnections <- mapM (\ (a, b) -> do { ob <- oppositeM b ; return $! (a, ob) }) substitutions
--	st <- State.get
--	State.lift $ do
--		aux <- do
--			let testSubsts (a, b) =
--				if a == b
--					then do
--						IORef.modifyIORef (stateCircles st) (+ 1)
--						return False
--					else return True
--			arr <- createMDA (stateSource st) id
--			assignments <- filterM testSubsts substitutions
--			modifyMDA arr $ map swap assignments
--			extractMDAState arr
--		reconnect st $ map (\ (a, b) -> (a, aux b)) reconnections
--	where
--		swap (a, b) = (b, a)


greedy :: [Dart ct -> MoveM s ct Bool] -> MoveM s ct ()
greedy = undefined -- reductionsList = iteration
--	where
--		iteration = do
--			darts <- State.get >>= \ st -> State.lift $ return $ allDarts $ stateSource st
--			changed <- anyM processDart darts
--			when changed iteration

--		processDart d = do
--			masked <- State.get >>= \ st -> State.lift $ IOArray.readArray (stateMask st) (fst $ begin d)
--			if masked == Masked
--				then return False
--				else anyM (\ r -> r d) reductionsList

--		anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
--		anyM _ [] = return False
--		anyM f (cur : rest) = do
--			res <- f cur
--			if res
--				then return True
--				else anyM f rest



{-
module Math.KnotTh.Tangles.Moves.Moves
	( MoveM
	, move
	, moveZ
	, greedy
	, connectM
	, substituteM
	, maskM
	, flipM
	, oppositeM
	, emitCircleM
	) where

import qualified Data.Bits as Bits
import qualified Data.Array as Array
import qualified Data.Array.IO as IOArray
import qualified Data.IORef as IORef
import qualified System.IO.Unsafe as IOUnsafe
import qualified Control.Monad.State.Strict as State
import Control.Monad
import Control.Applicative ((<$>))
import Math.KnotTh.Tangles


data MutableDartArray ct a = MDA !(IOArray.IOArray (Dart ct) a) !(IOArray.IOArray (Dart ct) a)

type MoveM t c d ct a = State.StateT (MoveState t c d ct) IO a


createMDA :: Tangle ct -> (Dart ct -> a) -> IO (MutableDartArray ct a)
createMDA tangle f = do
	dartsArr <- IOArray.newListArray (dartsRange tangle) $ map f darts
	legsArr <- IOArray.newListArray (legsRange tangle) $ map f legs
	return $! MDA dartsArr legsArr
	where
		darts = allDarts tangle
		legs = allLegs tangle


modifyMDA :: MutableDartArray ct a -> [(Dart ct, a)] -> IO ()
modifyMDA (MDA dartsArr legsArr) list =
	forM_ list $ (\ (i, v) -> IOArray.writeArray (if isLeg i then legsArr else dartsArr) i v)


readMDA :: (Tangle t c d ct) => MutableDartArray t c d ct a -> d -> IO a
readMDA (MDA dartsArr legsArr) i = IOArray.readArray (if isLeg i then legsArr else dartsArr) i


extractMDAState :: (Tangle t c d ct) => MutableDartArray t c d ct a -> IO (d -> a)
extractMDAState (MDA dartsArr legsArr) = do
	dartsFreeze <- freeze dartsArr
	legsFreeze <- freeze legsArr
	return $! (\ d -> (if isLeg d then legsFreeze else dartsFreeze) Array.! d)
	where
		freeze :: (Array.Ix d) => IOArray.IOArray d a -> IO (Array.Array d a)
		freeze = IOArray.freeze


reconnect :: MoveState ct -> [(Dart ct, Dart ct)] -> IO ()
reconnect st = modifyMDA (stateConnections st) . concatMap (\ (a, b) -> [(a, b), (b, a)])


greedy :: (Tangle t c d ct) => [d -> State.StateT (MoveState t c d ct) IO Bool] -> State.StateT (MoveState t c d ct) IO ()
greedy reductionsList = iteration
	where
		iteration = do
			darts <- State.get >>= \ st -> State.lift $ return $ allDarts $ stateSource st
			changed <- anyM processDart darts
			when changed iteration

		processDart d = do
			masked <- State.get >>= \ st -> State.lift $ IOArray.readArray (stateMask st) (fst $ begin d)
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


connectM :: (Tangle t c d ct) => [(d, d)] -> State.StateT (MoveState t c d ct) IO ()
connectM connections =
	State.get >>= \ st -> State.lift $
		reconnect st connections


substituteM :: (Tangle t c d ct) => [(d, d)] -> State.StateT (MoveState t c d ct) IO ()
substituteM substitutions = do
	reconnections <- mapM (\ (a, b) -> do { ob <- oppositeM b ; return $! (a, ob) }) substitutions
	st <- State.get
	State.lift $ do
		aux <- do
			let testSubsts (a, b) =
				if a == b
					then do
						IORef.modifyIORef (stateCircles st) (+ 1)
						return False
					else return True

			arr <- createMDA (stateSource st) id
			assignments <- filterM testSubsts substitutions
			modifyMDA arr $ map swap assignments
			extractMDAState arr

		reconnect st $ map (\ (a, b) -> (a, aux b)) reconnections

	where
		swap (a, b) = (b, a)
-}