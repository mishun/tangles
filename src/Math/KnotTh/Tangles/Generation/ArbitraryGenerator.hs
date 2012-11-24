module Math.KnotTh.Tangles.Generation.ArbitraryGenerator
	( generateArbitrary
	) where

--import Data.Word
--import qualified Data.List as List
--import qualified Control.Monad.State.Strict as State
--import Control.Monad
--import qualified Data.CRC as CRC
--import qualified Data.EquivalenceClasses as EquivCl
--import qualified Math.KnotTh.Tangles.BorderIncremental.Generator as BIGenerator
--import qualified Math.KnotTh.Tangles.BorderIncremental.Tests as BITests
--import qualified Math.KnotTh.Tangles.Util as Util
--import qualified Math.KnotTh.Tangles.Util.Connectivity as Connectivity
--import qualified Math.KnotTh.Tangles.Util.Alternating as Alternating

import qualified Data.Map as Map
import Control.Monad.State.Strict (execState, get, put)
import qualified Data.IntDisjointSet as DS
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.BorderIncremental.SimpleTypes
import qualified Math.KnotTh.Tangles.Moves.ReidemeisterIII as ReidemeisterIII
import qualified Math.KnotTh.Tangles.Moves.ReidemeisterReduction as ReidemeisterReduction
import qualified Math.KnotTh.Tangles.Moves.Pass as Pass
import qualified Math.KnotTh.Tangles.Moves.Flype as Flype
--import qualified Math.KnotTh.Tangles.Moves.Weak as Weak
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Tangles


data EquivalenceClasses k v = EquivalenceClasses
	{ merger' :: v -> v -> v
	, keys'   :: Map.Map k Int
	, values' :: Map.Map Int v
	, sets'   :: DS.IntDisjointSet
	}


{-
null :: EquivalenceClasses k v -> Bool
null = Map.null . _indices


size :: EquivalenceClasses k v -> Int
size = Map.size . _indices


member :: (Ord k) => k -> EquivalenceClasses k v -> Bool
member key = Map.member key . _indices


notMember :: (Ord k) => k -> EquivalenceClasses k v -> Bool
notMember key = Map.notMember key . _indices


empty :: (v -> v -> v) -> EquivalenceClasses k v
empty merger = EC merger 0 Map.empty Map.empty $ fromBound 7


singleton :: (Ord k) => (v -> v -> v) -> k -> v -> EquivalenceClasses k v
singleton merger key value = insert key value (empty merger)


insert :: (Ord k) => k -> v -> EquivalenceClasses k v -> EquivalenceClasses k v
insert key value (EC merger free indices info disj)
	| exists     = EC merger free indices nextInfo disj
	| otherwise  =
		let nextDisj =
			if free <= bound disj
				then disj
				else reBound (2 * (bound disj) + 1) disj

		in EC merger (free + 1) (Map.insert key free indices) nextInfo nextDisj

	where
		exists = Map.member key indices

		index = if exists
			then findSet disj $! (indices Map.! key)
			else free

		nextInfo = Map.alter alter index info
			where
				alter (Just !x) = Just $! merger x value
				alter Nothing = Just value


join :: (Ord k) => (k, k) -> EquivalenceClasses k v -> EquivalenceClasses k v
join (a, b) m@(EC merger free indices info disjSet)
	| Maybe.isNothing indexA  = m
	| Maybe.isNothing indexB  = m
	| rootA == rootB          = m
	| otherwise  = EC merger free indices newInfo newSet

	where
		indexA = Map.lookup a indices
		indexB = Map.lookup b indices

		rootA = findSet disjSet $ Maybe.fromJust indexA
		rootB = findSet disjSet $ Maybe.fromJust indexB

		newSet = unionSet (rootA, rootB) disjSet

		value = merger (info Map.! rootA) (info Map.! rootB)

		newInfo =
			if rootA == findSet newSet rootA
				then Map.insert rootA value $ Map.delete rootB info
				else Map.insert rootB value $ Map.delete rootA info


join' :: (Ord k) => EquivalenceClasses k v -> (k, k) -> EquivalenceClasses k v
join' m p = join p m


find :: (Ord k) => k -> EquivalenceClasses k v -> v
find key (EC _ _ indices info disjSet) = info Map.! root
	where
		index = (Map.!) indices key

		root = findSet disjSet index


sameClass :: (Ord k) => EquivalenceClasses k v -> k -> k -> Bool
sameClass m a b
	| Maybe.isNothing indexA  = False
	| Maybe.isNothing indexB  = False
	| otherwise  = rootA == rootB

	where
		indices = _indices m
		disjSet = _sets m

		indexA = Map.lookup a indices
		indexB = Map.lookup b indices

		rootA = findSet disjSet $ Maybe.fromJust indexA
		rootB = findSet disjSet $ Maybe.fromJust indexB


keys :: EquivalenceClasses k v -> [k]
keys = Map.keys . _indices


keysSet :: (Ord k) => EquivalenceClasses k v -> Set.Set k
keysSet = Map.keysSet . _indices


classes :: EquivalenceClasses k v -> [v]
classes m = Map.elems $ _info m


assocs :: (Ord k) => EquivalenceClasses k v -> [(k, v)]
assocs ec = map (\ k -> (k, (_info ec) Map.! (findSet (_sets ec) $ (_indices ec) Map.! k))) $ keys ec

-}

{-
data DiagramInfo = Wrong | Ok !(Tangle ArbitraryCrossing) | Composite Int


instance Eq DiagramInfo where
	(==) a b = (compare a b) == EQ


instance Ord DiagramInfo where
	compare Wrong Wrong = EQ
	compare Wrong _ = LT
	compare _ Wrong = GT

	compare (Composite n) (Composite m) = compare n m

	compare (Ok d) (Composite n) = if numberOfCrossings d <= n then LT else GT
	compare (Composite n) (Ok d) = if numberOfCrossings d <= n then GT else LT

	compare (Ok a) (Ok b)
		| nA < nB       = LT
		| nA > nB       = GT
		| altA && altB  = EQ
		| altA          = LT
		| altB          = GT
		| otherwise     = EQ

		where
			nA = numberOfCrossings a
			nB = numberOfCrossings b

			altA = Alternating.isAlternating a
			altB = Alternating.isAlternating b


mergeInfo :: DiagramInfo -> DiagramInfo -> DiagramInfo
mergeInfo a b =
	if compare a b == GT
		then b
		else a


extractDiagram :: DiagramInfo -> Maybe (Tangle ArbitraryCrossing)
extractDiagram (Ok diagram) = Just diagram
extractDiagram _ = Nothing



invariant :: (Tangle t c d ct) => t -> Word64
invariant !diagram = CRC.listCRC64 $! Util.diskHomeomorphismInvariant diagram


bad :: Word64
bad = CRC.emptyCRC64

{-
invariant :: (Tangle t c d ct) => t -> [Int]
invariant !diagram = Util.diskHomeomorphismInvariant diagram


bad :: [Int]
bad = []
-}-}

generateArbitrary :: (Monad m) => Int -> (NonAlternatingTangle -> m ()) -> m ()
generateArbitrary maxN yield
	| maxN < 1   = error "generateArbitrary: maxN must be at least 1"
	| otherwise  =
		let diagrams = classes
			where
				classes =
					let yieldD t _ = get >>= \ l -> put $! t : l
					in execState (simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing] maxN yieldD) []
		in mapM_ yield diagrams
{-	where
		diagrams = mapMaybe extractDiagram $ EquivCl.classes classes

		classes = execState generator $! EquivCl.singleton mergeInfo bad Wrong
			where
				generator = BIGenerator.generateFromCrossings test [ArbitraryCrossing] maxN processDiagram
				test = (BITests.preTestIrreducibleArbitraryForTriangle (maxN + 0), BITests.postTestProjection)

		processDiagram !diagram =
			when (l <= 40) $! do
				let inv = invariant diagram
				visited <- contains inv
				insertLive diagram inv
				when (not visited) $! traverse diagram inv
			where
				l = numberOfLegs diagram

		traverse !diagram !inv = forM_ neighbours action					
			where
				neighbours =
					let moves = [ReidemeisterIII.neighbours, Flype.neighbours, Pass.neighbours{-, DoublePass.neighbours, Weak.neighbours-}]
					in concatMap (map ReidemeisterReduction.greedy1st2ndReduction . ($ diagram)) moves

				action (!d, !extraCircles)
					| extraCircles > 0                  = merge inv bad
					| numberOfCrossings d < 1           = merge inv bad
					| not $ Connectivity.isConnected d  = merge inv bad
					| otherwise  = do
						let invD = invariant d
						visited <- contains invD
						when (not visited) $! do
							insertDead d invD
							traverse d invD
						merge inv invD

		insertLive !diagram !inv = State.modify (\ !c -> EquivCl.insert inv (Ok diagram) c)
		insertDead !diagram !inv = State.modify (\ !c -> EquivCl.insert inv (Composite $ numberOfCrossings diagram) c)
		contains !inv = State.gets (\ !c -> EquivCl.member inv c)
		merge !invA !invB = State.modify (\ !c -> EquivCl.join (invA, invB) c)
-}
