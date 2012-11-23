module Math.KnotTh.Tangles.Generation.ArbitraryGenerator
	( --generateArbitrary
	) where
{-
import Data.Word
import qualified Data.List as List
import qualified Control.Monad.State.Strict as State
import Control.Monad
import qualified Data.CRC as CRC
import qualified Data.EquivalenceClasses as EquivCl
import qualified Math.KnotTh.Tangles.BorderIncremental.Generator as BIGenerator
import qualified Math.KnotTh.Tangles.BorderIncremental.Tests as BITests
import qualified Math.KnotTh.Tangles.Util as Util
import qualified Math.KnotTh.Tangles.Util.Connectivity as Connectivity
import qualified Math.KnotTh.Tangles.Util.Alternating as Alternating
import qualified Math.KnotTh.Tangles.Moves.ReidemeisterIII as ReidemeisterIII
import qualified Math.KnotTh.Tangles.Moves.ReidemeisterReduction as ReidemeisterReduction
import qualified Math.KnotTh.Tangles.Moves.Pass as Pass
import qualified Math.KnotTh.Tangles.Moves.Flype as Flype
import qualified Math.KnotTh.Tangles.Moves.Weak as Weak
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Tangles


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
-}

generateArbitrary :: (Monad m) => Int -> (Tangle ArbitraryCrossing -> m ()) -> m ()
generateArbitrary maxN yield
	| maxN < 1   = error "generateArbitrary: maxN must be at least 1"
	| otherwise  = mapM_ yield diagrams
	where
		diagrams = mapMaybe extractDiagram $ EquivCl.classes classes

		classes = State.execState generator $! EquivCl.singleton mergeInfo bad Wrong
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