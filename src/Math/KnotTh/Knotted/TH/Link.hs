{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.Knotted.TH.Link
	( produceShowDart
	, produceShowCrossing
	, produceShowKnot
	, produceKnottedInstance
	) where

import Language.Haskell.TH
--import Data.Bits ((.&.), shiftL, shiftR, complement)
import Data.List (intercalate)
import Text.Printf (printf)
--import Data.Array.Base (unsafeAt, unsafeWrite, unsafeRead)
--import Data.Array (Array)
--import Data.Array.Unboxed (UArray)
--import Data.Array.Unsafe (unsafeFreeze)
--import Data.Array.ST (STArray, STUArray, runSTArray, newArray_)
--import Control.Monad.ST (ST, runST)
--import Control.Monad (when, forM_)
import Math.KnotTh.Knotted


produceShowDart :: Name -> DecQ
produceShowDart dartN = do
	ct <- varT `fmap` newName "ct"
	instanceD (cxt []) (conT ''Show `appT` (conT dartN `appT` ct))
		[ valD (varP 'show)
			(normalB [| \ d -> let (c, p) = begin d in printf "(Dart %i %i)" (crossingIndex c) p |])
			[]
		]


produceShowCrossing :: Name -> DecQ
produceShowCrossing crosN = do
	ct <- varT `fmap` newName "ct"
	instanceD (cxt [classP ''Show [ct], classP ''CrossingType [ct]]) (conT ''Show `appT` (conT crosN `appT` ct))
		[ valD (varP 'show)
			(normalB [| \ c ->
				printf "(Crossing %i %s [ %s ])"
					(crossingIndex c)
					(show $ crossingState c)
					(intercalate " " $ map (show . opposite) $ incidentDarts c)
				|])
			[]
		]


produceShowKnot :: Name -> DecQ
produceShowKnot knotN = do
	ct <- varT `fmap` newName "ct"
	instanceD (cxt [classP ''Show [ct], classP ''CrossingType [ct]]) (conT ''Show `appT` (conT knotN `appT` ct))
		[ valD (varP 'show)
			(normalB [| \ knot ->
				printf "(%s (%i O) %s)"
					$(litE $ stringL $ nameBase knotN)
					(numberOfFreeLoops knot)
					(intercalate " " $ map show $ allCrossings knot)
				|])
			[]
		]


produceKnottedInstance :: Name -> DecsQ
produceKnottedInstance knotN = do
	--let knotT = conT knottedName
	let dartN = mkName "Dart"
	let crosN = mkName "Crossing"

	--dartD <- do
	--	ct <- newName "ct"
	--	return $! DataD [] dartN [PlainTV ct] [NormalC dartN [(IsStrict, ConT knotN `AppT` ConT ct), (Unpacked, ConT ''Int)]] []

	h <- [d|
		--DataD [] dartN [PlainTV ct] [NormalC dartN [(IsStrict, ConT knotN `AppT` ConT ct), (Unpacked, ConT ''Int)]] []
		data Dart ct = Dart !($(conT knotN) ct) {-# UNPACK #-} !Int

		instance Eq (Dart ct) where
			(==) (Dart _ d1) (Dart _ d2) = (d1 == d2)

		instance Ord (Dart ct) where
			compare (Dart _ d1) (Dart _ d2) = compare d1 d2

		{-# INLINE dartLink #-}
		dartLink :: Dart ct -> $(conT knotN) ct
		dartLink (Dart l _) = l


		data Crossing ct = Crossing !($(conT knotN) ct) {-# UNPACK #-} !Int

		instance Eq (Crossing ct) where
			(==) (Crossing _ c1) (Crossing _ c2) = (c1 == c2)

		instance Ord (Crossing ct) where
			compare (Crossing _ c1) (Crossing _ c2) = compare c1 c2

		{-# INLINE crossingLink #-}
		crossingLink :: Crossing ct -> $(conT knotN) ct
		crossingLink (Crossing l _) = l


		instance (Show ct, CrossingType ct) => Show ($(conT knotN) ct) where
			show link =
				let d = map show $ allCrossings link
				in concat
					[ "("
					, $(litE $ stringL $ nameBase knotN)
					, " ("
					, show $ numberOfFreeLoops link
					, " O) "
					, intercalate " " d
					, " )"
					]

		|]

	kd <- InstanceD [] (ConT ''Knotted `AppT` ConT knotN `AppT` ConT (mkName "Crossing") `AppT` ConT dartN)
		`fmap` [d|
			--crossingOwner = crossingLink

			--crossingIndex (Crossing _ c) = c + 1

			--crossingState (Crossing l c) = stateArray l `unsafeAt` c

			--nthIncidentDart (Crossing l c) i = $(conE dartN) l $! (c `shiftL` 2) + (i .&. 3)

			--opposite (Dart l d) = $(conE dartN) l $! crossingsArray l `unsafeAt` d

			--nextCCW (Dart l d) = $(conE dartN) l $! (d .&. complement 3) + ((d + 1) .&. 3)

			--nextCW (Dart l d) = $(conE dartN) l $! (d .&. complement 3) + ((d - 1) .&. 3)

			--incidentCrossing (Dart l d) = Crossing l $! d `shiftR` 2

			--dartPlace (Dart _ d) = d .&. 3

			--dartOwner = dartLink

			--dartArrIndex $(conP dartN [wildP]) = i
		|]

	return $! h ++ [kd]
