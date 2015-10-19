module Math.Topology.KnotTh.Algebra.Cobordism.CobordismMatrix.Test
    ( test
    ) where

import Test.Framework (Test, testGroup)
--import Test.Framework.Providers.HUnit (testCase)
--import Test.HUnit hiding (Test, test)
import Math.Topology.KnotTh.Algebra.Cobordism.CobordismMatrix
import Math.Topology.KnotTh.Algebra.Cobordism.DottedCobordism
import Math.Topology.KnotTh.Algebra.Cobordism.Test


test :: Test
test = testGroup "Cobordism matrix"
    [ generalCobordism3Tests (undefined :: CobordismMatrix (DottedCobordism' Integer))
    ]
