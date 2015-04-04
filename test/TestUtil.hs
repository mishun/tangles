module TestUtil
    ( testCase
    , testProperty
    ) where

import qualified Distribution.TestSuite as TS
import qualified Test.HUnit as HU
import qualified Test.QuickCheck as QC


testCase :: String -> HU.Assertion -> TS.Test
testCase name assertion =
    let ti = TS.TestInstance
            { TS.run       =
                snd `fmap` HU.performTest (\ _ -> return)
                                          (\ msg _ _ -> return $ TS.Finished (TS.Error msg))
                                          (\ msg _ _ -> return $ TS.Finished (TS.Fail msg))
                                          (TS.Finished TS.Pass)
                                          (HU.TestCase assertion)
            , TS.name      = name
            , TS.tags      = []
            , TS.options   = []
            , TS.setOption = \ _ _ -> Right ti
            }
    in TS.Test ti


testProperty :: (QC.Testable p) => String -> p -> TS.Test
testProperty name prop =
    let ti = TS.TestInstance
            { TS.run       = do
                res <- QC.quickCheckResult prop
                return $ TS.Finished $
                    case res of
                        QC.Success {}           -> TS.Pass
                        QC.GaveUp {}            -> TS.Fail (QC.output res)
                        QC.Failure {}           -> TS.Fail (QC.reason res)
                        QC.NoExpectedFailure {} -> TS.Fail (QC.output res)
            , TS.name      = name
            , TS.tags      = []
            , TS.options   = []
            , TS.setOption = \ _ _ -> Right ti
            }
    in TS.Test ti
