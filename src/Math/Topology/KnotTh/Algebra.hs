module Math.Topology.KnotTh.Algebra
    ( extendedGCD
    ) where


extendedGCD :: (Integral a) => a -> a -> (a, a, a)
extendedGCD !a 0 | a >= 0     = (a, 1, 0)
                 | otherwise  = (-a, -1, 0)
extendedGCD !a !b =
    let (quotient, remainder) = divMod a b
        (g, x, y) = extendedGCD b remainder
    in (g, y, x - quotient * y)
