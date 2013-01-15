module Graphics.HP.Units
    ( pt
    , mm
    , cm
    ) where


pt :: Double
pt = 1.0


mm :: Double
mm = pt * 360 / 127


cm :: Double
cm = mm * 10.0
