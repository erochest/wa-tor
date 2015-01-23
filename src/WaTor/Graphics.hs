module WaTor.Graphics
    ( render
    ) where


import           Data.Maybe
import           Graphics.Gloss

import           WaTor.Types
import           WaTor.Utils
import           WaTor.Vector


render :: Simulation -> IO Picture
render (Simulation _ extent scaleBy (WaTor _ wator)) =
    return . uncurry translate (slide `both` extent)
           . pictures
           . mapMaybe render'
           . zip indexes'
           . toList
           $ fmap entityColor wator
    where
        scaleBy' = fromIntegral scaleBy
        indexes' = map floatPair . indexes $ v2dExtent wator
        slide x  = (-0.5) * scaleBy' * x

        render' (_     , Nothing) = Nothing
        render' ((x, y), Just c)  =
            Just . color c
                 . translate (scaleBy' * x) (scaleBy' * y)
                 $ circle (scaleBy' / 2.0)

entityColor :: Entity -> Maybe Color
entityColor Fish{}  = Just $ mixColors 0.3 0.7 blue green
entityColor Shark{} = Just $ bright red
entityColor Empty   = Nothing

dimTo :: Int -> Color -> Color
dimTo e | e >= 100  = id
        | e >=  90  = dim
        | e >=  80  = dim . dim
        | e >=  70  = dim . dim . dim
        | e >=  60  = dim . dim . dim . dim
        | e >=  50  = dim . dim . dim . dim . dim
        | e >=  40  = dim . dim . dim . dim . dim . dim
        | e >=  30  = dim . dim . dim . dim . dim . dim . dim
        | e >=  20  = dim . dim . dim . dim . dim . dim . dim . dim
        | e >=  10  = dim . dim . dim . dim . dim . dim . dim . dim . dim
        | otherwise = dim . dim . dim . dim . dim . dim . dim . dim . dim . dim
