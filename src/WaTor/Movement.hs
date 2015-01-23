

module WaTor.Movement
    ( up
    , down
    , right
    , left
    , neighborhood
    , neighborhoodEntities
    ) where


import           Control.Arrow       hiding (left, right)
import           Data.Maybe
import           Data.Traversable    hiding (mapM)
import qualified Data.Vector.Mutable as MV

import           WaTor.Types
import           WaTor.Vector

up, down, left, right :: Coord -> Coord -> Coord

up (_, h) (x, 0) = (x, h - 1)
up _      (x, y) = (x, y - 1)

down (_, h) (x, y) | y == h - 1 = (x, 0)
                   | otherwise  = (x, y + 1)

left (w, _) (0, y) = (w - 1, y)
left _      (x, y) = (x - 1, y)

right (w, _) (x, y) | x == w - 1 = (0, y)
                    | otherwise  = (x + 1, y)

neighborhood :: Coord -> Coord -> [Coord]
neighborhood extent center = map (\f -> f extent center) [up, right, down, left]

neighborhoodEntities :: Coord -> Coord -> Vector2D MV.IOVector a
                     -> IO [(Coord, a)]
neighborhoodEntities extent center v =
    fmap (mapMaybe sequenceA)
        . mapM (sequenceA . (id &&& (v !!?)))
        $ neighborhood extent center


