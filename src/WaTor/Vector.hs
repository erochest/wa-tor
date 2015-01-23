module WaTor.Vector
    ( (!?)
    , (!!?)
    , idx
    , fromList
    , toList
    , indexes
    ) where


import           Control.Applicative
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

import           WaTor.Types


(!?) :: Vector2D V.Vector a -> Coord -> Maybe a
v@(V2D _ d) !? p = d V.!? idx v p

(!!?) :: Vector2D MV.IOVector a -> Coord -> IO (Maybe a)
v@(V2D _ d) !!? p | i >= MV.length d = return Nothing
                  | otherwise        = Just <$> MV.read d i
    where
        i = idx v p

idx :: Vector2D v a -> Coord -> Int
idx (V2D (w, _) _) (x, y) = y * w + x

fromList :: Coord -> [a] -> Vector2D V.Vector a
fromList extent = V2D extent . V.fromList

toList :: Vector2D V.Vector a -> [a]
toList = V.toList . v2dData

indexes :: Coord -> [Coord]
indexes (w, h) = [ (x, y) | y <- [0..h-1], x <- [0..w-1] ]
