{-# LANGUAGE RecordWildCards #-}


module WaTor.Random
    ( shuffle
    , randomWaTor
    , randomEntity
    , randomSwap
    , randomElem
    ) where


import           Control.Applicative
import           Control.Monad
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import           System.Random.MWC

import           WaTor.Types


randomWaTor :: GenIO -> Params -> Coord -> IO WaTor
randomWaTor g p extent@(w, h) =
    WaTor 0 . V2D extent . V.fromList <$> replicateM (w * h) (randomEntity g p)

randomEntity :: GenIO -> Params -> IO Entity
randomEntity g Params{..} = randomEntity' <$> uniform g
    where
        randomEntity' x | x <= initialFish                   = Fish  0
                        | x <= (initialFish + initialSharks) = Shark 0 initEnergy
                        | otherwise                          = Empty

shuffle :: V.Vector a -> GenIO -> IO (V.Vector a)
shuffle v g = do
    v' <- V.thaw v
    mapM_ (randomSwap g v') $ reverse [1 .. (V.length v - 1)]
    V.freeze v'

randomSwap :: GenIO -> MV.IOVector a -> Int -> IO a
randomSwap _ v 0 = MV.read v 0
randomSwap _ v 1 = do
    x <- MV.read v 1
    MV.write v 1 =<< MV.read v 0
    MV.write v 0 x
    return x
randomSwap g v j = do
    i <- uniformR (0, j - 1) g
    x <- MV.read v i
    MV.write v i =<< MV.read v j
    MV.write v j x
    return x

randomElem :: [a] -> GenIO -> IO (Maybe a)
randomElem [] _ = return Nothing
randomElem xs g = Just . (xs !!) <$> uniformR (0, length xs - 1) g
