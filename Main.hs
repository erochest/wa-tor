{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import Prelude hiding (mapM)
import           Control.Arrow                        hiding (left, right)
import           Control.Monad hiding (mapM)
import           Data.Bifunctor
import           Data.Maybe
import           Data.Traversable
import qualified Data.Vector                          as V
import qualified Data.Vector.Generic                  as GV
import qualified Data.Vector.Mutable                  as MV
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Simulate
import           Options.Applicative
import           System.Random.MWC


type Chronon = Int
type Energy  = Int
type Coord   = (Int, Int)

data WaTor
        = WaTor
        { time  :: !Chronon
        , world :: !(Vector2D V.Vector Entity)
        }

data Simulation
        = Simulation
        { params  :: !Params
        , extent  :: !Point
        , scaleBy :: !Int
        , wator   :: !WaTor
        }

data Params
        = Params
        { initialSharks  :: !Double
        , initialFish    :: !Double
        , sharkReproduce :: !Chronon
        , fishReproduce  :: !Chronon
        , initEnergy     :: !Energy
        , fishBoost      :: !Energy
        }

data Entity
        = Fish
        { fishAge :: !Chronon
        }
        | Shark
        { sharkAge    :: !Chronon
        , sharkEnergy :: !Energy
        }
        | Empty
        deriving (Eq, Show)

data Vector2D v a
        = V2D
        { v2dExtent :: !Coord
        , v2dData   :: !(v a)
        }

instance Functor v => Functor (Vector2D v) where
    fmap f (V2D e d) = V2D e $ fmap f d

(!?) :: Vector2D V.Vector a -> Coord -> Maybe a
(V2D (w, _) d) !? (x, y) = d V.!? i
    where
        i = y * w + x

(!!?) :: Vector2D MV.IOVector a -> Coord -> IO (Maybe a)
(V2D (w, _) d) !!? (x, y) | i >= MV.length d = return Nothing
                          | otherwise        = Just <$> MV.read d i
    where
        i = y * w + x

fromList :: Coord -> [a] -> Vector2D V.Vector a
fromList extent = V2D extent . V.fromList

toList :: Vector2D V.Vector a -> [a]
toList = V.toList . v2dData

indexes :: Coord -> [Coord]
indexes (w, h) = [ (x, y) | y <- [0..h-1], x <- [0..w-1] ]

both :: (a -> b) -> (a, a) -> (b, b)
both f = f `bimap` f

floatPair :: Coord -> (Float, Float)
floatPair = both fromIntegral


randomWaTor :: GenIO -> Params -> Coord -> IO WaTor
randomWaTor g p extent@(w, h) =
    WaTor 0 . V2D extent . V.fromList <$> replicateM (w * h) (randomEntity g p)

randomEntity :: GenIO -> Params -> IO Entity
randomEntity g Params{..} = randomEntity' <$> uniform g
    where
        randomEntity' x | x <= initialFish                   = Fish  0
                        | x <= (initialFish + initialSharks) = Shark 0 initEnergy
                        | otherwise                          = Empty

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
        slide x  = -1.0 * x

        render' (_     , Nothing) = Nothing
        render' ((x, y), Just c)  =
            Just . color c
                 . translate (scaleBy' * x) (scaleBy' * y)
                 $ circle (scaleBy' / 2.0)

entityColor :: Entity -> Maybe Color
entityColor Fish{}  = Just $ dim blue
entityColor Shark{} = Just $ dim red
entityColor Empty   = Nothing

step :: GenIO -> ViewPort -> Float -> Simulation -> IO Simulation
step g _ _ s@(Simulation ps _ _ (WaTor t (V2D extent w))) = do
    v' <- V2D extent <$> V.thaw w

    ((`shuffle` g) $ V.zip indexes' w)
        >>= V.mapM_ (uncurry (stepCell g ps extent v'))

    v'' <- V.freeze $ v2dData v'
    return $ s { wator = WaTor { time = t + 1
                               , world = V2D extent v''
                               }
               }
    where
        indexes' = V.fromList $ indexes extent

stepCell :: GenIO
         -> Params
         -> Coord
         -> Vector2D MV.IOVector Entity
         -> Coord
         -> Entity
         -> IO ()

stepCell g Params{..} extent v pos f@Fish{} = do
    mto <-  (`randomElem` g)
        =<< filter (isEmpty . snd)
        <$> neighborhoodEntities extent pos v
    case mto of
        Just (to, Empty)  ->
            swapReproduce v pos to f' fishAge fishReproduce $ Fish 0
        Just (_, Fish{})  -> return ()
        Just (_, Shark{}) -> return ()
        Nothing           -> return ()
    where
        f' = f { fishAge = fishAge f + 1 }

stepCell g ps extent v pos@(x, y) Shark{..} = undefined
stepCell _ _ _ _ _      Empty = return ()

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

isFish, isShark, isEmpty :: Entity -> Bool

isFish Fish{}   = True
isFish _        = False

isShark Shark{} = True
isShark _       = False

isEmpty Empty   = True
isEmpty _       = False

swapReproduce :: Vector2D MV.IOVector Entity
              -> Coord
              -> Coord
              -> Entity
              -> (Entity -> Chronon)
              -> Chronon
              -> Entity
              -> IO ()
swapReproduce v from to entity getAge reproduceAge child = undefined

shuffle :: V.Vector a -> GenIO -> IO (V.Vector a)
shuffle = undefined

randomSwap :: V.Vector a -> Int -> GenIO -> IO a
randomSwap = undefined

randomElem :: [a] -> GenIO -> IO (Maybe a)
randomElem = undefined


main :: IO ()
main = do
    ps  <- execParser opts
    withSystemRandom $ asGenIO $ \g -> do
        wtr <- randomWaTor g ps (w, h)
        simulateIO (InWindow "Wa-Tor" (w * scaleBy, h * scaleBy) (0, 0))
                   black
                   7
                   (Simulation ps (fromIntegral w, fromIntegral h) scaleBy wtr)
                   render
                   (step g)
    where
        w       = 500
        h       = 309
        scaleBy = 2

opts' :: Parser Params
opts' =   Params
      <$> option auto
                 (  long "initial-sharks"
                 <> metavar "INITIAL_SHARKS_PROBABILITY" <> value 0.1
                 <> help "The probability that an initial location will\
                         \ be a shark.")
      <*> option auto
                 (  long "initial-fish"
                 <> metavar "INITIAL_FISH_PROBABILITY" <> value 0.2
                 <> help "The probability that an initial location will\
                         \ be a fish.")
      <*> option auto
                 (  long "reproduce-shark"
                 <> metavar "SHARK_REPRODUCE_AGE" <> value 10
                 <> help "The age at which shark reproduce.")
      <*> option auto
                 (  long "reproduce-fish"
                 <> metavar "FISH_REPRODUCE_AGE" <> value 10
                 <> help "The age at which fish reproduce.")
      <*> option auto
                 (  long "initial-shark-energy"
                 <> metavar "INITIAL_SHARK_ENERGY" <> value 15
                 <> help "The amount of energy sharks are created with.")
      <*> option auto
                 (  long "fish-energy"
                 <> metavar "EATEN_FISH_ENERGY" <> value 10
                 <> help "The amount of energy a shark gets from eating\
                         \ a fish.")

opts :: ParserInfo Params
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Wa-Tor population dynamics simulation."
            <> header "wa-tor - population dynamics")
