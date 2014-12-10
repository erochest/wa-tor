{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Monad
import           Data.Bifunctor
import           Data.Maybe
import qualified Data.Vector                          as V
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Simulate
import           Options.Applicative
import           System.Random.MWC


type Chronon = Int
type Energy  = Int
type Coord   = (Int, Int)

data WaTor
        = WaTor
        { world :: !(Vector2D Entity)
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
        | Shark
        | Empty
        deriving (Eq, Show, Enum, Bounded)

data Vector2D a
        = V2D
        { v2dExtent :: !Coord
        , v2dData   :: !(V.Vector a)
        }

instance Functor Vector2D where
    fmap f (V2D e d) = V2D e $ fmap f d

(!?) :: Vector2D a -> Coord -> Maybe a
(V2D (w, _) d) !? (x, y) = d V.!? i
    where
        i = y * w + x

fromList :: Coord -> [a] -> Vector2D a
fromList extent = V2D extent . V.fromList

toList :: Vector2D a -> [a]
toList = V.toList . v2dData


randomWaTor :: GenIO -> Params -> Coord -> IO WaTor
randomWaTor g p extent@(w, h) =
    WaTor . V2D extent . V.fromList <$> replicateM (w * h) (randomEntity g p)

randomEntity :: GenIO -> Params -> IO Entity
randomEntity g Params{..} = randomEntity' <$> uniform g
    where
        randomEntity' x | x <= initialFish                   = Fish
                        | x <= (initialFish + initialSharks) = Shark
                        | otherwise                          = Empty

render :: Simulation -> IO Picture
render (Simulation _ extent scaleBy (WaTor wator)) =
    return . uncurry translate (bimap slide slide extent)
           . pictures
           . mapMaybe render'
           . zip indexes
           . toList
           $ fmap entityColor wator
    where
        (w, h)   = v2dExtent wator
        scaleBy' = fromIntegral scaleBy
        indexes  = [ (x, y) | y <- [0..h-1], x <- [0..w-1] ]
        slide x  = -1.0 * x

        render' (_     , Nothing) = Nothing
        render' ((x, y), Just c)  =
            Just . color c
                 . translate (scaleBy' * fromIntegral x)
                             (scaleBy' * fromIntegral y)
                 $ circle (scaleBy' / 2.0)

entityColor :: Entity -> Maybe Color
entityColor Fish  = Just $ dim blue
entityColor Shark = Just $ dim red
entityColor Empty = Nothing

step :: ViewPort -> Float -> Simulation -> IO Simulation
step _ _ = return


main :: IO ()
main = do
    ps  <- execParser opts
    wtr <- withSystemRandom $ asGenIO $ \g ->
        randomWaTor g ps (w, h)
    simulateIO (InWindow "Wa-Tor" (w * scaleBy, h * scaleBy) (0, 0))
               black
               10
               (Simulation ps (fromIntegral w, fromIntegral h) scaleBy wtr)
               render
               step
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
