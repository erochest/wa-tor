{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Arrow                        hiding (left, right)
import           Control.Monad                        hiding (mapM)
import           Data.Bifunctor
import qualified Data.HashMap.Strict                  as M
import           Data.Maybe
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO
import qualified Data.Text.Lazy                       as TL
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
import           Data.Traversable
import qualified Data.Vector                          as V
import qualified Data.Vector.Mutable                  as MV
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Simulate
import           Options.Applicative
import           Prelude                              hiding (mapM)
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
        , width          :: !Int
        , height         :: !Int
        , scaling        :: !Int
        , speed          :: !Int
        , countLog       :: !(Maybe FilePath)
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

logCounts :: WaTor -> FilePath -> IO ()
logCounts w fp = TIO.appendFile fp
               . TL.toStrict
               . toLazyText
               . cline (time w)
               . V.foldl' indexCounts M.empty
               . v2dData
               $ world w
    where
        cline t m =  decimal t <> "\t"
                  <> decimal (M.lookupDefault 0 "fish"  m) <> "\t"
                  <> decimal (M.lookupDefault 0 "shark" m) <> "\t"
                  <> decimal (M.lookupDefault 0 "empty" m) <> "\n"

indexCounts :: M.HashMap T.Text Int -> Entity -> M.HashMap T.Text Int
indexCounts m e = M.insertWith (+) (entityKey e) 1 m

entityKey :: Entity -> T.Text
entityKey Fish{}  = "fish"
entityKey Shark{} = "shark"
entityKey Empty   = "empty"

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
entityColor f@Fish{}  = Just . dimTo (fishAge f) $ blue
entityColor s@Shark{} = Just . dimTo (sharkEnergy s) $ red
entityColor   Empty   = Nothing

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

step :: GenIO -> ViewPort -> Float -> Simulation -> IO Simulation
step g _ _ s@(Simulation ps _ _ wator@(WaTor t (V2D extent w))) = do
    maybe (return ()) (logCounts wator) $ countLog ps
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

stepCell g Params{..} extent v from f@Fish{} = do
    ns <- neighborhoodEntities extent from v
    moveEmpty g v from f' fishReproduce ns fishAge $ Fish 0
    where
        f' = f { fishAge = fishAge f + 1 }

stepCell g Params{..} extent v from s@Shark{}
    | sharkEnergy s == 0 = MV.write (v2dData v) (idx v from) Empty >> putStrLn ("DIED: " ++ show s)
    | otherwise          = do
        ns    <- neighborhoodEntities extent from v
        mfish <- randomElem (filter (isFish . snd) ns) g
        case mfish of
            Just (to, f@Fish{}) -> do
                putStrLn ("EATING: " ++ show (s' { sharkEnergy = sharkEnergy s + fishBoost })
                         ++ " < " ++ show f)
                move v from to
                     (s' { sharkEnergy = sharkEnergy s + fishBoost })
                     sharkAge sharkReproduce child
            _ -> moveEmpty g v from s' sharkReproduce ns sharkAge child
        where
            s'    = Shark (sharkAge s + 1) (sharkEnergy s - 1)
            child = Shark 0 initEnergy

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

move :: Vector2D MV.IOVector Entity
     -> Coord
     -> Coord
     -> Entity
     -> (Entity -> Chronon)
     -> Chronon
     -> Entity
     -> IO ()
move v2@(V2D _ v) from to entity getAge reproduceAge child = do
    MV.write v (idx v2 to) entity
    MV.write v (idx v2 from) $
        if (getAge entity `mod` reproduceAge) == 0
            then child
            else Empty

moveEmpty :: GenIO
          -> Vector2D MV.IOVector Entity
          -> Coord
          -> Entity
          -> Chronon
          -> [(Coord, Entity)]
          -> (Entity -> Chronon)
          -> Entity
          -> IO ()
moveEmpty g v from curr repro ns getAge child =
    randomElem (filter (isEmpty . snd) ns) g
    >>= \case
        Just (to, Empty)   ->
            move v from to curr getAge repro child
        Just (_,  Fish{})  -> return ()
        Just (_,  Shark{}) -> return ()
        Nothing            -> return ()

shuffle :: V.Vector a -> GenIO -> IO (V.Vector a)
shuffle v g = do
    v' <- V.thaw v
    mapM_ (randomSwap g v') [V.length v - 1 .. 1]
    V.freeze v'

randomSwap :: GenIO -> MV.IOVector a -> Int -> IO a
randomSwap _ v 0 = MV.read v 0
randomSwap _ v 1 = do
    x <- MV.read v 1
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


main :: IO ()
main = do
    ps  <- execParser opts
    let w       = width ps
        h       = height ps
        scaleBy = scaling ps
    withSystemRandom $ asGenIO $ \g -> do
        wtr <- randomWaTor g ps (w, h)
        simulateIO (InWindow "Wa-Tor" (w * scaleBy, h * scaleBy) (0, 0))
                   black
                   7
                   (Simulation ps (fromIntegral w, fromIntegral h) scaleBy wtr)
                   render
                   (step g)

opts' :: Parser Params
opts' =   Params
      <$> option auto
                 (  long "initial-sharks"
                 <> metavar "INITIAL_SHARKS_PROBABILITY" <> value 0.1
                 <> help "The probability that an initial location will\
                         \ be a shark. Default = 0.1.")
      <*> option auto
                 (  long "initial-fish"
                 <> metavar "INITIAL_FISH_PROBABILITY" <> value 0.2
                 <> help "The probability that an initial location will\
                         \ be a fish. Default = 0.2.")
      <*> option auto
                 (  long "reproduce-shark"
                 <> metavar "SHARK_REPRODUCE_AGE" <> value 10
                 <> help "The age at which shark reproduce. Default = 10.")
      <*> option auto
                 (  long "reproduce-fish"
                 <> metavar "FISH_REPRODUCE_AGE" <> value 10
                 <> help "The age at which fish reproduce. Default = 10.")
      <*> option auto
                 (  long "initial-shark-energy"
                 <> metavar "INITIAL_SHARK_ENERGY" <> value 15
                 <> help "The amount of energy sharks are created with.\
                         \ Default = 15.")
      <*> option auto
                 (  long "fish-energy"
                 <> metavar "EATEN_FISH_ENERGY" <> value 10
                 <> help "The amount of energy a shark gets from eating\
                         \ a fish. Default = 10.")
      <*> option auto
                 (  long "width"
                 <> metavar "CELLS" <> value 500
                 <> help "The width of the view area in cells. Default = 500.")
      <*> option auto
                 (  long "height"
                 <> metavar "CELLS" <> value 309
                 <> help "The height of the view area in cells. Default = 309.")
      <*> option auto
                 (  long "scaling"
                 <> metavar "ZOOM" <> value 2
                 <> help "The size to zoom the elements to. Default = 2.")
      <*> option auto
                 (  long "speed"
                 <> metavar "FRAMES_PER_SECOND" <> value 7
                 <> help "The frames/second in the simulation. Default = 7.")
      <*> optional (strOption
                       (  long "count-log" <> metavar "FILENAME"
                       <> help "A file name for the counts. This will be TSV."))

opts :: ParserInfo Params
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Wa-Tor population dynamics simulation."
            <> header "wa-tor - population dynamics")
