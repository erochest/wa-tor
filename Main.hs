{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main where


import           Control.Arrow                        hiding (left, right)
import           Control.Concurrent.STM
import           Control.Monad                        hiding (mapM)
import           Data.Bifunctor
import qualified Data.HashMap.Strict                  as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO
import qualified Data.Text.Lazy                       as TL
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
import           Data.Text.Lazy.Builder.RealFloat
import           Data.Traversable
import qualified Data.Vector                          as V
import qualified Data.Vector.Mutable                  as MV
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Simulate
import           Options.Applicative
import           Prelude                              hiding (mapM)
import           System.Random.MWC


type Chronon   = Int
type Energy    = Int
type Coord     = (Int, Int)
type EntityVar = TVar Entity

data WaTor
        = WaTor
        { time  :: !Chronon
        , world :: !(Vector2D V.Vector EntityVar)
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

data StepSummary
        = StepSum
        { fishBorn      :: !Int
        , fishEaten     :: !Int
        , sharksBorn    :: !Int
        , sharksStarved :: !Int
        } deriving (Show)

instance Monoid StepSummary where
    mempty = StepSum 0 0 0 0
    (StepSum fb1 fe1 sb1 ss1) `mappend` (StepSum fb2 fe2 sb2 ss2) =
        StepSum (fb1 + fb2) (fe1 + fe2) (sb1 + sb2) (ss1 + ss2)

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

idx :: Vector2D v a -> Coord -> Int
idx (V2D (w, _) _) (x, y) = y * w + x

fromList :: Coord -> [a] -> Vector2D V.Vector a
fromList extent = V2D extent . V.fromList

toList :: Vector2D V.Vector a -> [a]
toList = V.toList . v2dData

readCell :: Vector2D V.Vector (TVar a) -> Coord -> STM a
readCell v@(V2D _ v') i = readTVar $ v' V.! idx v i

writeCell :: Vector2D V.Vector (TVar a) -> Coord -> a -> STM ()
writeCell v@(V2D _ v') i a = writeTVar (v' V.! idx v i) a

indexes :: Coord -> [Coord]
indexes (w, h) = [ (x, y) | y <- [0..h-1], x <- [0..w-1] ]

both :: (a -> b) -> (a, a) -> (b, b)
both f = f `bimap` f

floatPair :: Coord -> (Float, Float)
floatPair = both fromIntegral


randomWaTor :: GenIO -> Params -> Coord -> IO WaTor
randomWaTor g p extent@(w, h) =
        WaTor 0 . V2D extent . V.fromList
    <$> replicateM (w * h) (newTVarIO =<< randomEntity g p)

randomEntity :: GenIO -> Params -> IO Entity
randomEntity g Params{..} = randomEntity' <$> uniform g
    where
        randomEntity' x | x <= initialFish                   = Fish  0
                        | x <= (initialFish + initialSharks) = Shark 0 initEnergy
                        | otherwise                          = Empty

logCounts :: WaTor -> FilePath -> IO ()
logCounts w fp =   TIO.appendFile fp
               .   TL.toStrict
               .   toLazyText
               .   cline (time w)
               =<< V.foldM' indexCounts M.empty
               (   v2dData
               $   world w
               )
    where

        per :: Int -> Int -> Double
        per n d = fromIntegral n / fromIntegral d

        cline :: Int -> M.HashMap T.Text (Sum Int, Sum Int) -> Builder
        cline t m = let (Sum fc, Sum fa) = M.lookupDefault (Sum 0, Sum 1) "fish"  m
                        (Sum sc, Sum se) = M.lookupDefault (Sum 0, Sum 1) "shark" m
                        (Sum e, _)       = M.lookupDefault (Sum 0, Sum 1) "empty" m
                    in  decimal t <> "\t"
                    <> decimal fc <> "\t" <> realFloat (fa `per` fc) <> "\t"
                    <> decimal sc <> "\t" <> realFloat (se `per` sc) <> "\t"
                    <> decimal e  <> "\n"

indexCounts :: M.HashMap T.Text (Sum Int, Sum Int) -> EntityVar
            -> IO (M.HashMap T.Text (Sum Int, Sum Int))
indexCounts m = fmap ic . readTVarIO
    where
        insert key e metric = M.insertWith mappend key (Sum 1, Sum (metric e))
        ic f@Fish{}  = insert "fish"  f fishAge     m
        ic s@Shark{} = insert "shark" s sharkEnergy m
        ic e@Empty   = insert "empty" e (const 0)   m

entityKey :: Entity -> T.Text
entityKey Fish{}  = "fish"
entityKey Shark{} = "shark"
entityKey Empty   = "empty"

render :: Simulation -> IO Picture
render (Simulation _ extent scaleBy (WaTor _ wator)) =
    uncurry translate (slide `both` extent)
        .   pictures
        .   mapMaybe render'
        .   zip indexes'
        .   V.toList
        .   V.map entityColor
        <$> V.mapM readTVarIO (v2dData wator)
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
entityColor Fish{}  = Just . dim $ blue
entityColor Shark{} = Just . dim $ red
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

step :: GenIO -> ViewPort -> Float -> Simulation -> IO Simulation
step g _ _ s@(Simulation ps _ _ wator@(WaTor t w)) = do
    maybe (return ()) (logCounts wator) $ countLog ps

    ((`shuffle` g) . V.zip indexes' $ v2dData w)
        >>= V.mapM step'
        >>= print . V.foldl' mappend mempty

    return $ s { wator = wator { time = t + 1 } }
    where
        indexes' = V.fromList . indexes $ v2dExtent w
        step' :: (Coord, EntityVar) -> IO StepSummary
        step' (p, e) = do
            [r1, r2] <- replicateM 2 (uniform g) :: IO [Double]
            atomically $
                stepCell ps w (r1, r2) p =<< readTVar e

stepCell :: Params
         -> Vector2D V.Vector EntityVar
         -> (Double, Double)
         -> Coord
         -> Entity
         -> STM StepSummary

stepCell Params{..} v@(V2D extent _) (r, _) from f@Fish{} = readCell v from >>=
    \case
        Fish{} ->
            let ns = neighborhoodEntities extent from v
            in  moveEmpty r v from f' fishReproduce ns fishAge $ Fish 0
        _ -> return mempty
    where
        f' = f { fishAge = fishAge f + 1 }

stepCell Params{..} v@(V2D extent _) (r1, r2) from s@Shark{}
    | sharkEnergy s == 0 =  maybe (return ()) (`writeTVar` Empty) (v !? from)
                         >> return (mempty { sharksStarved = 1 })
    | otherwise          = do
        let ns = neighborhoodEntities extent from v
        mfish <-  (`randomElem` r1)
              .   filter (isFish . snd)
              <$> mapM (sequenceA . fmap readTVar) ns
        case mfish of
            Just (to, Fish{}) ->
                move v from to s'' sharkAge sharkReproduce child
            _ -> moveEmpty r2 v from s' sharkReproduce ns sharkAge child
        where
            s'    = Shark (sharkAge s + 1) (sharkEnergy s - 1)
            s''   = s' { sharkEnergy = sharkEnergy s + fishBoost }
            child = Shark 0 initEnergy

stepCell _ _ _ _      Empty = return mempty

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

neighborhoodEntities :: Coord -> Coord -> Vector2D V.Vector a -> [(Coord, a)]
neighborhoodEntities extent center v =
    mapMaybe (sequenceA . (id &&& (v !?))) $ neighborhood extent center

isFish, isShark, isEmpty :: Entity -> Bool

isFish Fish{}   = True
isFish _        = False

isShark Shark{} = True
isShark _       = False

isEmpty Empty   = True
isEmpty _       = False

move :: Vector2D V.Vector EntityVar
     -> Coord
     -> Coord
     -> Entity
     -> (Entity -> Chronon)
     -> Chronon
     -> Entity
     -> STM StepSummary
move v2 from to entity getAge reproduceAge child = do
    let child' = if (getAge entity `mod` reproduceAge) == 0
                     then child
                     else Empty
    was <- readCell v2 to
    writeCell v2 to entity
    writeCell v2 from child
    return $ StepSum (if isFish child' && isFish entity then 1 else 0)
                     (if isFish was then 1 else 0)
                     (if isShark child' && isShark entity then 1 else 0)
                     0

update :: Vector2D V.Vector EntityVar -> Coord -> Entity -> STM StepSummary
update v2 coord entity =
    writeCell v2 coord entity >> return mempty

moveEmpty :: Double
          -> Vector2D V.Vector EntityVar
          -> Coord
          -> Entity
          -> Chronon
          -> [(Coord, EntityVar)]
          -> (Entity -> Chronon)
          -> Entity
          -> STM StepSummary
moveEmpty r v from curr repro ns getAge child = do
    empties <- filter (isEmpty . snd) <$> mapM (sequenceA . fmap readTVar) ns
    case randomElem empties r of
        Just (to, Empty)   ->
            move v from to curr getAge repro child
        Just (_,  Fish{})  -> update v from curr
        Just (_,  Shark{}) -> update v from curr
        Nothing            -> update v from curr

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

randomElem :: [a] -> Double -> Maybe a
randomElem [] _ = Nothing
randomElem xs r = fmap fst
                . listToMaybe
                . dropWhile ((< r) . snd)
                . map (fmap (/ len))
                $ zip xs ([1.0..] :: [Double])
    where
        len = fromIntegral $ length xs


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
