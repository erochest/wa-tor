module WaTor.Types
    ( Chronon
    , Energy
    , Coord
    , Counts
    , WaTor(..)
    , Simulation(..)
    , Params(..)
    , StepSummary(..)
    , Entity(..)
    , Vector2D(..)
    ) where


import           Data.Monoid
import qualified Data.Vector    as V
import           Graphics.Gloss


type Chronon = Int
type Energy  = Int
type Coord   = (Int, Int)
type Counts  = ((Int, Int), (Int, Int), Int)

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
        , quiet          :: !Bool
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
