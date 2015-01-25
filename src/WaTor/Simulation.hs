{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


module WaTor.Simulation
    ( step
    ) where


import           Control.Applicative
import           Control.Monad                        hiding (mapM)
import           Data.Monoid
import           Data.Traversable
import qualified Data.Vector                          as V
import qualified Data.Vector.Mutable                  as MV
import           Graphics.Gloss.Interface.IO.Simulate
import           Prelude                              hiding (mapM)
import           System.Random.MWC

import           WaTor.Metrics
import           WaTor.Movement
import           WaTor.Random
import           WaTor.Types
import           WaTor.Utils
import           WaTor.Vector


step :: GenIO -> Params -> ViewPort -> Float -> Simulation -> IO Simulation
step g p _ _ s@(Simulation ps _ _ wator@(WaTor t (V2D extent w))) = do
    let counts@((fc, _), (sc, _), _) = getCounts wator
    maybe (return ()) (logCounts wator counts) $ countLog ps
    w' <- if fc == 0 && sc == 0
              then mapM (const $ randomEntity g p) w
              else return w

    v' <- V2D extent <$> V.thaw w'

    ((`shuffle` g) $ V.zip indexes' w)
        >>= V.mapM (uncurry (stepCell g ps extent v'))
        >>= unless (quiet p) .
                print . V.foldl' mappend mempty

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
         -> IO StepSummary

stepCell g Params{..} extent v from f@Fish{} = do
    current <- v !!? from
    case current of
        Just Fish{} -> do
            ns <- neighborhoodEntities extent from v
            moveEmpty g v from f' fishReproduce ns fishAge $ Fish 0
        _ -> return mempty
    where
        f' = f { fishAge = fishAge f + 1 }

stepCell g Params{..} extent v from (Shark _ 0) = do
    MV.write (v2dData v) (idx v from) Empty
    return (mempty { sharksStarved = 1 })
stepCell g Params{..} extent v from s@Shark{} = do
    ns    <- neighborhoodEntities extent from v
    mfish <- randomElem (filter (isFish . snd) ns) g
    case mfish of
        Just (to, Fish{}) ->
            move v from to s'' sharkAge sharkReproduce child
        _ -> moveEmpty g v from s' sharkReproduce ns sharkAge child
    where
        s'    = Shark (sharkAge s + 1) (sharkEnergy s - 1)
        s''   = s' { sharkEnergy = sharkEnergy s + fishBoost }
        child = Shark 0 initEnergy

stepCell _ _ _ _ _      Empty = return mempty

move :: Vector2D MV.IOVector Entity
     -> Coord
     -> Coord
     -> Entity
     -> (Entity -> Chronon)
     -> Chronon
     -> Entity
     -> IO StepSummary
move v2@(V2D _ v) from to entity getAge reproduceAge child = do
    let child' = if (getAge entity `mod` reproduceAge) == 0
                     then child
                     else Empty
        to'   = idx v2 to
        from' = idx v2 from
    was <- MV.read v to'
    MV.write v to' entity
    MV.write v from' child'
    return $ StepSum (if isFish child' && isFish entity then 1 else 0)
                     (if isFish was then 1 else 0)
                     (if isShark child' && isShark entity then 1 else 0)
                     0

update :: Vector2D MV.IOVector Entity -> Coord -> Entity -> IO StepSummary
update v2@(V2D _ v) coord entity =
    MV.write v (idx v2 coord) entity >> return mempty

moveEmpty :: GenIO
          -> Vector2D MV.IOVector Entity
          -> Coord
          -> Entity
          -> Chronon
          -> [(Coord, Entity)]
          -> (Entity -> Chronon)
          -> Entity
          -> IO StepSummary
moveEmpty g v from curr repro ns getAge child =
    randomElem (filter (isEmpty . snd) ns) g
    >>= \case
        Just (to, Empty)   ->
            move v from to curr getAge repro child
        Just (_,  Fish{})  -> update v from curr
        Just (_,  Shark{}) -> update v from curr
        Nothing            -> update v from curr
