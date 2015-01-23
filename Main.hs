{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Graphics.Gloss.Interface.IO.Simulate
import           System.Random.MWC

import           Opts
import           WaTor.Graphics
import           WaTor.Random
import           WaTor.Simulation
import           WaTor.Types


-- TODO: Add the option to output nothing.

-- TODO: Add the option to view a heat-map version of the output.

main :: IO ()
main = do
    ps  <- execParser opts
    let w       = width ps
        h       = height ps
        scaleBy = scaling ps
    withSystemRandom $ asGenIO $ \g -> do
        wtr <- randomWaTor g ps (w, h)
        simulateIO (InWindow "Wa-Tor" (w * scaleBy, h * scaleBy) (0, 0))
                   (dark $ dark blue)
                   7
                   (Simulation ps (fromIntegral w, fromIntegral h) scaleBy wtr)
                   render
                   (step g ps)
