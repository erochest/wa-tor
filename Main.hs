{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Simulate


data Simulation = Simulation


render :: Simulation -> IO Picture
render _ = putStrLn "render" >> return Blank

step :: ViewPort -> Float -> Simulation -> IO Simulation
step _ _ = return


main :: IO ()
main =
    simulateIO (InWindow "Wa-Tor" (w, h) (0, 0))
               black
               10
               Simulation
               render
               step
    where
        w = 500
        h = 309
