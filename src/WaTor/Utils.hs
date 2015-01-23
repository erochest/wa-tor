{-# LANGUAGE OverloadedStrings #-}


module WaTor.Utils
    ( both
    , floatPair
    , entityKey
    , isFish
    , isShark
    , isEmpty
    ) where


import           Control.Applicative
import           Data.Bifunctor
import qualified Data.Text           as T

import           WaTor.Types


both :: (a -> b) -> (a, a) -> (b, b)
both f = f `bimap` f

floatPair :: Coord -> (Float, Float)
floatPair = both fromIntegral

entityKey :: Entity -> T.Text
entityKey Fish{}  = "fish"
entityKey Shark{} = "shark"
entityKey Empty   = "empty"

isFish, isShark, isEmpty :: Entity -> Bool

isFish Fish{}   = True
isFish _        = False

isShark Shark{} = True
isShark _       = False

isEmpty Empty   = True
isEmpty _       = False
