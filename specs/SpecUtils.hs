module SpecUtils where


import           System.Random.MWC

import           Test.Hspec


randomly :: (GenIO -> IO ()) -> IO ()
randomly = withSystemRandom . asGenIO

shouldReturnSatisfy :: Show a => IO a -> (a -> Bool) -> Expectation
shouldReturnSatisfy m f =
    m >>= (`shouldSatisfy` f)
