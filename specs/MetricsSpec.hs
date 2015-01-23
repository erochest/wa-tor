module MetricsSpec where


import qualified Data.List         as L
import qualified Data.Vector       as V
import           System.Random.MWC

import           WaTor.Metrics
import           WaTor.Types

import           Test.Hspec


waTor :: Coord -> [Entity] -> WaTor
waTor c = WaTor 0 . V2D c . V.fromList


spec :: Spec
spec =
    describe "getCounts" $ do
        let e = (3, 3)
        it "should return all zeros for an empty world." $
            let w = waTor e $ L.replicate (3 * 3) Empty
            in  getCounts w `shouldBe` ((0, 1), (0, 1), 3 * 3)
        it "should count any fish." $
            let w = waTor e [ Fish 1, Empty , Empty
                            , Empty , Fish 1, Empty
                            , Empty , Empty , Fish 1
                            ]
            in  getCounts w `shouldBe` ((3, 3), (0, 1), (3 * 3) - 3)
        it "should count any sharks." $
            let w = waTor e [ Empty    , Empty    , Shark 1 1
                            , Empty    , Shark 1 1, Empty
                            , Shark 1 1, Empty    , Empty
                            ]
            in  getCounts w `shouldBe` ((0, 1), (3, 3), (3 * 3) - 3)
        it "should count fish and sharks (imagine that)." $
            let w = waTor e [ Fish 2   , Empty    , Shark 1 1
                            , Fish 2   , Shark 1 1, Empty
                            , Shark 1 1, Empty    , Fish 2
                            ]
            in  getCounts w `shouldBe` ((3, 6), (3, 3), (3 * 3) - 6)
