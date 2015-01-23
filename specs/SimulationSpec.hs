module SimulationSpec where


import qualified Data.List                       as L
import qualified Data.Vector                     as V
import           System.Random.MWC

import           WaTor.Metrics
import           WaTor.Simulation
import           WaTor.Types
import           WaTor.Utils

import           SpecUtils

import           Test.Hspec
import           Test.Hspec.Expectations.Contrib


spec :: Spec
spec =
    describe "step" $
        it "should restart if everyone is extinct." $ randomly $ \g -> do
            let e = (100, 60)
                p = Params 0.01 0.05 30 15 30 5 100 60 1 7 Nothing
                w = WaTor 0 . V2D e . V.fromList $ L.replicate (100 * 60) Empty
                s = Simulation p (floatPair e) 1 w
            s' <- step g p undefined undefined s
            getCounts (wator s)  `shouldBe`    ((0, 1), (0, 1), uncurry (*) e)
            getCounts (wator s') `shouldNotBe` ((0, 1), (0, 1), uncurry (*) e)
