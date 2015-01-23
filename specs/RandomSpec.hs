module RandomSpec where


import           Control.Applicative
import qualified Data.Vector                     as V

import           SpecUtils
import           WaTor.Random

import           Test.Hspec
import           Test.Hspec.Expectations.Contrib


spec :: Spec
spec = do
    describe "shuffle" $
        it "should not return items in the same order." $ randomly $ \g -> do
            let v = V.fromList [1..100]
            shuffle v g `shouldNotReturn` v

    let v  = V.fromList [1..100]
    describe "randomSwap" $ do
        it "should do nothing for item 0." $ randomly $ \g -> do
            v'  <- V.thaw v
            randomSwap g v' 0 `shouldReturn` 1
            V.freeze v'       `shouldReturn` v
        it "should swap the first two for item 1." $ randomly $ \g -> do
            v'  <- V.thaw v
            randomSwap g v' 1 `shouldReturn` 2
            (take 5 . V.toList <$> V.freeze v') `shouldReturn` [2, 1, 3, 4, 5]
        it "should randomly swap the given item with another." $ randomly $ \g -> do
            v'  <- V.thaw v
            randomSwap g v' 2 `shouldReturnSatisfy` \y -> y == 1 || y == 2
            (take 5 . V.toList <$> V.freeze v') `shouldReturnSatisfy` \list ->
                list == [3, 2, 1, 4, 5] || list == [1, 3, 2, 4, 5]
