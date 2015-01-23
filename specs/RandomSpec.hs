module RandomSpec where


import           Control.Applicative
import qualified Data.Vector                     as V
import           System.Random.MWC

import           WaTor.Random

import           Test.Hspec
import           Test.Hspec.Expectations.Contrib


randomly :: (GenIO -> IO ()) -> IO ()
randomly = withSystemRandom . asGenIO


spec :: Spec
spec = describe "WaTor.Random" $ do
    describe "shuffle" $
        it "should not return items in the same order." $ randomly $ \g -> do
            let v = V.fromList [1..100]
            s <- shuffle v g
            s `shouldNotBe` v

    let v  = V.fromList [1..100]
    describe "randomSwap" $ do
        it "should do nothing for item 0." $ randomly $ \g -> do
            v'  <- V.thaw v
            x   <- randomSwap g v' 0
            v'' <- V.freeze v'
            x   `shouldBe` 1
            v'' `shouldBe` v
        it "should swap the first two for item 1." $ randomly $ \g -> do
            v'  <- V.thaw v
            x   <- randomSwap g v' 1
            v'' <- V.toList <$> V.freeze v'
            x `shouldBe` 2
            take 5 v'' `shouldBe` [2, 1, 3, 4, 5]
        it "should randomly swap the given item with another." $ randomly $ \g -> do
            v'  <- V.thaw v
            x   <- randomSwap g v' 2
            v'' <- V.toList <$> V.freeze v'
            x `shouldSatisfy` \y -> y == 1 || y == 2
            take 5 v'' `shouldSatisfy` \list ->
                list == [3, 2, 1, 4, 5] || list == [1, 3, 2, 4, 5]
