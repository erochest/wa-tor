{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module WaTor.Metrics
    ( getCounts
    , logCounts
    , indexCounts
    ) where


import qualified Data.HashMap.Strict              as M
import           Data.Monoid
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO
import qualified Data.Text.Lazy                   as TL
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Builder.Int
import           Data.Text.Lazy.Builder.RealFloat
import qualified Data.Vector                      as V

import           WaTor.Types
import           WaTor.Utils


getCounts :: WaTor -> Counts
getCounts = rotate
          . V.foldl' indexCounts M.empty
          . v2dData
          . world
    where
        init :: (Sum Int, Sum Int)
        init = (Sum 0, Sum 1)
        deSum = both getSum

        rotate m = ( deSum $ M.lookupDefault init "fish" m
                   , deSum $ M.lookupDefault init "shark" m
                   , getSum . fst $ M.lookupDefault init "empty" m
                   )

logCounts :: WaTor -> Counts -> FilePath -> IO ()
logCounts w ((fc, fa), (sc, se), e) fp = TIO.appendFile fp
       . TL.toStrict
       . toLazyText
       $ cline (time w)
    where
        init :: (Sum Int, Sum Int)
        init = (Sum 0, Sum 1)
        getD :: Int -> Double
        getD = fromIntegral
        cline t =  decimal t <> "\t"
                <> decimal fc <> "\t" <> realFloat (getD fa / getD fc) <> "\t"
                <> decimal sc <> "\t" <> realFloat (getD se / getD sc) <> "\t"
                <> decimal e  <> "\n"

indexCounts :: M.HashMap T.Text (Sum Int, Sum Int) -> Entity -> M.HashMap T.Text (Sum Int, Sum Int)
indexCounts m Fish{fishAge}      = M.insertWith mappend "fish"  (Sum 1, Sum fishAge)     m
indexCounts m Shark{sharkEnergy} = M.insertWith mappend "shark" (Sum 1, Sum sharkEnergy) m
indexCounts m Empty              = M.insertWith mappend "empty" (Sum 1, Sum 0) m

