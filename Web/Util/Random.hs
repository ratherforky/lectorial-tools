module Web.Util.Random where

import BasicPrelude
import System.Random
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

exampleGen :: StdGen
exampleGen = mkStdGen 42

-- >>> randomChoose [1,2,3,4] exampleGen
-- Just 1

randomChooseMaybeIO :: [a] -> IO (Maybe a)
randomChooseMaybeIO xs = randomChooseMaybe xs <$> getStdGen

randomChooseIO :: NonEmpty a -> IO a
randomChooseIO xs = randomChoose xs <$> getStdGen

randomChooseMaybe :: RandomGen g => [a] -> g -> Maybe a
randomChooseMaybe []     _   = Nothing
randomChooseMaybe (x:xs) gen = Just $ randomChoose (x NE.:| xs) gen

randomChoose :: RandomGen g => NonEmpty a -> g -> a
randomChoose xs gen = xs NE.!! randomIndex
  where
    n = length xs
    (randomIndex, _) = randomR (0, n-1) gen
