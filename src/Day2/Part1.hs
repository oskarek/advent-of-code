{-# LANGUAGE TupleSections #-}
module Day2.Part1 (solve) where

import           Data.Map    (Map, elems, fromListWith)
import           Text.Parsec (ParseError)

-- | Get the frequency count for each element in a list
frequencies :: Ord a => [a] -> Map a Int
frequencies = fromListWith (+) . map (,1)

-- | Return whether some element is repeated exactly n times
hasNRepeat :: Ord a => Int -> [a] -> Bool
hasNRepeat n = (n `elem`) . elems . frequencies

-- | Get the checksum number components for an ID
idNums :: String -> (Int, Int)
idNums id =
  ( if hasNRepeat 2 id then 1 else 0
  , if hasNRepeat 3 id then 1 else 0 )

-- | Calculate the accumulated sum of ID nums
sumIDNums :: [(Int, Int)] -> (Int, Int)
sumIDNums = foldr (\(x,y) (x',y') -> (x+x', y+y')) (0,0)

-- | Calculate the checksum for a list of IDs
calculateChecksum :: [String] -> Int
calculateChecksum = uncurry (*) . sumIDNums . map idNums

solve :: [String] -> String
solve = show . calculateChecksum
