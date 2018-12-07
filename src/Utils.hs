{-# LANGUAGE TupleSections #-}
module Utils where

import           Data.List (maximumBy, sortBy)
import           Data.Map  (Map)
import qualified Data.Map  as M
import           Data.Ord  (comparing)

-- | Get the frequency count for each element in a list
freqs :: Ord a => [a] -> Map a Int
freqs = M.fromListWith (+) . map (,1)

-- | Get the maximum key and value pair by comparing the values by the given function
maxPairBy :: Ord a' => (a -> a') -> M.Map k a -> (k, a)
maxPairBy f = maximumBy (comparing (f . snd)) . M.toList

-- | Get an exclusive minimum value if one exists
minimumByExcl :: Eq a => (a -> a -> Ordering) -> [a] -> Maybe a
minimumByExcl comp l = case sortBy comp l of
    []        -> Nothing
    [x]       -> Just x
    (x:x':xs) -> if x `comp` x' == EQ then Nothing else Just x