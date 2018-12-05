{-# LANGUAGE TupleSections #-}
module Utils where

import           Data.Map (Map)
import qualified Data.Map as M

-- | Get the frequency count for each element in a list
freqs :: Ord a => [a] -> Map a Int
freqs = M.fromListWith (+) . map (,1)
