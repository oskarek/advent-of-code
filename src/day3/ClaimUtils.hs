{-# LANGUAGE TupleSections #-}
module ClaimUtils where

import           ClaimParser (Claim (Claim))
import           Data.Map    (Map, filterWithKey, fromListWith, keys, (!))

type Point = (Int, Int)

-- | Get the points covered by a claim
claimToPoints :: Claim -> [Point]
claimToPoints (Claim _ (x,y) (w,h)) = [x..x+w-1] >>= \x -> map (x,) [y..y+h-1]

-- | Get how many claims are covering each point
pointFreqs :: [Claim] -> Map Point Int
pointFreqs = fromListWith (+) . map (,1) . concatMap claimToPoints

-- | Get all points that are covered by at least two claims
overlappedPoints :: [Claim] -> [Point]
overlappedPoints = keys . filterWithKey (\_ n -> n > 1) . pointFreqs

-- | Filter out only the claims that are not overlapping with any other claim
nonOverlapping :: [Claim] -> [Claim]
nonOverlapping cs = let freqs = pointFreqs cs
                        in filter (all ((== 1) . (freqs !)) . claimToPoints) cs
