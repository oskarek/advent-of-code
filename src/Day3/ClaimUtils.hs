module Day3.ClaimUtils where

import           Control.Applicative (liftA2)
import           Data.Map            (Map, filterWithKey, keys, (!))
import           Utils               (freqs)

data Claim = Claim { claimID :: Int
                   , pos     :: (Int, Int)
                   , size    :: (Int, Int) } deriving (Eq, Show)

type Point = (Int, Int)

-- | Get the points covered by a claim
claimToPoints :: Claim -> [Point]
claimToPoints (Claim _ (x,y) (w,h)) = liftA2 (,) [x..x+w-1] [y..y+h-1]

-- | Get how many claims are covering each point
pointFreqs :: [Claim] -> Map Point Int
pointFreqs = freqs . concatMap claimToPoints

-- | Get all points that are covered by at least two claims
overlappedPoints :: [Claim] -> [Point]
overlappedPoints = keys . filterWithKey (\_ n -> n > 1) . pointFreqs

-- | Filter out only the claims that are not overlapping with any other claim
nonOverlapping :: [Claim] -> [Claim]
nonOverlapping cs = let freqs = pointFreqs cs
                        in filter (all ((== 1) . (freqs !)) . claimToPoints) cs
