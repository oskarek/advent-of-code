module Main where

import           ClaimParser (getClaims)
import           ClaimUtils  (overlappedPoints)

main :: IO ()
main = length . overlappedPoints <$> getClaims
          >>= print
