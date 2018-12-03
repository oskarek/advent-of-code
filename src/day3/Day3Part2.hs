module Main where

import           ClaimParser (getClaims)
import           ClaimUtils  (nonOverlapping)

main :: IO ()
main = nonOverlapping <$> getClaims
          >>= print
