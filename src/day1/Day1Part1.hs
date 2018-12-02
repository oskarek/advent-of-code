module Main where

import           InputParser (ParseRes, parseFreqChanges, runParseRes)

finalFreq :: ParseRes Integer
finalFreq = sum <$> parseFreqChanges

main :: IO ()
main = runParseRes finalFreq
          >>= either print print
