module Main where

import           Data.Set    (Set, insert, member)
import qualified Data.Set    as Set
import           InputParser (ParseRes, parseFreqChanges, runParseRes)

-- | Return the leftmost element that occurs more than once in the list
findDup :: Ord a => [a] -> Maybe a
findDup = dup Set.empty
  where dup _ [] = Nothing
        dup seen (x:xs) = if x `member` seen
                            then Just x
                            else dup (x `insert` seen) xs

firstDupFreq :: ParseRes (Maybe Integer)
firstDupFreq = findDup . scanl (+) 0 . cycle <$> parseFreqChanges

main :: IO ()
main = runParseRes firstDupFreq
          >>= either print print
