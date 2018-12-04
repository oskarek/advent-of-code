module Day1.Part2 (solve) where

import qualified Data.Set as S

-- | Return the leftmost element that occurs more than once in the list
findDup :: Ord a => [a] -> Maybe a
findDup = dup S.empty
  where dup _ [] = Nothing
        dup seen (x:xs) = if x `S.member` seen
                            then Just x
                            else dup (x `S.insert` seen) xs

solve :: [Int] -> String
solve = show . findDup . scanl (+) 0 . cycle
