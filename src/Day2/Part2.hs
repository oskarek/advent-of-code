module Day2.Part2
    ( solve
    )
where

import           Data.List                      ( tails )

-- | Get number of diffing elements in two lists
diff :: Eq a => ([a], [a]) -> Int
diff (xs, ys) = length $ filter id $ zipWith (/=) xs ys

-- | Get all 2-combinations of a list
combos :: [a] -> [(a, a)]
combos xs = [ (x, y) | (x : rest) <- tails xs, y <- rest ]

-- | Get the pairwise similar (diff of 1) elements of a list
groupSimilar :: Eq a => [[a]] -> [([a], [a])]
groupSimilar = filter ((== 1) . diff) . combos

-- | Get the common elements between two lists
getCommons :: Eq a => ([a], [a]) -> [a]
getCommons (xs, ys) = map fst . filter (uncurry (==)) $ zip xs ys

solve :: [String] -> String
solve = unlines . map getCommons . groupSimilar
