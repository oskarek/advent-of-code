module Day5.Day5
  ( solve
  )
where

import           Control.Arrow                  ( (&&&) )
import           Data.Char                      ( toLower )
import qualified Types                         as T

parse :: T.Parser String
parse = Right . head

react :: String -> String
react = foldr (<:>) ""

(<:>) :: Char -> String -> String
c <:> []       = [c]
c <:> (c' : s) = if c `anti` c' then s else c : c' : s
  where c1 `anti` c2 = toLower c1 == toLower c2 && c1 /= c2

removeUnit :: Char -> String -> String
removeUnit unit = filter ((/= unit) . toLower)

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  solve1 = show . length . react
  solve2 s =
    show . minimum $ map (length . react . flip removeUnit s) ['a' .. 'z']
