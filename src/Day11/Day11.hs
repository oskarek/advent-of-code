module Day11.Day11 where

import           Control.Arrow                  ( (&&&) )
import qualified Types                         as T
import qualified Parsing                       as P
import qualified Data.Map                      as M
import           Utils                          ( maxPairBy )

parse :: T.Parser Int
parse = P.parse P.int . head

makeGrid :: Int -> Int -> M.Map (Int, Int) Int
makeGrid serial gridSize = M.fromList $ zip pos' (map (powerLevel serial) pos')
  where pos' = (,) <$> [1 .. gridSize] <*> [1 .. gridSize]

squares :: M.Map (Int, Int) Int -> Int -> Int -> M.Map (Int, Int, Int) Int
squares grid gridSize sqSize = M.fromList $ map makeSquare grid'
 where
  grid' = (,) <$> [1 .. gridSize - sqSize + 1] <*> [1 .. gridSize - sqSize + 1]
  makeSquare (x, y) = ((x, y, sqSize), totPow)
   where
    totPow          = sum $ map (grid M.!) squarePositions
    squarePositions = (,) <$> [x .. x + sqSize - 1] <*> [y .. y + sqSize - 1]

powerLevel :: Int -> (Int, Int) -> Int
powerLevel serial (x, y) =
  let rackID = x + 10
  in  subtract 5 . (`mod` 10) . (`div` 100) $ (rackID * y + serial) * rackID

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  largestSquare = fst . maxPairBy id
  solve1 serial = show . largestSquare $ squares grid 300 3
    where grid = makeGrid serial 300
  solve2 serial = show . largestSquare $ mconcat
    (map (squares grid 300) [1 .. 300])
    where grid = makeGrid serial 300
