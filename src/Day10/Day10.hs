module Day10.Day10 where

import           Control.Arrow                  ( (&&&)
                                                , (***)
                                                )
import qualified Types                         as T
import           Text.Parsec             hiding ( parse )
import qualified Parsing                       as P
import qualified Data.Set                      as S
import           Data.Ord                       ( comparing )

data Point = Point { pos :: (Int, Int), velo :: (Int, Int) }

parse :: T.Parser [Point]
parse = traverse (P.parse point)
 where
  point = do
    _     <- string "position=<" <* spaces
    _pos  <- intTuple
    _     <- string "> velocity=<" <* spaces
    _velo <- intTuple
    _     <- string ">"
    pure (Point _pos _velo)

  intTuple = (,) <$> P.int <* (char ',' *> spaces) <*> P.int

boundingBox :: [(Int, Int)] -> ((Int, Int), (Int, Int))
boundingBox = (minMax *** minMax) . unzip where minMax = minimum &&& maximum

boxArea :: ((Int, Int), (Int, Int)) -> Int
boxArea ((minX, maxX), (minY, maxY)) = (maxX - minX) * (maxY - minY)

movePoint :: Int -> Point -> Point
movePoint n (Point (x, y) (x', y')) = Point (x + n * x', y + n * y') (x', y')

buildLines :: [(Int, Int)] -> [[String]]
buildLines poss = map rowString [0 .. maxY - minY]
 where
  ((minX, maxX), (minY, maxY)) = boundingBox poss
  posSet                       = S.fromList poss
  rowString row = map (mark row) [0 .. maxX - minX]
  mark y x = if (x + minX, y + minY) `S.member` posSet then "X" else "."

buildMessage :: [Point] -> String
buildMessage points =
  let t              = finalT points
      finalPositions = map (pos . movePoint t) points
  in  unlines . map concat $ buildLines finalPositions

firstLocalMinBy :: (a -> a -> Ordering) -> [a] -> a
firstLocalMinBy _ []  = error "empty list"
firstLocalMinBy _ [x] = x
firstLocalMinBy f (x : x' : xs) =
  if x `f` x' == GT then firstLocalMinBy f (x' : xs) else x

finalT :: [Point] -> Int
finalT points = firstLocalMinBy (comparing area) [0 ..]
  where area t = boxArea . boundingBox $ map (pos . movePoint t) points

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  solve1 = buildMessage
  solve2 = show . finalT
