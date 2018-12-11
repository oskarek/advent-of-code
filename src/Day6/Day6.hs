module Day6.Day6
  ( solve
  )
where

import           Control.Applicative            ( liftA2 )
import           Control.Arrow                  ( (&&&)
                                                , (***)
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import           Data.Ord                       ( comparing )
import qualified Parsing                       as P
import           Text.Parsec             hiding ( parse )
import qualified Types                         as T
import           Utils                          ( freqs
                                                , maxPairBy
                                                , minimumByExcl
                                                )

type Point = (Int, Int)

parse :: T.Parser [Point]
parse = traverse (P.parse point)
  where point = (,) <$> P.int <* string ", " <*> P.int

-- | Get the manhattan distance beteeen two points
dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- | Get the bounds of the map
getBounds :: [Point] -> (Point, Point)
getBounds points = (minMax *** minMax) $ unzip points
  where minMax = minimum &&& maximum

-- | Get the entire area of points
getArea :: (Point, Point) -> [Point]
getArea ((minX, maxX), (minY, maxY)) = liftA2 (,) [minX .. maxX] [minY .. maxY]

-- | Make a map pointing each place in the map to its nearest point
makeMinDistMap :: [Point] -> [Point] -> M.Map Point (Maybe Point)
makeMinDistMap area points = M.fromList $ map minDist area
  where minDist place = (place, minimumByExcl (comparing $ dist place) points)

-- | Get the points which cover an infinite area
infinites :: (Point, Point) -> M.Map Point (Maybe Point) -> [Point]
infinites ((minX, maxX), (minY, maxY)) minMap =
  catMaybes $ M.elems $ M.filterWithKey (\p _ -> onEdge p) minMap
  where onEdge (x, y) = x `elem` [minX, maxX] || y `elem` [minY, maxY]

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  solve1 points =
    let bounds = getBounds points
        area   = getArea bounds
        minMap = makeMinDistMap area points
        infs   = infinites bounds minMap
    in  show
        . snd
        . maxPairBy id
        . freqs
        . filter (`notElem` infs)
        . catMaybes
        . M.elems
        $ minMap
  solve2 points =
    let area = getArea (getBounds points)
        distSum place = sum (map (dist place) points)
    in  show $ length $ filter ((< 10000) . distSum) area
