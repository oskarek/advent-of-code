{-# LANGUAGE TupleSections #-}
module Day4.Day4 (solve) where

import           Control.Applicative (liftA2, (<$))
import           Control.Arrow       (second, (&&&))
import           Data.List           (maximumBy, sort)
import qualified Data.Map            as M
import           Data.Ord            (comparing)
import qualified Parsing             as P
import           Text.Parsec         hiding (parse)
import qualified Types               as T

data Event = BeginsShift Int | FallsAsleep | WakesUp deriving (Eq, Show)
data Record = Record Int Event deriving (Eq, Show)

parse :: T.Parser [Record]
parse = mapM (P.parse record) . sort
  where record = liftA2 Record parseTimestamp parseEvent
        parseTimestamp = manyTill anyChar (char ':') *> P.int <* string "] "
        parseEvent =  FallsAsleep <$ string "falls asleep"
                  <|> WakesUp <$ string "wakes up"
                  <|> BeginsShift <$> (string "Guard #" *> P.int <* string " begins shift")

-- | Build up a map that describes how many times each guard has been asleep, for each minute
buildGuardMap :: [Record] -> M.Map Int (M.Map Int Int)
buildGuardMap = build 0 0
  where build _ _ [] = M.empty
        build guard start (Record time event : recs) = case event of
          BeginsShift g -> build g start recs
          FallsAsleep   -> build guard time recs
          WakesUp -> let freqs = M.fromList $ map (,1) [start .. time-1]
                         in (M.unionWith . M.unionWith) (+) (M.singleton guard freqs) $ build guard 0 recs

-- | Get the maximum key and value pair by comparing the keys by the given function
maxPairBy :: Ord k' => (k -> k') -> M.Map a k -> (a, k)
maxPairBy f = maximumBy (comparing (f . snd)) . M.toList

getMaxGuardOn :: ([Int] -> Int) -> [Record] -> Int
getMaxGuardOn f = uncurry (*) . second (fst . maxPairBy id) . maxPairBy (f . M.elems) . buildGuardMap

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
  where solve1 = show . getMaxGuardOn sum
        solve2 = show . getMaxGuardOn maximum
