module Day4.Day4
  ( solve
  )
where

import           Control.Applicative            ( liftA2
                                                , (<$)
                                                )
import           Control.Arrow                  ( second
                                                , (&&&)
                                                )
import           Data.List                      ( sort )
import qualified Data.Map                      as M
import qualified Parsing                       as P
import           Text.Parsec             hiding ( parse )
import qualified Types                         as T
import           Utils                          ( freqs
                                                , maxPairBy
                                                )

data Event = BeginsShift Int | FallsAsleep | WakesUp deriving (Eq, Show)
data Record = Record Int Event deriving (Eq, Show)

parse :: T.Parser [Record]
parse = mapM (P.parse record) . sort
 where
  record         = liftA2 Record parseTimestamp parseEvent
  parseTimestamp = manyTill anyChar (char ':') *> P.int <* string "] "
  parseEvent =
    FallsAsleep
      <$  string "falls asleep"
      <|> WakesUp
      <$  string "wakes up"
      <|> BeginsShift
      <$> (string "Guard #" *> P.int <* string "begins shift")

-- | Build up a map that describes how many times each guard has been asleep, for each minute
buildGuardMap :: [Record] -> M.Map Int (M.Map Int Int)
buildGuardMap = build 0 0
 where
  build _     _     []                         = M.empty
  build guard start (Record time event : recs) = case event of
    BeginsShift g -> build g start recs
    FallsAsleep   -> build guard time recs
    WakesUp       -> (M.unionWith . M.unionWith)
      (+)
      (M.singleton guard (freqs [start .. time - 1]))
      (build guard 0 recs)

getMaxGuardOn :: ([Int] -> Int) -> [Record] -> Int
getMaxGuardOn f =
  uncurry (*)
    . second (fst . maxPairBy id)
    . maxPairBy (f . M.elems)
    . buildGuardMap

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  solve1 = show . getMaxGuardOn sum
  solve2 = show . getMaxGuardOn maximum
