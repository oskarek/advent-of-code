module Day1.Day1 where

import           Control.Arrow                  ( (&&&) )
import qualified Parsing                       as P
import           Text.Parsec             hiding ( parse )
import qualified Types                         as T

import           Utils                          ( findDup )

parse :: T.Parser [Int]
parse = mapM (P.parse freqChange)
 where
  freqChange = modifier <*> P.int <* spaces
  modifier   = negate <$ char '-' <|> id <$ char '+'

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  solve1 = show . sum
  solve2 = show . findDup . scanl (+) 0 . cycle
