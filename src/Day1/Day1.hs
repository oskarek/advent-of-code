module Day1.Day1 where

import           Control.Arrow ((&&&))
import qualified Parsing       as P
import           Text.Parsec   hiding (parse)
import qualified Types         as T

import qualified Day1.Part1    as P1
import qualified Day1.Part2    as P2

parse :: T.Parser [Int]
parse = mapM (P.parse freqChange)
  where freqChange = modifier <*> P.int <* spaces
        modifier = negate <$ char '-' <|> id <$ char '+'

solve :: T.Solver
solve = fmap (P1.solve &&& P2.solve) . parse
