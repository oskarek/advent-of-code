module Day2.Day2 where

import           Control.Arrow                  ( (&&&) )
import qualified Parsing                       as P
import           Text.Parsec             hiding ( parse )
import qualified Types                         as T

import qualified Day2.Part1                    as P1
import qualified Day2.Part2                    as P2

parse :: T.Parser [String]
parse = mapM (P.parse (many1 anyChar))

solve :: T.Solver
solve = fmap (P1.solve &&& P2.solve) . parse
