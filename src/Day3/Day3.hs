module Day3.Day3 where

import           Control.Arrow                  ( (&&&) )
import           Day3.ClaimUtils                ( Claim(..)
                                                , nonOverlapping
                                                , overlappedPoints
                                                )
import qualified Parsing                       as P
import           Text.Parsec             hiding ( parse )
import           Text.Parsec.String             ( Parser )
import qualified Types                         as T

claim :: Parser Claim
claim =
  Claim <$> parseID <* string "@ " <*> parsePos <* string ": " <*> parseSize
 where
  parseID   = char '#' *> P.int
  parsePos  = (,) <$> P.int <* char ',' <*> P.int
  parseSize = (,) <$> P.int <* char 'x' <*> P.int

parse :: T.Parser [Claim]
parse = mapM (P.parse claim)

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  solve1 = show . length . overlappedPoints
  solve2 = unlines . map show . nonOverlapping
