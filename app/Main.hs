module Main where

import           Data.Map            (Map, (!?))
import qualified Data.Map            as M
import           Data.Semigroup      ((<>))
import           Options.Applicative hiding (ParseError)
import           Text.Parsec         (ParseError)
import qualified Types               as T

import qualified Day1.Day1           as Day1
import qualified Day2.Day2           as Day2
import qualified Day3.Day3           as Day3
import qualified Day4.Day4           as Day4
import qualified Day5.Day5           as Day5
import qualified Day6.Day6           as Day6

solvers :: Map Integer T.Solver
solvers = M.fromList [ (1, Day1.solve)
                     , (2, Day2.solve)
                     , (3, Day3.solve)
                     , (4, Day4.solve)
                     , (5, Day5.solve)
                     , (6, Day6.solve)
                     ]

printSolutions :: (String, String) -> IO ()
printSolutions (s1, s2) = putStrLn $ "Solution to part 1:\n" ++ s1
                                  ++ "\n\n"
                                  ++ "Solution to part 2:\n" ++ s2

main :: IO ()
main = do
  day <- execParser opts
  case solvers !? day of
    Nothing -> putStrLn $ "Day " ++ show day ++ " problem not solved yet!"
    Just solver -> readFile ("input/day" ++ show day ++ ".txt")
                      >>= either print printSolutions . solver . lines
  where
    opts = info (day <**> helper)
      ( fullDesc
      <> progDesc "Solve the puzzles for DAY"
      <> header "Advent of Code 2018 solutions" )
    day = argument auto (help "The number of the day to solve" <> metavar "DAY")
