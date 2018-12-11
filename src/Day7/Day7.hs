module Day7.Day7 where

import           Control.Arrow                  ( (&&&)
                                                , (***)
                                                )
import qualified Types                         as T
import           Text.Parsec             hiding ( parse
                                                , State
                                                )
import qualified Parsing                       as P
import qualified Data.Map                      as M
import           Data.Map                       ( Map )
import           Control.Monad.Trans.State
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as S
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import           Safe                           ( headMay )
import           Data.List                      ( sort
                                                , nub
                                                , partition
                                                )
import           Data.Char                      ( ord )

newtype Step = Step Char deriving (Eq, Ord)
instance Show Step where
  show (Step c) = [c]

parse :: T.Parser [(Step, Step)]
parse = traverse (P.parse rel)
 where
  rel = do
    _ <- string "Step "
    a <- Step <$> anyChar
    _ <- string " must be finished before step "
    b <- Step <$> anyChar
    _ <- string " can begin."
    pure (b, a)

buildMap :: [(Step, Step)] -> Map Step (Set Step)
buildMap = M.fromListWith (<>) . addEmpties . (map . fmap) S.singleton
 where
  addEmpties steps =
    let empties = uncurry (flip (\\)) . (S.fromList *** mconcat) $ unzip steps
    in  steps ++ map (, S.empty) (S.toList empties)

nextSteps :: Map Step (Set Step) -> [Step]
nextSteps = M.keys . M.filter null

clearStep :: Step -> Map Step (Set Step) -> Map Step (Set Step)
clearStep s = M.filterWithKey (\k _ -> k /= s) . (M.map . S.filter) (/= s)

mapToSteps :: Map Step (Set Step) -> [Step]
mapToSteps = evalState steps
 where
  steps = headMay . nextSteps <$> get >>= maybe (pure []) addStep
  addStep step = modify (clearStep step) >> (step :) <$> steps

data StepPeriod = SP { step :: Step, start :: Int, end :: Int }

totDuration :: [StepPeriod] -> Int
totDuration = maybe (-1) end . headMay

toWorkPeriod :: Int -> Step -> StepPeriod
toWorkPeriod start step = SP { step  = step
                             , start = start
                             , end   = start + duration step - 1
                             }
  where duration (Step c) = ord c - 4

mapToDuration :: Map Step (Set Step) -> Int
mapToDuration = go (replicate 5 []) [] 0
 where
  go periods accSteps time m =
    let
      (available, busy) = partition ((< time) . totDuration) periods
      finished          = mapMaybe (fmap step . headMay) available
      inProgress        = mapMaybe (fmap step . headMay) busy
      m'                = foldr clearStep m finished
      nexts = filter (`notElem` inProgress) . nub $ nextSteps m' ++ accSteps
      (now, later)      = splitAt (length available) $ sort nexts
      newWorkPeriods    = map (toWorkPeriod time) now
      available' =
        zipWith (++) (map (: []) newWorkPeriods ++ repeat []) available
    in
      if M.null m' then time else go (available' ++ busy) later (time + 1) m'

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  solve1 = concatMap show . mapToSteps . buildMap
  solve2 = show . mapToDuration . buildMap
