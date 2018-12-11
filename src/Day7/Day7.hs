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
import           Data.List                      ( partition )
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

nextSteps :: Map Step (Set Step) -> Set Step
nextSteps = M.keysSet . M.filter null

clearSteps :: Foldable t => t Step -> Map Step (Set Step) -> Map Step (Set Step)
clearSteps = flip (foldr clearStep)
 where
  clearStep s = M.filterWithKey (\k _ -> k /= s) . (M.map . S.filter) (/= s)

mapToSteps :: Map Step (Set Step) -> [Step]
mapToSteps = evalState steps
 where
  steps = S.toAscList . nextSteps <$> get >>= addSteps
  addSteps [] = pure []
  addSteps ss = modify (clearSteps ss) >> (ss ++) <$> steps

data StepPeriod = SP { step :: Step, start :: Int, end :: Int }

toStepPeriod :: Int -> Step -> StepPeriod
toStepPeriod _start s@(Step c) =
  SP { step = s, start = _start, end = _start + ord c - 5 }

mapToDuration :: Map Step (Set Step) -> Int
mapToDuration = go (replicate 5 []) S.empty 0
 where
  go periods accSteps time m =
    let (available, busy      ) = partition ((< time) . endTime) periods
        (finished , inProgress) = (lastSteps *** lastSteps) (available, busy)
        m'                      = clearSteps finished m
        nexts                   = (nextSteps m' <> accSteps) \\ inProgress
        (now, later)            = splitAt (length available) $ S.toAscList nexts
        newStepPeriods          = map (toStepPeriod time) now
        available'              = newStepPeriods `appendStepsTo` available
    in  if M.null m'
          then time
          else go (available' ++ busy) (S.fromList later) (time + 1) m'

  lastSteps = S.fromList . mapMaybe (fmap step . headMay)
  appendStepsTo work = zipWith (++) (map (: []) work ++ repeat [])
  endTime = maybe (-1) end . headMay

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  solve1 = concatMap show . mapToSteps . buildMap
  solve2 = show . mapToDuration . buildMap
