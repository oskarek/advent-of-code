{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Day9.Day9 where

import           Control.Arrow                  ( (&&&) )
import qualified Types                         as T
import           Text.Parsec             hiding ( parse )
import qualified Parsing                       as P
import qualified Data.List.PointedList         as PL
import qualified Data.List.PointedList.Circular
                                               as PLC
import           Control.Lens
import           Data.Maybe                     ( fromJust )

data GameInfo = GI { _playerQuant :: Int, _lastMarbleWorth :: Int } deriving Show
data Player = PL { _id :: Int, _score :: Int } deriving Show

makeLenses ''GameInfo
makeLenses ''Player

parse :: T.Parser GameInfo
parse = P.parse gameInfo . head
 where
  gameInfo = do
    quant <- P.int <* string "players; "
    worth <- string "last marble is worth " *> P.int <* string "points"
    pure $ GI quant worth

playGame :: GameInfo -> PL.PointedList Player
playGame GI {..} = snd
  $ foldl go2 (PLC.singleton 0, players) [1 .. _lastMarbleWorth]
 where
  players = fromJust $ PL.fromList $ map (flip PL 0) [1 .. _playerQuant]
  go2 (game, _players) worth
    | worth `mod` 23 == 0
    = let movedGame      = PLC.moveN (-7) game
          worth'         = movedGame ^. PLC.focus
          updatedGame    = fromJust . PLC.delete $ movedGame
          updatedPlayers = _players & (PLC.focus . score) +~ (worth + worth')
      in  (updatedGame, PLC.next updatedPlayers)
    | otherwise
    = (PLC.insert worth (PLC.next game), PLC.next _players)

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  solve1 = show . maximum . fmap _score . playGame
  solve2 = solve1 . (lastMarbleWorth *~ 100)
