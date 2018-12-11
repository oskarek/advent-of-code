{-# LANGUAGE RecordWildCards #-}
module Day8.Day8 where

import           Control.Arrow                  ( (&&&) )
import qualified Types                         as T
import           Text.Parsec             hiding ( parse )
import qualified Parsing                       as P
import           Safe                           ( atMay )
import           Data.Maybe                     ( mapMaybe )

data Node = Node { subnodes :: [Node], metadata :: [Int] }

parse :: T.Parser Node
parse = P.parse node . head
 where
  node = do
    subnodeQuantity <- P.int
    metaQuantity    <- P.int
    subnodes_       <- count subnodeQuantity node
    metadata_       <- count metaQuantity P.int
    pure (Node subnodes_ metadata_)

nodeValue :: Node -> Int
nodeValue (Node [] metas) = sum metas
nodeValue Node {..} =
  let refNodes = mapMaybe (\i -> subnodes `atMay` (i - 1)) metadata
  in  sum $ map nodeValue refNodes

solve :: T.Solver
solve = fmap (solve1 &&& solve2) . parse
 where
  solve1 = show . sum . metas
    where metas Node {..} = metadata ++ concatMap metas subnodes
  solve2 = show . nodeValue
