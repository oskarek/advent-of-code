module Parsing where

import qualified Text.Parsec                   as P
import qualified Text.Parsec.String            as PS
import qualified Types                         as T
import Data.Functor ((<$))

modif :: PS.Parser (Int -> Int)
modif = negate <$ P.char '-' P.<|> pure id

int :: PS.Parser Int
int = modif <*> (read <$> P.many1 P.digit <* P.spaces)

parse :: PS.Parser a -> String -> T.ParseRes a
parse p = P.parse p ""
