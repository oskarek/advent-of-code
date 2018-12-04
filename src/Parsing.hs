module Parsing where

import           Data.Void
import qualified Text.Parsec        as P
import qualified Text.Parsec.String as PS
import qualified Types              as T

int :: PS.Parser Int
int = read <$> P.many1 P.digit

parse :: PS.Parser a -> String -> T.ParseRes a
parse p = P.parse p ""
