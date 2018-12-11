module Types where

import           Text.Parsec                    ( ParseError )

type ParseRes a = Either ParseError a
type Parser a = [String] -> ParseRes a
type Solver = [String] -> ParseRes (String, String)
