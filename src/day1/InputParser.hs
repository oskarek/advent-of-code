module InputParser where

import           Control.Applicative        ((<$))
import           Control.Monad.Trans.Except
import           Text.Parsec
import           Text.Parsec.String         (Parser)

-- ParseRes monad transformer
type ParseRes = ExceptT ParseError IO

runParseRes :: ParseRes a -> IO (Either ParseError a)
runParseRes = runExceptT

-- parsing
modifier :: Parser (Integer -> Integer)
modifier = negate <$ char '-'
           <|> id <$ char '+'

freqChange :: Parser Integer
freqChange = modifier <*> number <* spaces
  where number = read <$> many1 digit

parseFreqChanges :: ParseRes [Integer]
parseFreqChanges = ExceptT (parse (many freqChange) "" <$> getContents)
