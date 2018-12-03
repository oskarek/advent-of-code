module ClaimParser where

import           Text.Parsec
import           Text.Parsec.String (Parser)

data Claim = Claim { claimID :: Int
                   , pos     :: (Int, Int)
                   , size    :: (Int, Int) } deriving (Eq, Show)

int, parseID :: Parser Int
int = read <$> many1 digit
parseID = char '#' *> int <* spaces

parsePos, parseSize :: Parser (Int, Int)
parsePos = (,) <$> int <* char ',' <*> int
parseSize = (,) <$> int <* char 'x' <*> int <* spaces

claim :: Parser Claim
claim = Claim <$>
  parseID <* char '@' <* spaces <*>
  parsePos <* char ':' <* spaces <*>
  parseSize

getClaims :: IO [Claim]
getClaims = either (const []) id . parse (many claim) "" <$> getContents
