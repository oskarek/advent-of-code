module IDParser where

type ID = String

getIDs :: IO [ID]
getIDs = lines <$> getContents
