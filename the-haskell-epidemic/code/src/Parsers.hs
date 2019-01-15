module Parsers where

import Text.Parsec

wordList :: Parsec String () [String]
wordList =
  between (char '[') (char ']') (word `sepBy` char ',')
  where
    word = many1 letter
