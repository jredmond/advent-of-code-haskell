{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text (Parser, satisfy, notChar, count, many1, option, parseOnly)
import Control.Applicative
import Data.Text (Text, pack)
import Data.Char (isHexDigit)


-- part 1


-- Parser
type MemString = [MemChar]
data MemChar = Backslash | Quote | Hex | Character deriving (Show)

-- Part 1
part1 :: IO ()
part1 = readFile "day8Input.txt" >>= print . (\ls -> total ls - sumMemChar ls) . lines
  where total = sum . map length

sumMemChar :: [String] -> Int
sumMemChar = sum . map (length . parse . pack)

parse :: Text -> MemString
parse = success . parseOnly memStringP
  where success (Right a) = a

memStringP :: Parser MemString
memStringP = "\"" *> option [] (many1 memCharP) <* "\""

memCharP :: Parser MemChar
memCharP = backslashP <|> quoteP <|> hexP <|> characterP

backslashP :: Parser MemChar
backslashP = "\\\\" *> return Backslash

quoteP :: Parser MemChar
quoteP = "\\\"" *> return Quote

hexP :: Parser MemChar
hexP = "\\x" *> count 2 (satisfy isHexDigit) *> return Hex

characterP :: Parser MemChar
characterP = notChar '\"' *> return Character