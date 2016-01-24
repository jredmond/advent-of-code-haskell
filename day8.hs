import Prelude ()
import BasePrelude

-- Decode the three escaped characters and the surrounding
-- double quotes
decode :: String -> String
decode ('\"':xs) = f xs
  where f ('\\':'\\':xs)    = '\\':f xs 
        f ('\\':'\"':xs)    = '\"':f xs
        f ('\\':'x':x:y:xs) = '!':f xs
        f "\""              = []
        f (x:xs)            = x:f xs
        f []                = []

encode :: String -> String
encode xs = "\"" <> f xs <> "\""
  where f ('\\':xs)         = "\\\\" <> f xs 
        f ('\"':xs)         = "\\\"" <> f xs
        f (x:xs)            = x:f xs
        f []                = []

input = lines <$> readFile "day8Input.txt"
output f = (,) <$> (sum . map length <$> input) 
             <*> (sum . map (length . f) <$> input)
             >>= print

part1 = output decode
part2 = output encode