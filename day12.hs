import Data.Char (isDigit)
import Data.List (isInfixOf)
import Control.Applicative

input = concat . lines <$> readFile "day12Input.txt"
part1 = sum . parse <$> input >>= print
part2 = sum . parse2 <$> input >>= print

parse :: String -> [Int]
parse [] = []
parse (x:xs) 
  | x == '-'  = (\(n,rest) -> read ('-':n) : parse rest) $ span isDigit xs
  | isDigit x = (\(n,rest) -> read n : parse rest) $ span isDigit (x:xs)
  | otherwise = parse xs

parse2 :: String -> [Int]
parse2 [] = []
parse2 (x:xs) 
    | x == '{'  = f $ betweenOneSet (x:xs)
    | x == '-'  = (\(n,rest) -> read ('-':n) : parse2 rest) $ span isDigit xs
    | isDigit x = (\(n,rest) -> read n : parse2 rest) $ span isDigit (x:xs)
    | otherwise = parse2 xs
  where f (object,rest) = if "red" `isInfixOf` object 
                          then parse2 rest 
                          else parse2 xs

betweenOneSet :: String -> (String,String)
betweenOneSet xs = let contents = f 0 0 xs in (contents, drop (length contents) xs) 
  where f i j [] = []
        f i j (x:xs)
          | x == '{'           = '.' : f (i + 1) j xs
          | x == '}' && i <= 1 = "."
          | x == '}'           = '.' : f (i - 1) j xs
          | x == '['           = '[' : f i (j+1) xs
          | x == ']'           = ']' : f i (j-1) xs
          | x == 'e' && j > 0  = '!' : f i j xs
          | i == 1             = x : f i j xs
          | otherwise          = '.' : f i j xs