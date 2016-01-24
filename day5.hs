
-- Part 1
part1 :: IO ()
part1 = readFile "day5Input.txt" >>= print . length . filter isNice . lines 

isNice :: String -> Bool
isNice s = all ($ s) [not . hasDisallowedStrings, hasDouble, hasAtLeast3Vowels]

hasAtLeast3Vowels :: String -> Bool
hasAtLeast3Vowels = (>= 3) . length . filter (`elem` "aeiou")

hasDouble :: String -> Bool
hasDouble = any (\[a,b] -> a == b) . windows 2

hasDisallowedStrings :: String -> Bool
hasDisallowedStrings = any (`elem` ["ab","cd","pq","xy"]) . windows 2

-- sliding window
-- windows 2 "abcd" == ["ab","bc","cd"]
windows :: Int -> [a] -> [[a]]
windows n xs
  | length v < n = []
  | otherwise    = v : windows n (tail xs)
  where
    v = take n xs

-- Part 2:
part2 :: IO ()
part2 = readFile "day5Input.txt" >>= print . length . filter isNice2 . lines 

isNice2 :: String -> Bool
isNice2 s = all ($ s) [has2EqualPairs, hasLetterSandwich]

hasLetterSandwich :: String -> Bool
hasLetterSandwich = any (\[a,_,b] -> a == b) . windows 3

has2EqualPairs :: String -> Bool
has2EqualPairs       [] = False
has2EqualPairs      [_] = False
has2EqualPairs (a:b:xs) = [a,b] `elem` windows 2 xs || has2EqualPairs (b:xs)