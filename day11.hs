import Data.List (nub, find)


part1and2 = take 2 . filter isValid . tail $ iterate succR "hepxcrrq"

succR :: String -> String
succR = reverse . succL . reverse
  where succL "z"      = "aa"
        succL ('z':xs) = 'a' : succL xs
        succL (x:xs)   = succ x : xs


-- Check the three password requirements
isValid :: String -> Bool
isValid s = all ($ s) [not . hasDisallowedChars, hasStraight, has2UniquePairs]

hasStraight :: String -> Bool
hasStraight = any isStraight . windows 3
  where isStraight [a,b,c] = succ a == b && succ b == c

hasDisallowedChars :: String -> Bool
hasDisallowedChars = any (`elem` "iol")

has2UniquePairs :: String -> Bool
has2UniquePairs = (>= 2) . length . nub . filter (\[a,b] -> a == b) . windows 2

-- sliding window
-- windows 2 "abcd" == ["ab","bc","cd"]
windows :: Int -> [a] -> [[a]]
windows n xs
  | length v < n = []
  | otherwise    = v : windows n (tail xs)
  where
    v = take n xs