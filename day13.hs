import Control.Applicative
import Data.List (nub, permutations, find, concatMap)

data SeatingPair = SeatingPair String String Int deriving (Show)

parse :: [String] -> [SeatingPair]
parse = map (f . words) 
  where f [a,_,t,h,_,_,_,_,_,_,b]
         | t == "gain" = SeatingPair a (init b) (read h)
         | t == "lose" = SeatingPair a (init b) (negate $ read h)

guests :: [SeatingPair] -> [String]
guests = nub . concatMap f
  where f (SeatingPair guest1 guest2 _) = [guest1, guest2]

getHappyness :: String -> String -> [SeatingPair] -> Int
getHappyness x y es = sum . map (\(SeatingPair _ _ h) -> h) $ filter f es
  where f (SeatingPair x' y' _)
         | x == x' && y == y' = True
         | x == y' && y == x' = True
         | otherwise          = False

evaluateArrangement :: [SeatingPair] -> [String] -> Int
evaluateArrangement ps = sum . map f . pairs . (\xs -> xs ++ [head xs])
  where f (x, y) = getHappyness x y ps

allSeatingArrangements :: [SeatingPair] -> [Int]
allSeatingArrangements es = map (evaluateArrangement es) . permutations . guests $ es

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

input = lines <$> readFile "day13Input.txt"
part1 = maximum . allSeatingArrangements . parse <$> input >>= print

-- Part 2
allSeatingArrangementsInclMe :: [SeatingPair] -> [Int]
allSeatingArrangementsInclMe es = map (evaluateArrangement es) . permutations . (\gs -> "Me":gs) . guests $ es

part2 = maximum . allSeatingArrangementsInclMe . parse <$> input >>= print
