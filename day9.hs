import Control.Applicative
import Data.List (nub, permutations, find, concatMap)
import Data.Maybe (mapMaybe)

data Edge = Edge String String Int deriving (Show)

cities :: [Edge] -> [String]
cities = nub . concatMap f
  where f (Edge city1 city2 _) = [city1, city2]

parse :: [String] -> [Edge]
parse = map (f . words) 
  where f [a,_,b,_,dist] = Edge a b (read dist)

input = lines <$> readFile "day9Input.txt"
part1 = minimum . allRouteDistances . parse <$> input >>= print
part2 = maximum . allRouteDistances . parse <$> input >>= print

allRouteDistances :: [Edge] -> [Int]
allRouteDistances es = map (routeDistance es) . permutations . cities $ es

routeDistance :: [Edge] -> [String] -> Int
routeDistance es = sum . mapMaybe f . pairs
  where f (x, y) = getDistance x y es

getDistance :: String -> String -> [Edge] -> Maybe Int
getDistance x y es = getCost <$> find f es
  where f (Edge x' y' _) 
         | x == x' && y == y' = True
         | x == y' && y == x' = True
         | otherwise          = False

getCost :: Edge -> Int
getCost (Edge _ _ i) = i

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)