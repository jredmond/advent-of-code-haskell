import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Tuple (swap)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)

-- Part 1
type Replacement = (String, String)

main = do
  input <- readFile "day19Input.txt"
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 input = let (s:_:mappings) = reverse $ lines input
                  reps = parse mappings
              in S.size $ uniqueMols reps s

parse :: [String] -> [Replacement]
parse = map ((\[from,_,to] -> (from,to)) . words)

singleReplacements :: Replacement -> String -> [String]
singleReplacements (k,v) src = 
    map (intercalate k) [ front ++ (x ++ v ++ y) : back
                        | i <- [1..length pieces - 1]
                        , let (front,x:y:back) = splitAt (i-1) pieces]
  where pieces = splitOn k src

uniqueMols :: [Replacement] -> String -> HashSet String
uniqueMols reps src = S.fromList $ concat [ singleReplacements rep src | rep <- reps]

-- Part 2
uniquePredecessors :: [Replacement] -> String -> HashSet String
uniquePredecessors reps src = S.fromList $ concat [ singleReplacements (swap rep) src | rep <- reps]

-- Depth first search backwards from starting string
findStepsBackToElectron :: [Replacement] -> String -> Int
findStepsBackToElectron reps = fromJust . f 0
  where f step []  = Nothing
        f step "e" = Just step
        f step str = listToMaybe . mapMaybe (f (step+1)) . S.toList $ uniquePredecessors reps str

part2 :: String -> Int
part2 input = let (s:_:mappings) = reverse $ lines input
                  reps = parse mappings
              in findStepsBackToElectron reps s 