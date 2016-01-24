import Control.Applicative
import Data.List.Split (split, keepDelimsL, whenElt)
import Data.Char (isUpper)
import qualified Data.Set as Set
import Data.Sequence (Seq, fromList, elemIndicesL, update)
import Data.Foldable (toList)
import Data.Maybe (isJust, fromJust)
import Data.List (find, findIndex)

-- Part 1
type Replacement = (String, String)

input = parse . lines <$> readFile "day19Input.txt"
input2 = readFile "day19Input2.txt"
part1 = do
    reps <- input
    initialMol <- input2
    let allUnique = oneStep reps initialMol
    return (length allUnique)

parse :: [String] -> [Replacement]
parse = map ((\[from,_,to] -> (from,to)) . words)

toAtoms :: String -> Seq String
toAtoms = fromList . tail . split (keepDelimsL $ whenElt (liftA2 (||) isUpper (=='e')))

allReplacements :: Seq String -> Replacement -> [String]
allReplacements s (from, to) = map (\i -> concat . toList $ update i to s) $ elemIndicesL from s

oneStep :: [Replacement] -> String -> [String]
oneStep reps initialMol = ordNub . concatMap (allReplacements . toAtoms $ initialMol) $ reps

-- O(n log n) rather than nub's O(n²)
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

-- Part 2
part2 = do
    reps <- input
    initialMol <- input2
    let steps = findIndex (isJust . find (==initialMol)) . iterate (concatMap (oneStep reps)) $ ["e"]
    print (fromJust steps)