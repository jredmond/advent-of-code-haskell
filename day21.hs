import Data.List.Split (splitOn)
import Data.List (group, sort)

data Character = Character {hitpoints::Int, damage::Int, armor::Int} deriving (Show)
type Item = (Int,Int,Int)

main :: IO ()
main = do
    input <- readFile "day21Input.txt"
    print . head . filter (playerWins . startingStats) . itemCombinations $ parseItems input
    print . head . filter (not . playerWins . startingStats) . reverse . itemCombinations $ parseItems input

playerAttacks :: (Character,Character) -> (Character,Character)
playerAttacks (atk,def) = (atk,def {hitpoints = hitpoints def - max 1 (damage atk - armor def)})

bossAttacks :: (Character,Character) -> (Character,Character)
bossAttacks (def,atk) = (def {hitpoints = hitpoints def - max 1 (damage atk - armor def)},atk)

startingStats :: Item -> (Character,Character)
startingStats (_,dam,arm) = (Character 100 dam arm,Character 104 8 1)

battle :: (Character,Character) -> [(Character,Character)]
battle chars = scanl (flip ($)) chars (cycle [playerAttacks,bossAttacks])

playerWins :: (Character,Character) -> Bool
playerWins chars = (\(p,_) -> hitpoints p > 0) . head . filter f $ battle chars
  where f (p,b) = hitpoints p <= 0 || hitpoints b <= 0

itemCombinations :: ([Item],[Item],[Item]) -> [Item]
itemCombinations (weaps,armors,rings) = map head . group $ sort [w `plus` a `plus` r1 `plus` r2 
                                                                | w <- weaps
                                                                , a <- (0,0,0):armors
                                                                , r1 <- (0,0,0):rings
                                                                , r2 <- filter (/= r1) ((0,0,0):rings)]
  where plus (a1,b1,c1) (a2,b2,c2) = (a1+a2,b1+b2,c1+c2)

parseItems :: String -> ([Item],[Item],[Item])
parseItems input = let [_:weaps, _:armors, _:rings] = splitOn [""] $ lines input
                   in (map parseItem weaps, map parseItem armors, map parseRing rings)

parseItem :: String -> Item
parseItem str = let [_,cost,attack,defence] = words str
                in (read cost, read attack, read defence)

parseRing :: String -> Item
parseRing str = let [_,_,cost,attack,defence] = words str
                in (read cost, read attack, read defence)