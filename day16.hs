{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text (Parser, takeTill, decimal, string,skipWhile, parseOnly)
import Data.Attoparsec.Combinator (many')
import Data.Text (pack, unpack)
import Control.Applicative
import qualified Data.Map.Strict as M

--Part 1
type Trait = (String, Int)
type Sue = [Trait]

input = lines <$> readFile "day16Input.txt"
part1 = filter (potentialSue evidence . snd) . zip [1..] . parse <$> input >>= print

evidence = M.fromList [("children",3),
                       ("cats",7),
                       ("samoyeds",2),
                       ("pomeranians",3),
                       ("akitas",0),
                       ("vizslas",0),
                       ("goldfish",5),
                       ("trees",3),
                       ("cars",2),
                       ("perfumes",1)]

potentialSue :: M.Map String Int -> Sue -> Bool
potentialSue traits = all f
  where f (trait,count) = traits M.! trait == count      

parse :: [String] -> [Sue]
parse = map (success . parseOnly sueP . pack)
  where success (Right a) = a

sueP :: Parser Sue
sueP = p *> many' traitP
  where p = "Sue " *> decimal <* ": "

traitP :: Parser Trait
traitP = (\s i -> (unpack s,i)) <$> takeTill (== ':') <* ": " <*> decimal <* skipWhile (`elem` ", ")

-- Part 2
evidence2 = M.fromList [("children",(==3)),
                        ("cats",(>7)),
                        ("samoyeds",(==2)),
                        ("pomeranians",(<3)),
                        ("akitas",(==0)),
                        ("vizslas",(==0)),
                        ("goldfish",(<5)),
                        ("trees",(>3)),
                        ("cars",(==2)),
                        ("perfumes",(==1))]

potentialSue2 :: M.Map String (Int->Bool) -> Sue -> Bool
potentialSue2 traits = all f
  where f (trait,count) = traits M.! trait $ count  

part2 = filter (potentialSue2 evidence2 . snd) . zip [1..] . parse <$> input >>= print

