{-# LANGUAGE OverloadedStrings #-}

-- Unboxed as this is more efficient when using simple atomic types
-- such as bool (part of the unbox class)
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Prelude hiding (lines)
import Control.Applicative
import Data.Attoparsec.Text (Parser, decimal, parseOnly)
import Data.Text (Text, pack, lines)

type Action = Bool -> Bool
type Light = (Int, Int)
data Instruction = Instruction {action :: Action, start :: Light, end :: Light}
type Display = M.IOVector Bool


part1 :: IO Int
part1 = do 
  s <- readFile "day6Input.txt"
  let is = map unsafeParse . lines . pack $ s
  d <- initialDisplay
  execute d is
  d' <- V.freeze d
  return $ countLights d'

execute :: Display -> [Instruction] -> IO ()
execute d = mapM_ executeI
  where executeI (Instruction f (x1,y1) (x2,y2)) = sequence_ [M.modify d f (x+y*1000) | x <- [x1..x2], y <- [y1..y2]]

initialDisplay :: IO Display
initialDisplay = M.replicate 1000000 False

countLights :: V.Vector Bool -> Int
countLights = V.length . V.filter id

-- Parser
unsafeParse :: Text -> Instruction
unsafeParse = success . parseOnly instructionP
  where success (Right a) = a
        success _         = error "Failed to parse instruction"

instructionP :: Parser Instruction
instructionP = Instruction <$> actionP <*> (" " *> lightP <* " through ") <*> lightP

actionP :: Parser Action
actionP = 
      match "turn on"  (const True)
  <|> match "turn off" (const False)
  <|> match "toggle"   not
    where match s a = s >> pure a

lightP :: Parser Light
lightP = (,) <$> (decimal <* ",") <*> decimal

-- Part 2:
type Action2 = Int -> Int
data Instruction2 = Instruction2 {action2 :: Action2, start2 :: Light, end2 :: Light}
type Display2 = M.IOVector Int

part2 :: IO Int
part2 = do 
  s <- readFile "day6Input.txt"
  let is = map unsafeParse2 . lines . pack $ s
  d <- initialDisplay2
  execute2 d is
  d' <- V.freeze d
  return $ sumBrightness d'

execute2 :: Display2 -> [Instruction2] -> IO ()
execute2 d = mapM_ executeI
  where executeI (Instruction2 f (x1,y1) (x2,y2)) = sequence_ [M.modify d f (x+y*1000) | x <- [x1..x2], y <- [y1..y2]]

initialDisplay2 :: IO Display2
initialDisplay2 = M.replicate 1000000 0

sumBrightness :: V.Vector Int -> Int
sumBrightness = V.sum

-- Parser 2 (After correct mistranslations from Ancient Nordic Elvish)
unsafeParse2 :: Text -> Instruction2
unsafeParse2 = success . parseOnly instructionP2
  where success (Right a) = a
        success _         = error "Failed to parse instruction"

instructionP2 :: Parser Instruction2
instructionP2 = Instruction2 <$> actionP2 <*> (" " *> lightP <* " through ") <*> lightP

actionP2 :: Parser Action2
actionP2 = 
      match "turn on"  (+ 1)
  <|> match "turn off" (\x -> max 0 (x-1))
  <|> match "toggle"   (+ 2)
    where match s a = s >> pure a
