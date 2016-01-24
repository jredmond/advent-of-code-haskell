{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Bits ((.&.),(.|.),complement,shiftL,shiftR)
import Data.Attoparsec.Text (Parser, decimal, parseOnly, takeWhile1)
import Data.Text (Text, pack)
import Data.Word (Word16)
import Data.Char (isLower)
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict (State, get, put, evalState)

type Id = Text
data Wire = Wire Id Signal deriving (Show)
data Signal = And Signal Signal | Or Signal Signal | Not Signal |
              LShift Signal Int | RShift Signal Int |
              Value Word16 | WireInput Id deriving (Show)

part1 :: IO Word16
part1 = do
  str <- readFile "day7Input.txt"
  let signals = parseAll str
  return $ evalWire signals "a"

part2 :: IO Word16
part2 = do
  str <- readFile "day7Input.txt"
  let signals = M.insert "b" (Value 3176) $ parseAll str
  return $ evalWire signals "a"

-- Evaluate the signal corresponding to Wire Id
evalWire :: M.Map Id Signal -> Id -> Word16
evalWire signals i = evalState (evalSignal signals sig) M.empty
  where sig = signals M.! i

-- Recursively process signals to find the final wire value, using the 
-- state monad to keep track of calculated wires
evalSignal :: M.Map Id Signal -> Signal -> State (M.Map Id Word16) Word16
evalSignal signals s =
  case s of
    And s1 s2   -> (.&.) <$> eval s1 <*> eval s2
    Or s1 s2    -> (.|.) <$> eval s1 <*> eval s2
    Not s       -> complement <$> eval s
    LShift s n  -> flip shiftL n <$> eval s
    RShift s n  -> flip shiftR n <$> eval s
    Value n     -> return n
    WireInput i -> do
      values <- get
      case M.lookup i values of
        Just n  -> return n
        Nothing -> do
          v <- eval $ signals M.! i
          values' <- get
          put $ M.insert i v values'
          return v
  where eval = evalSignal signals

-- Construct the parser (attoparsec)
parseAll :: String -> M.Map Id Signal
parseAll = M.fromList . map (tuple . parse . pack) . lines
  where tuple (Wire i s) = (i, s)

parse :: Text -> Wire
parse = success . parseOnly wireP
  where success (Right a) = a

wireP :: Parser Wire
wireP = flip Wire <$> signalP <* " -> " <*> idP

-- value and wire input are checked last as they are subsets of other parsers
signalP :: Parser Signal
signalP = andP <|> orP <|> notP <|> lShiftP <|> rShiftP <|> valueP <|> wireInputP

andP :: Parser Signal
andP = And <$> gateInputP <* " AND " <*> gateInputP

orP :: Parser Signal
orP = Or <$> gateInputP <* " OR " <*> gateInputP

notP :: Parser Signal
notP = Not <$> ("NOT " *> gateInputP)

lShiftP :: Parser Signal
lShiftP = LShift <$> gateInputP <* " LSHIFT " <*> decimal

rShiftP :: Parser Signal
rShiftP = RShift <$> gateInputP <* " RSHIFT " <*> decimal

valueP :: Parser Signal
valueP = Value <$> decimal

idP :: Parser Id
idP = takeWhile1 isLower

wireInputP :: Parser Signal
wireInputP = WireInput <$> idP

gateInputP :: Parser Signal
gateInputP = wireInputP <|> valueP

