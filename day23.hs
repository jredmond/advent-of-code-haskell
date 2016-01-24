import Control.Applicative

type State = (Int, Int, Int)
type Instruction = State -> State

input = map parseInstruction . lines <$> readFile "day23Input.txt"
part1 = run (0,0,0) <$> input >>= print
part2 = run (0,1,0) <$> input >>= print

parseInstruction :: String -> Instruction
parseInstruction = f . words . stripChars "+,"
  where
    f ["hlf",r] = modifyPC (+1) . modifyRegister r (`div` 2)
    f ["tpl",r] = modifyPC (+1) . modifyRegister r (* 3)
    f ["inc",r] = modifyPC (+1) . modifyRegister r (+ 1)
    f ["jmp",offset] = modifyPC (+ read offset)
    f ["jie",r,offset] = \s -> if even (getRegister r s) then modifyPC (+ read offset) s else modifyPC (+ 1) s
    f ["jio",r,offset] = \s -> if  getRegister r s == 1 then modifyPC (+ read offset) s else modifyPC (+ 1) s

getRegister r (_,a,b) = if r == "a" then a else b
getPC (pc,_,_) = pc
modifyRegister r f (pc,a,b) = if r == "a" then (pc, f a, b) else (pc, a, f b)
modifyPC f (pc,a,b) = (f pc, a, b)

stripChars :: String -> String -> String
stripChars = filter . flip notElem

run :: State -> [Instruction] -> State
run s fs | getPC s >= length fs = s
run s fs = run (fs !! getPC s $ s) fs