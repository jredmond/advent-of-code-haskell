{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

import Control.Monad (unless)
import Control.Monad.State.Strict (State, get, put, execState)
import Control.Lens (makeLenses, (-=), (+=), (%=), (.=), use, view, _1, _2, _3, (^.), (-~))
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Pure data type for representing the game state
data WizSim = WizSim
  { _playerHP    :: Int
  , _playerArmor :: Int
  , _bossHP      :: Int
  , _bossDamage  :: Int
  , _mana        :: Int
  , _manaSpent   :: Int
  , _buffs       :: [Buff]
  , _spellHist   :: [String]
  } deriving (Show)

-- (name, cooldown, effect)
type Buff = (String, Int, State WizSim ())

instance Show (State WizSim ()) where
    show _ = "<< Some Buff >>"

-- (name, manaCost, spell)
type Spell = (String, Int, State WizSim ())

-- Create the lenses
makeLenses ''WizSim

spells :: [Spell]
spells = [
  ("magicMissle", 53, bossHP -= 4),
  ("drain", 73, bossHP -= 2 >> playerHP += 2),
  ("shield", 113, addEffect ("shield", 6, playerArmor += 7)),
  ("poison", 173, addEffect ("poison", 6, bossHP -= 3)),
  ("recharge", 229, addEffect ("recharge", 5, mana += 101))
  ]

spellName :: Spell -> String
spellName (n,_,_) = n

addEffect :: Buff -> State WizSim ()
addEffect newBuff = buffs %= (:) newBuff

startingState :: WizSim
startingState = WizSim 50 0 71 10 500 0 [] []

applyEffects :: State WizSim ()
applyEffects = do 
  playerArmor .= 0
  bs <- use buffs
  -- Apply active effects
  mapM_ (view _3) bs
  -- Update cooldowns and remove finished
  buffs %= filter ((/=0) . view _2) . map (_2 -~ 1)

-- Cannot cast spells that are already active
availableSpells :: [Buff] -> [Spell] -> [Spell]
availableSpells bs = filter ((`notElem` activeBuffs) . view _1)
  where activeBuffs = map (view _1) bs

bossAttacks :: State WizSim ()
bossAttacks = do
  wiz <- get
  playerHP -= max 1 (wiz^.bossDamage - wiz^.playerArmor)

castSpell :: Spell -> State WizSim ()
castSpell spell = do
  view _3 spell
  let manaCost = view _2 spell
  mana -= manaCost
  manaSpent += manaCost
  spellHist %= (:) (spellName spell)

runTurns :: State WizSim ()
runTurns = do
    --playerHP -= 1
    applyEffects
    curState <- get
    let options = availableSpells (curState^.buffs) spells
    let states = map (flip execState curState . runRestOfTurn) options
    put . minimumBy (comparing (view manaSpent)) . (:) dummyState . filter isWin $ states
  where
    runRestOfTurn spell = do
      --trace (show spell) (castSpell spell)
      castSpell spell
      applyEffects
      bossAttacks
      curState <- get
      --trace (show curState) (unless (fightIsOver curState) runTurns)
      unless (fightIsOver curState) runTurns

fightIsOver :: WizSim -> Bool
fightIsOver wiz = wiz^.mana < 0 || wiz^.playerHP <= 0 || wiz^. bossHP <=0

dummyState :: WizSim
dummyState = WizSim 50 0 71 10 500 99999 [] []

isWin :: WizSim -> Bool
isWin wiz = (wiz^.mana) >= 0 && (wiz^.bossHP) <=0

part1 = view manaSpent $ execState runTurns startingState

-- view the winning cast spells
winningSpells = view spellHist $ execState runTurns startingState

--winning
--["magicMissle"
--,"poison"
--,"recharge"
--,"shield"
--,"poison"
--,"recharge"
--,"shield"
--,"poison"
--,"recharge"
--,"shield"
--,"poison"
--,"magicMissle"
--]