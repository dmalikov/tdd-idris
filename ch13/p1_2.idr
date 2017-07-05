%default total

namespace Guess

  data GuessCmd : Type -> Nat -> Nat -> Type where
    Try : Integer -> GuessCmd Ordering (S guesses) guesses
    Pure : ty -> GuessCmd ty state state
    (>>=) : GuessCmd a state1 state2 -> (a -> GuessCmd b state2 state3) -> GuessCmd b state1 state3

  threeGuesses : GuessCmd () 3 0
  threeGuesses = do Try 10
                    Try 20
                    Try 15
                    Pure ()

  -- notTypeChecked : GuessCmd () 0 0
  -- notTypeChecked = do Try 10
  --                Pure ()

namespace Matter

  data Matter = Solid | Liquid | Gas

  data MatterCmd : Type -> Matter -> Matter -> Type where
    (>>=) : MatterCmd a matter1 matter2 -> (a -> MatterCmd b matter2 matter3) -> MatterCmd b matter1 matter3
    Melt : MatterCmd () Solid Liquid
    Boil : MatterCmd () Liquid Gas
    Condense : MatterCmd () Gas Liquid
    Freeze : MatterCmd () Liquid Solid

  iceSteam : MatterCmd () Solid Gas
  iceSteam = do Melt
                Boil

  steamIce : MatterCmd () Gas Solid
  steamIce = do Condense
                Freeze

  -- overMelt : MatterCmd () Solid Gas
  -- overMelt = do Melt
  --               Melt
