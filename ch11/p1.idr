import Data.Primitives.Views

%default total

every_other : Stream a -> Stream a
every_other (_ :: y :: xs) = y :: every_other xs

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

data InfList : Type -> Type where
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem

countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x + 1))

getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z _ = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs

Functor InfList where
  map f (x :: xs) = (f x) :: map f xs

data Face = Heads | Tails

getFace : Int -> Face
getFace x with (divides x 2)
  getFace ((2 * _) + rem) | (DivBy _) = if rem == 0
                                           then Heads
                                           else Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count = take count . map getFace

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = approx :: (square_root_approx number next)
 where
  next = (approx + (number / approx)) / 2

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z _ _ (x :: _) = x
square_root_bound (S k) number bound (val :: vals)
    = if abs (val * val - number) < bound
         then val
         else square_root_bound k number bound vals

square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000001 (square_root_approx number number)
