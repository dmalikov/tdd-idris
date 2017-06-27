data Vect : (len : Nat) -> (elem : Type) -> Type where
    Nil : Vect 0 elem
    (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

total
headUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
                        (contra : (x = y) -> Void) ->
                        ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

total
tailUnequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
                         (contra : (xs = ys) -> Void) ->
                         ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

total
same_cons : {xs : Vect n a} -> {ys : Vect n a} ->
             xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf

total
same_vects : {xs : Vect n a} -> {ys : Vect n a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
same_vects Refl prf1 = same_cons prf1

total
DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys)
    = case decEq x y of
           Yes prf => case decEq xs ys of
                           Yes prf1 => Yes (same_vects prf prf1)
                           No contra => No (tailUnequal contra)
           No contra => No (headUnequal contra)
