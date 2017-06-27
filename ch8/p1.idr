total
same_cons : {xs : List a} -> {ys : List a} ->
             xs = ys -> x :: xs = x :: ys
same_cons prf = cong prf

total
same_lists : {xs : List a} -> {ys : List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl prf1 = same_cons prf1

data ThreeEq : (a : Nat) -> (b : Nat) -> (c : Nat) -> Type where
     ReflThree : (x : Nat) -> ThreeEq x x x

allSames : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSames x x x (ReflThree x) = ReflThree (S x)
