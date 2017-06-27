import Data.Vect

total
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in (plusSuccRightSucc m k)

total
myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
 where
  reverseProof : Vect ((S n) + len) a -> Vect (plus n (S len)) a
  reverseProof {n} {len} xs = rewrite sym (plusSuccRightSucc n len) in xs
  reverse' : Vect n a -> Vect m a -> Vect (n + m) a
  reverse' {n} acc [] = rewrite plusZeroRightNeutral n in acc
  reverse' {n} acc (x :: xs) = reverseProof (reverse' (x :: acc) xs)
