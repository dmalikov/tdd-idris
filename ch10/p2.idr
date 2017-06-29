import Data.Nat.Views
import Data.List.Views
import Data.Vect
import Data.Vect.Views

%default total

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix [] _ = []
equalSuffix _ [] = []
equalSuffix input1 input2 with (snocList input1)
  equalSuffix [] _ | Empty = []
  equalSuffix (xs ++ [x]) input2 | (Snoc xs') with (snocList input2)
    equalSuffix (_ ++ [_]) [] | (Snoc _) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xs') | (Snoc ys') = if x == y then (equalSuffix xs ys | xs' | ys') ++ [x]
                                                                            else []

mergeSort : Ord a => Vect n a -> Vect n a
mergeSort xs with (splitRec xs)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (xs ++ ys) | (SplitRecPair lrec rrec) = merge (mergeSort xs | lrec) (mergeSort ys | rrec)

toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary Z | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven rec) = toBinary n | rec ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = toBinary n | rec ++ "1"

palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (ys ++ [y])) | (VCons rec) = if x == y then palindrome ys | rec else False
