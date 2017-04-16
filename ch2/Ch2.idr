module Ch2

palindrome2 : String -> Bool
palindrome2 str = str == reverse str

palindrome3 : String -> Bool
palindrome3 str = let l = toLower str in
  l == reverse l

palindrome4 : String -> Bool
palindrome4 str = let l = toLower str in
  if length str >= 10 
    then l == reverse l
    else False

palindrome5 : Nat -> String -> Bool
palindrome5 n str = let l = toLower str in
  if length str >= n 
    then l == reverse l
    else False

counts : String -> (Nat, Nat)
counts str = ((List.length . split (== ' ')) str, length str)

top_ten : Ord a => List a -> List a
top_ten = List.take 10 . reverse . sort

over_length : Nat -> List String -> Nat
over_length n = length . filter ((> n) . length)