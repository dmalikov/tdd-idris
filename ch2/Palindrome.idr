module Main

||| 9
palindrome : Nat -> String -> Bool
palindrome n str = let l = toLower str in
  if length str >= n 
    then l == reverse l
    else False

main : IO ()
main = repl "\nEnter a string: " (show . palindrome 10)