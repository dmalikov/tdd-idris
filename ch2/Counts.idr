module Main

counts : String -> (Nat, Nat)
counts str = ((List.length . split (== ' ')) str, length str)

main : IO ()
main = repl "\nEnter a string: " (show . counts)