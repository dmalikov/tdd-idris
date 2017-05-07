import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
  putStr ("Guess number [" ++ show guesses ++ "] : ")
  Just guessNum <- readNumber
    | Nothing => do putStrLn "Invalid output"
                    guess target guesses
  case compare guessNum target of
       GT => do putStrLn "Too high"
                guess target (guesses + 1)
       LT => do putStrLn "Too low"
                guess target (guesses + 1)
       EQ => putStrLn "Correct!"


replWith_ : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
replWith_ state prompt onInput = do
  putStr prompt
  input <- getLine
  case onInput state input of
       Just (output, state') => do putStr output
                                   replWith_ state' prompt onInput
       Nothing => pure ()

repl_ : (prompt : String) -> (onInput : String -> String) -> IO ()
repl_ prompt onInput = replWith_ () prompt (\_,s => Just (onInput s, ()))

main : IO ()
main = do
  num <- map fromInteger time
  guess (mod num 100) 1
