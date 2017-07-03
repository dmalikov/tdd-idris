import Data.Primitives.Views
import System

%default total

data Command : Type -> Type where
     PutStr : String -> Command ()
     GetLine : Command String
     ReadFile : String -> Command (Either FileError String)
     WriteFile : String -> String -> Command (Either FileError ())
     Pure : ty -> Command ty
     Bind : Command a -> (a -> Command b) -> Command b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

data ConsoleIO : Type -> Type where
     Quit : a -> ConsoleIO a
     Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

data Input = Answer Int
           | QuitCmd

namespace ConsoleDo
  (>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile f) = readFile f
runCommand (WriteFile f c) = writeFile f c
runCommand (Pure val) = pure val
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run fuel (Quit val) = pure (Just val)
run (More fuel) (Do c f) = do res <- runCommand c
                              run fuel (f res)
run Dry _ = pure Nothing

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed)
  where
    bound : Int -> Int
    bound x with (divides x 12)
      bound ((12 * div) + rem) | (DivBy prf) = abs rem + 1

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure (Answer (cast answer))

mutual
  correct : Stream Int -> (attempts : Nat) -> (score : Nat) -> ConsoleIO Nat
  correct nums attempts score = do PutStr "Correct!\n"
                                   quiz nums (attempts + 1) (score + 1)

  wrong : Stream Int -> Int -> (attempts : Nat) -> (score : Nat) -> ConsoleIO Nat
  wrong nums ans attempts score = do PutStr ("Wrong, the answer is " ++ show ans ++ "\n")
                                     quiz nums (attempts + 1) score

  quiz : Stream Int -> (attempts : Nat) -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: num2 :: nums) attempts score
    = do PutStr ("Score so far: " ++ show score ++ " / " ++ show attempts ++ "\n")
         input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
         case input of
              Answer answer => if answer == num1 * num2
                                  then correct nums attempts score
                                  else wrong nums (num1 * num2) attempts score
              QuitCmd => Quit score

data ShellInput
  = Cat String
  | Copy String String
  | Exit

readShellInput : String -> Command (Maybe ShellInput)
readShellInput prompt = do PutStr prompt
                           input <- GetLine
                           case map toLower (words input) of
                                ["exit"] => Pure (Just Exit)
                                ["cat", filename] => Pure (Just (Cat filename))
                                ["copy", src, dst] => Pure (Just (Copy src dst))
                                _ => Pure Nothing

partial
shell : ConsoleIO ()
shell = do Just shellInput <- readShellInput ">> "
                | Nothing => do PutStr "Invalid command\n"
                                shell
           case shellInput of
                Cat filename => do Right content <- ReadFile filename
                                        | Left error => do PutStr (show error ++ "\n")
                                                           shell
                                   PutStr content
                                   shell
                Copy src dst => do Right content <- ReadFile src
                                        | Left error => do PutStr (show error ++ "\n")
                                                           shell
                                   WriteFile dst content
                                   shell
                Exit => Quit ()

partial
shellIO : IO ()
shellIO = do Just _ <- run forever shell
                | Nothing => putStr "Ran out of fuel"
             pure ()

partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0 0)
              | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score)
