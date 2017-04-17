module Main

import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = reverse xs ++ [x]

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs

main : IO ()
main = do
  printLn $ my_length [1..10]
  printLn $ my_reverse [1..10]
  printLn $ my_map (* 2) [1..10]
  printLn $ my_vect_map length ["Hot", "Dog", "Jumping", "Frog"]
