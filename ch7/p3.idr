data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Functor Expr where
  map f (Val x) = Val (f x)
  map f (Add x y) = Add (map f x) (map f y)
  map f (Sub x y) = Sub (map f x) (map f y)
  map f (Mul x y) = Mul (map f x) (map f y)
  map f (Div x y) = Div (map f x) (map f y)
  map f (Abs x) = Abs (map f x)

data Vect : (len : Nat) -> (elem : Type) -> Type where
    Nil : Vect 0 elem
    (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

Eq ty => Eq (Vect n ty) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys
  (==) _ _ = False

Foldable (Vect n) where
  foldr f acc [] = acc
  foldr f acc (x :: xs) = f x (foldr f acc xs)
