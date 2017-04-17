
import Data.Vect

addMatrix : Num numType =>
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: (addMatrix xs ys)

createEmpties : Vect n (Vect 0 elem)
createEmpties {n = Z} = []
createEmpties {n = S k} = [] :: createEmpties

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = zipWith (::) x (transposeMat xs)

calcCell : Num numType => (x : Vect m numType) -> (y : Vect m numType) -> numType
calcCell x y = sum (zipWith (*) x y)

calcRow : Num numType => (x : Vect m numType) -> (ys : Vect p (Vect m numType)) -> Vect p numType
calcRow _ [] = []
calcRow x (y :: ys) = calcCell x y :: calcRow x ys

multMatrix' : Num numType => (xs : Vect n (Vect m numType)) -> Vect p (Vect m numType) -> Vect n (Vect p numType)
multMatrix' [] _ = []
multMatrix' (x :: xs) ys = calcRow x ys :: multMatrix' xs ys

multMatrix : Num numType => Vect n (Vect m numType) -> Vect m (Vect p numType) -> Vect n (Vect p numType)
multMatrix xs ys = multMatrix' xs (transposeMat ys)
