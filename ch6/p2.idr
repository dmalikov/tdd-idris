import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Int)

testMatrix : Matrix 2 3
testMatrix = [[0,0,0], [0,0,0]]

data Format = Number Format
            | Dbl Format
            | Str Format
            | Chr Format
            | Lit String Format
            | End

total
PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Dbl fmt) = (d : Double) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Chr fmt) = (c : Char) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

total
printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Dbl fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Chr fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

TupleVect : Nat -> Type -> Type
TupleVect Z x = ()
TupleVect (S k) x = (x, TupleVect k x)

test : TupleVect 4 Nat
test = (1,2,3,4,())
