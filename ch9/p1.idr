%default total

data Elem : a -> List a -> Type where
     Here : Elem x (x :: xs)
     There : (later : Elem x xs) -> Elem x (y :: xs)

data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value

lastNotNil : Last [] _ -> Void
lastNotNil LastOne impossible
lastNotNil (LastCons _) impossible

notInSingleton : (notIt : (value = x) -> Void) -> Last [x] value -> Void
notInSingleton notIt LastOne = notIt Refl
notInSingleton _ (LastCons LastOne) impossible
notInSingleton _ (LastCons (LastCons _)) impossible

lastNotInTail : (notThere : Last (x :: xs) value -> Void) -> Last (x1 :: (x :: xs)) value -> Void
lastNotInTail notThere (LastCons prf) = notThere prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No lastNotNil
isLast [x] value = case decEq value x of
                        Yes Refl => Yes LastOne
                        No notIt => No (notInSingleton notIt)
isLast (_ :: x :: xs) value = case isLast (x::xs) value of
                                   Yes prf => Yes (LastCons prf)
                                   No notThere => No (lastNotInTail notThere)
