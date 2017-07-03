import Control.Monad.State

data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Acice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))

flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

update : (stateType -> stateType) -> State stateType ()
update f = do c <- get
              put (f c)

increase : Nat -> State Nat ()
increase x = update (+x)

countEmpty : Tree a -> State Nat ()
countEmpty Empty = do c <- get
                      put (S c)
countEmpty (Node l _ r) = do countEmpty l
                             countEmpty r

countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do (e, n) <- get
                          put (S e, n)
countEmptyNode (Node l _ r) = do countEmptyNode l
                                 countEmptyNode r
                                 (e, n) <- get
                                 put (e, S n)
