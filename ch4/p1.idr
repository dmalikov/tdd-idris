data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right)
    = case compare x val of
           LT => Node (insert x left) val right
           EQ => orig
           GT => Node left val (insert x right)

listToTree : Ord a => List a -> Tree a
listToTree = foldl (\l,e => insert e l) Empty

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ [val] ++ (treeToList right)

data Expr : Type where
  Val : Int -> Expr
  Add : Expr -> Expr -> Expr
  Sub : Expr -> Expr -> Expr
  Mult : Expr -> Expr -> Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Sub x y) = (evaluate x) - (evaluate y)
evaluate (Mult x y) = (evaluate x) * (evaluate y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing x = x
maxMaybe x Nothing = x
maxMaybe (Just x) (Just y) = Just (max x y)

data Shape : Type where
  Triangle : Double -> Double -> Shape
  Rectangle : Double -> Double -> Shape
  Circle : Double -> Shape

%name Shape shape, shape1, shape2

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture : Type where
  Primitive : Shape -> Picture
  Combine : Picture -> Picture -> Picture
  Rotate : Double -> Picture -> Picture
  Translate : Double -> Double -> Picture -> Picture

%name Picture pic, pic1, pic2

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive triangle@(Triangle _ _)) = Just (area triangle)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic1 pic2) = maxMaybe (biggestTriangle pic1) (biggestTriangle pic2)
biggestTriangle (Rotate _ pic) = biggestTriangle pic
biggestTriangle (Translate _ _ pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))

