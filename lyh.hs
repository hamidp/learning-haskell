--
-- Learn you a haskell.
-- 

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height
	| bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)


calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs =
	[bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  

-- head
head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  

-- maximum with accumulator
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' (x:[]) = x
maximum' (x:xs) =
  maxAcc xs x
  where maxAcc [] a = a
        maxAcc (y:ys) a
            | y >= a = maxAcc ys y
            | otherwise = maxAcc ys a

-- quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort [ a | a <- xs, a <= x]
      bigger  = quicksort [ a | a <- xs, a > x]
  in smaller ++ [x] ++ bigger

-- with partitioning function mostly following
-- http://en.literateprograms.org/Quicksort_(Haskell)
part :: Ord a => a -> [a] -> ([a],[a],[a]) -> ([a],[a],[a])
--               e    lst     l    e   g       partitioned
part _ [] (l,e,g) = (l,e,g)
-- if the head of the list
--   bigger: append go the greater list g
--   smaller: append to the smaller list l
--   equal: append to the equal list e 
part p (x:xs) (l,e,g)
  | x > p     = part p xs (l  , e  , x:g)
  | x < p     = part p xs (x:l, e  , g)
  | otherwise = part p xs (l  , x:e, g)

qs2 :: Ord a => [a] -> [a]
qs2 [] = []
qs2 (x:xs) =
  qs2 smaller ++ equal ++ qs2 bigger
  where
    (smaller, equal, bigger) = part x xs ([],[x],[])
--                                        l   e   g


-- List.map2 from F#
-- http://msdn.microsoft.com/en-us/library/ee340232.aspx
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f _ [] = []
map2 f [] _ = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys

-- with accumulator.
-- todo -- remove the extra reverse
map2Acc :: (a -> b -> c) -> [a] -> [b] -> [c] -> [c]
map2Acc f _ [] acc = reverse acc
map2Acc f [] _ acc = reverse acc
map2Acc f (x:xs) (y:ys) acc = map2Acc f xs ys ((f x y) : acc)

-- maxi fold
maxfold :: Ord a => [a] -> a
maxfold [] = error "empty"
maxfold lst = foldr (\a b -> if a > b then a else b) f lst
              where f = head lst


-----------------------------------------------------------
-- Tree stuff.


data Tree a =
  EmptyTree
  | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)


treeMember :: (Ord a) => a -> Tree a -> Bool
treeMember x EmptyTree = False
treeMember x (Node a left right)
 | x == a = True
 | x < a  = treeMember x left
 | x > a  = treeMember x right


data LTree a =
    LEmptyTree
    | LNode a [LTree a]
    deriving (Show, Read, Eq)

lsingleton :: a -> LTree a
lsingleton x = LNode x []

ltreeInsert :: (Ord a) => a -> LTree a -> LTree a
ltreeInsert x LEmptyTree = lsingleton x
ltreeInsert x (LNode a lst) =
  let newl = (lsingleton x) : lst in
  (LNode a newl)


ltreeDepth :: LTree a -> Integer
ltreeDepth LEmptyTree = 0
ltreeDepth (LNode a lst) = 
  ltreeDepthRec 0 (LNode a lst)
  where
    ltreeDepthRec cur LEmptyTree = cur
    ltreeDepthRec cur (LNode x []) = cur
    ltreeDepthRec cur (LNode x lst1) =
      maximum $ map (ltreeDepthRec $ cur + 1) lst1
  