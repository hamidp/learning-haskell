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


